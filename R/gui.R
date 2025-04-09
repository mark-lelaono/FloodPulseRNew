#' Start FloodPulseR GUI
#' @export
start_floodpulse <- function() {

  # Clear FloodPulseR-specific temporary files
  fp_temp_dir <- file.path(tempdir(), "floodpulse_temp")
  if (dir.exists(fp_temp_dir)) {
    message("Clearing old FloodPulseR temporary files...")
    unlink(fp_temp_dir, recursive = TRUE, force = TRUE)
  }

  # Check required packages
  required_packages <- c("shiny", "sf", "terra", "leaflet", "leaflet.extras", "rgee")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop("The following required packages are not installed: ",
         paste(missing_packages, collapse = ", "),
         ". Please install them using install.packages() and try again.")
  }

  # Load packages
  lapply(required_packages, library, character.only = TRUE)

  # Verify that FloodPulseR is loaded correctly
  if (!"FloodPulseR" %in% (.packages())) {
    stop("Failed to load the FloodPulseR package. Please reinstall the package using devtools::install().")
  }

  # Initialize Google Earth Engine with Service Account Key (SaK)
  sak_path <- "C:/Users/ADMIN/Documents/fifth-catcher-456205-e8-21eef9f80731.json"
  gee_initialized <- FALSE

  # Check if SaK file exists
  if (!file.exists(sak_path)) {
    stop("Service Account Key (SaK) file not found at: ", sak_path,
         "\nPlease ensure the SaK file is placed in the specified directory.")
  }

  # Extract email from the SaK file
  sak_json <- jsonlite::fromJSON(sak_path)
  email <- sak_json$client_email
  if (is.null(email) || email == "") {
    stop("Invalid SaK file: client_email not found.")
  }

  # Copy the SaK to rgee's configuration directory
  tryCatch({
    message("Copying SaK for rgee authentication...")
    ee_utils_sak_copy(sakfile = sak_path)
    message("SaK copied successfully.")
  }, error = function(e) {
    stop("Failed to copy SaK for GEE authentication: ", e$message)
  })

  # Initialize GEE using the email from SaK
  tryCatch({
    message("Initializing GEE using Service Account email: ", email)
    ee_Initialize(email = email, drive = TRUE, gcs = TRUE)
    gee_initialized <- TRUE
    message("Google Earth Engine initialized successfully using Service Account.")
  }, error = function(e) {
    stop("Failed to initialize Google Earth Engine with Service Account Key: ", e$message,
         "\nEnsure the SaK is valid and permissions are correct.")
  })

  # Define UI
  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
        body {
          font-family: Arial, sans-serif;
          background-color: #f5f5f5;
        }
        .sidebar { background-color: #ffffff; border-right: 1px solid #ddd; padding: 15px; }
        .main-panel { padding: 20px; }
        h4 { color: #333; margin-bottom: 10px; }
        .leaflet-container { border: 1px solid #ddd; border-radius: 5px; }
        .well { background-color: #f9f9f9; border: 1px solid #ddd; border-radius: 5px; }
        .btn { background-color: #007bff; color: white; border: none; border-radius: 5px; }
        .btn:hover { background-color: #0056b3; }
      "))
    ),
    titlePanel("FloodPulseR: Sentinel-1 Flood Tool"),
    sidebarLayout(
      sidebarPanel(
        class = "sidebar",
        width = 3,
        h4("Inputs"),
        fileInput("aoi_zip", "Upload Shapefile as ZIP (include .shp, .shx, .dbf, .prj)",
                  accept = c(".zip"),
                  multiple = FALSE),
        dateInput("start_date", "Start Date", value = "2024-01-01", format = "yyyy-mm-dd"),
        dateInput("end_date", "End Date", value = "2024-01-02", format = "yyyy-mm-dd"),
        checkboxInput("use_drawn_area", "Use manually drawn area instead of shapefile", FALSE),
        conditionalPanel(
          condition = "input.use_drawn_area == true",
          actionButton("clear_draw", "Clear Drawing")
        ),
        actionButton("fetch", "Fetch and Process")
      ),
      mainPanel(
        class = "main-panel",
        h4("Area of Interest"),
        leafletOutput("map", height = "500px", width = "100%"),
        h4("Status"),
        verbatimTextOutput("status"),
        h4("Debug Info"),
        verbatimTextOutput("debug_info"),
        h4("GEE Status"),
        verbatimTextOutput("gee_status")
      )
    )
  )

  # Define Server
  server <- function(input, output, session) {
    # Initialize reactive values
    rv <- reactiveValues(
      drawn_shapes = NULL,
      aoi = NULL,
      shapefile_error = NULL
    )

    # Initialize map with Eastern Africa view
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
        addLayersControl(
          baseGroups = c("Satellite", "OpenStreetMap"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addDrawToolbar(
          targetGroup = "drawn_aoi",
          polylineOptions = FALSE,
          markerOptions = FALSE,
          circleOptions = FALSE,
          circleMarkerOptions = FALSE,
          editOptions = editToolbarOptions()
        ) %>%
        addLayersControl(
          overlayGroups = c("drawn_aoi", "uploaded_aoi"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        setView(lng = 38.0, lat = 3.0, zoom = 5)
    })

    # Output for GEE status
    output$gee_status <- renderPrint({
      if (gee_initialized) {
        cat("GEE initialized successfully with Service Account Key.")
      } else {
        cat("GEE initialization failed. Check debug info.")
      }
    })

    # Output for debugging information
    output$debug_info <- renderPrint({
      if (!is.null(input$aoi_zip)) {
        cat("Uploaded ZIP: ", input$aoi_zip$name, "\n")
      } else {
        cat("No ZIP uploaded yet\n")
      }
      if (!is.null(rv$drawn_shapes)) {
        cat("Drawn shapes available: Yes\n")
      }
      if (!is.null(rv$shapefile_error)) {
        cat("Shapefile error: ", rv$shapefile_error, "\n")
      }
    })

    # Reactive value to store AOI from shapefile
    aoi_data <- reactive({
      req(input$aoi_zip)
      req(!input$use_drawn_area)
      zip_file <- input$aoi_zip$datapath
      zip_name <- input$aoi_zip$name

      # Create a unique temporary directory for extraction using R's default tempdir()
      temp_dir <- file.path(tempdir(), "floodpulse_temp", paste0("shapefile_", format(Sys.time(), "%Y%m%d%H%M%S")))
      message("Creating temporary directory: ", temp_dir)
      dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

      # Check if the directory was created and is writable
      if (!dir.exists(temp_dir)) {
        stop("Failed to create temporary directory: ", temp_dir)
      }
      if (file.access(temp_dir, mode = 2) != 0) {
        stop("Temporary directory is not writable: ", temp_dir,
             "\nPlease ensure you have write permissions to this directory.")
      }

      # Extract the ZIP file
      message("Extracting ZIP file to: ", temp_dir)
      tryCatch({
        unzip(zip_file, exdir = temp_dir)
        message("ZIP file extracted successfully.")
      }, error = function(e) {
        rv$shapefile_error <- paste("Error extracting ZIP file:", e$message)
        return(NULL)
      })

      # Find the .shp file in the extracted contents
      extracted_files <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)
      shp_path <- extracted_files[grepl("\\.shp$", extracted_files, ignore.case = TRUE)][1]
      if (is.na(shp_path)) {
        rv$shapefile_error <- "No .shp file found in the uploaded ZIP."
        return(NULL)
      }

      # Check for accompanying files
      shp_dir <- dirname(shp_path)
      shp_base <- tools::file_path_sans_ext(basename(shp_path))
      has_shx <- file.exists(file.path(shp_dir, paste0(shp_base, ".shx")))
      has_dbf <- file.exists(file.path(shp_dir, paste0(shp_base, ".dbf")))
      has_prj <- file.exists(file.path(shp_dir, paste0(shp_base, ".prj")))  # Optional

      if (!has_shx || !has_dbf) {
        rv$shapefile_error <- "Missing required .shx or .dbf files in the ZIP."
        return(NULL)
      }

      # Read the shapefile
      tryCatch({
        message("Attempting to read shapefile: ", shp_path)
        sf_object <- st_read(shp_path, quiet = TRUE)
        if (is.na(st_crs(sf_object)) || is.null(st_crs(sf_object))) {
          message("No valid CRS found. Assuming WGS84 (EPSG:4326).")
          st_crs(sf_object) <- 4326
        }
        if (!identical(st_crs(sf_object)$epsg, 4326)) {
          sf_object <- st_transform(sf_object, 4326)
        }
        rv$shapefile_error <- NULL
        return(sf_object)
      }, error = function(e) {
        message("Error reading shapefile: ", e$message)
        rv$shapefile_error <- paste("Error reading shapefile:", e$message)
        return(NULL)
      })
    })

    # Observer for clearing drawn shapes
    observeEvent(input$clear_draw, {
      leafletProxy("map") %>%
        clearGroup("drawn_aoi")
      rv$drawn_shapes <- NULL
    })

    # Observer for drawn shapes
    observeEvent(input$map_draw_new_feature, {
      feature <- input$map_draw_new_feature
      drawn_sf <- tryCatch({
        if (feature$geometry$type == "Polygon" || feature$geometry$type == "Rectangle") {
          coords <- feature$geometry$coordinates[[1]]
          coords_matrix <- do.call(rbind, coords)
          polygon <- st_polygon(list(coords_matrix))
          sf_obj <- st_sf(geometry = st_sfc(polygon, crs = 4326))
          sf_obj
        } else {
          NULL
        }
      }, error = function(e) {
        message("Error converting drawn shape to SF: ", e$message)
        NULL
      })
      if (!is.null(drawn_sf)) {
        rv$drawn_shapes <- drawn_sf
      }
    })

    # Observer for deleted shapes
    observeEvent(input$map_draw_deleted_features, {
      deleted_ids <- sapply(input$map_draw_deleted_features$features, function(x) x$id)
      if (length(deleted_ids) > 0) {
        rv$drawn_shapes <- NULL
      }
    })

    observe({
      if (input$use_drawn_area) return()  # Only process if a shapefile is used and not the manually drawn area
      aoi <- aoi_data()
      if (is.null(aoi)) {
        output$status <- renderPrint({
          cat("Failed to load shapefile. Check debug info for details.")
        })
        return()
      }
      rv$aoi <- aoi

      # Calculate the bounding box of the shapefile
      bbox <- st_bbox(aoi)

      # Update map with Satellite view and zoom to AOI
      leafletProxy("map") %>%
        clearGroup("uploaded_aoi") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%  # Ensure satellite layer is added
        addPolygons(
          data = aoi,
          fillOpacity = 0,
          weight = 1,
          opacity = 1,
          color = "black",
          group = "uploaded_aoi"
        ) %>%
        fitBounds(
          bbox[["xmin"]],
          bbox[["ymin"]],
          bbox[["xmax"]],
          bbox[["ymax"]]
        )
    })



    # Get the final AOI to use
    get_active_aoi <- reactive({
      if (input$use_drawn_area && !is.null(rv$drawn_shapes)) {
        return(rv$drawn_shapes)
      } else if (!input$use_drawn_area && !is.null(rv$aoi)) {
        return(rv$aoi)
      } else {
        return(NULL)
      }
    })

    # Fetch and process Sentinel-1 image when user clicks "Fetch and Process"
    observeEvent(input$fetch, {
      tryCatch({
        # Get the active AOI (from shapefile or drawn shape)
        aoi <- get_active_aoi()
        req(aoi)

        # Convert AOI to an Earth Engine object
        aoi_ee <- sf_as_ee(aoi)

        # Fetch Sentinel-1 image collection
        s1_img <- ee$ImageCollection("COPERNICUS/S1_GRD")$
          filterBounds(aoi_ee)$
          filterDate(input$start_date, input$end_date)$
          filter(ee$Filter$listContains("transmitterReceiverPolarisation", "VV"))$
          filter(ee$Filter$eq("orbitProperties_pass", "DESCENDING"))
        # Check if images are available
        img_count <- s1_img$size()$getInfo()
        if (img_count == 0) {
          stop("No Sentinel-1 images available for the selected date range and AOI.")
        }
        s1_img <- s1_img$median()$select("VV")

        # Get map tile ID from Earth Engine
        map_id <- s1_img$getMapId(ee$vizParams(
          min = -25,
          max = 0,
          palette = c("black", "white")
        ))

        # Render the map with the Sentinel-1 image
        output$map <- renderLeaflet({
          leaflet() %>%
            addProviderTiles(providers$Esri.WorldImagery) %>%
            addTiles(
              urlTemplate = map_id$url,
              options = tileOptions(opacity = 0.7)
            ) %>%
            setView(lng = 38.0, lat = 3.0, zoom = 6)
        })

        # Update status message
        output$status <- renderPrint({
          cat("Fetched Sentinel-1 imagery from GEE for AOI and date range.")
        })
      }, error = function(e) {
        # Handle any errors
        output$status <- renderPrint({
          cat("Error during fetching and processing Sentinel-1 data:", e$message)
        })
      })
    })
  }

  # Run the Shiny app
  shinyApp(ui, server)
}
