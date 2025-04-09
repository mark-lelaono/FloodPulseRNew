#' Start FloodPulseRNew GUI
#'
#' Launches a Shiny application for monitoring floods using Sentinel-1 imagery from Google Earth Engine.
#' Requires a Google Earth Engine Service Account Key (SaK) file for initialization.
#'
#' @export
start_floodpulse <- function() {

  # Check and load required packages
  required_packages <- c("shiny", "leaflet", "sf", "rgee", "leaflet.extras", "googleCloudStorageR")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop("The following required packages are not installed: ",
         paste(missing_packages, collapse = ", "),
         ". Please install them using install.packages().")
  }

  lapply(required_packages, library, character.only = TRUE)

  # Verify that FloodPulseRNew is loaded (for package context)
  if (!"FloodPulseRNew" %in% (.packages())) {
    stop("Failed to load the FloodPulseRNew package. Please reinstall using devtools::install().")
  }

  # Initialize Google Earth Engine with Service Account Key (SaK)
  sak_path <- Sys.getenv("GEE_SAK_PATH", unset = "C:/Users/ADMIN/Documents/fifth-catcher-456205-e8-21eef9f80731.json")
  if (!file.exists(sak_path)) {
    stop("Service Account Key (SaK) file not found at: ", sak_path,
         "\nPlease ensure the SaK file is placed in the specified directory or set the GEE_SAK_PATH environment variable.")
  }

  tryCatch({
    email <- suppressWarnings(jsonlite::fromJSON(sak_path)$client_email)

    if (is.null(email) || email == "") stop("Invalid SaK file: client_email not found.")
    rgee::ee_utils_sak_copy(sakfile = sak_path)
    rgee::ee_Initialize(email = email, drive = TRUE, gcs = TRUE)
  }, error = function(e) {
    stop("Failed to initialize Google Earth Engine: ", e$message,
         "\nEnsure the SaK file is valid and permissions are correct.")
  })

  # Define Shiny app
  shinyApp(
    ui = fluidPage(
      titlePanel("FloodPulseRNew - Sentinel-1 Flood Monitoring"),
      sidebarLayout(
        sidebarPanel(
          fileInput("shapefile", "Upload Shapefile (.zip with .shp, .shx, .dbf, .prj)",
                    accept = c(".zip"), multiple = FALSE),
          checkboxInput("use_drawn_aoi", "Use drawn AOI instead of uploaded shapefile", value = FALSE),
          dateInput("start_date", "Start Date", value = Sys.Date() - 30),
          dateInput("end_date", "End Date", value = Sys.Date()),
          actionButton("fetch", "Fetch Sentinel-1 Data"),
          hr(),
          verbatimTextOutput("debug")
        ),
        mainPanel(
          leafletOutput("map", height = "600px")
        )
      )
    ),

    server = function(input, output, session) {

      # Reactive to store uploaded or drawn AOI
      aoi_data <- reactiveVal(NULL)

      # Initialize Leaflet map with satellite imagery
      output$map <- renderLeaflet({
        leaflet() %>%
          addProviderTiles(providers$Esri.WorldImagery) %>%
          leaflet.extras::addDrawToolbar(
            targetGroup = "draw",
            polylineOptions = FALSE,
            circleOptions = FALSE,
            markerOptions = FALSE,
            circleMarkerOptions = FALSE,
            editOptions = leaflet.extras::editToolbarOptions()
          ) %>%
          setView(lng = 38.0, lat = 3.0, zoom = 6)
      })

      # Observe shapefile upload (expecting .zip)
      observeEvent(input$shapefile, {
        req(input$shapefile)
        zip_path <- input$shapefile$datapath
        temp_dir <- file.path(tempdir(), paste0("shapefile_", format(Sys.time(), "%Y%m%d_%H%M%S")))
        dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

        tryCatch({
          # Unzip the file
          unzip(zip_path, exdir = temp_dir)
          shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)

          # Debug: Print extracted files
          updateTextInput(session, "debug", value = paste("Extracted files:", paste(list.files(temp_dir, full.names = TRUE), collapse = ", ")))

          if (length(shp_files) == 0) stop("No .shp file found in the uploaded ZIP.")
          shp_path <- shp_files[1]
          shp_base <- tools::file_path_sans_ext(basename(shp_path))
          shp_dir <- dirname(shp_path)

          # Check for required shapefile components
          required_ext <- c(".shx", ".dbf")
          missing <- sapply(required_ext, function(ext) {
            !file.exists(file.path(shp_dir, paste0(shp_base, ext)))
          })
          if (any(missing)) {
            stop("Missing required shapefile components: ", paste(required_ext[missing], collapse = ", "))
          }

          # Read shapefile
          aoi <- sf::st_read(shp_path, quiet = TRUE)
          if (is.na(sf::st_crs(aoi))) {
            sf::st_crs(aoi) <- 4326
            message("Assigned CRS: WGS84 (EPSG:4326)")
          }
          if (!identical(sf::st_crs(aoi)$epsg, 4326)) {
            aoi <- sf::st_transform(aoi, 4326)
            message("Transformed CRS to WGS84 (EPSG:4326)")
          }

          # Debug: Print AOI info
          aoi_bbox <- sf::st_bbox(aoi)
          updateTextInput(session, "debug", value = paste(
            "Shapefile loaded. Bounding box: xmin=", aoi_bbox["xmin"],
            ", ymin=", aoi_bbox["ymin"],
            ", xmax=", aoi_bbox["xmax"],
            ", ymax=", aoi_bbox["ymax"]
          ))

          aoi_data(aoi)

          # Update map with shapefile and zoom to extent
          leafletProxy("map", session) %>%
            clearGroup("uploaded_aoi") %>%
            addPolygons(
              data = aoi,
              color = "black",
              weight = 2,
              fillOpacity = 0.1,
              group = "uploaded_aoi"
            ) %>%
            fitBounds(
              lng1 = aoi_bbox["xmin"],
              lat1 = aoi_bbox["ymin"],
              lng2 = aoi_bbox["xmax"],
              lat2 = aoi_bbox["ymax"]
            )

        }, error = function(e) {
          showNotification(paste("Error processing shapefile:", e$message), type = "error")
          updateTextInput(session, "debug", value = paste("Error:", e$message))
        })
      })

      # Observe drawn AOI
      observeEvent(input$map_draw_new_feature, {
        if (input$use_drawn_aoi) {
          feature <- input$map_draw_new_feature
          if (feature$geometry$type %in% c("Polygon", "Rectangle")) {
            coords <- feature$geometry$coordinates[[1]]
            coords_matrix <- do.call(rbind, lapply(coords, unlist))
            polygon <- sf::st_polygon(list(coords_matrix)) %>% sf::st_sfc(crs = 4326)
            aoi <- sf::st_sf(geometry = polygon)
            aoi_data(aoi)

            # Debug: Print drawn AOI info
            aoi_bbox <- sf::st_bbox(aoi)
            updateTextInput(session, "debug", value = paste(
              "Drawn AOI created. Bounding box: xmin=", aoi_bbox["xmin"],
              ", ymin=", aoi_bbox["ymin"],
              ", xmax=", aoi_bbox["xmax"],
              ", ymax=", aoi_bbox["ymax"]
            ))

            leafletProxy("map", session) %>%
              clearGroup("drawn_aoi") %>%
              addPolygons(
                data = aoi,
                color = "blue",
                weight = 2,
                fillOpacity = 0.1,
                group = "drawn_aoi"
              ) %>%
              fitBounds(
                lng1 = aoi_bbox["xmin"],
                lat1 = aoi_bbox["ymin"],
                lng2 = aoi_bbox["xmax"],
                lat2 = aoi_bbox["ymax"]
              )
          }
        }
      })

      # Utility function to get the active AOI
      get_active_aoi <- reactive({
        req(!is.null(aoi_data()))
        aoi_data()
      })

      # Fetch Sentinel-1 data
      observeEvent(input$fetch, {
        req(input$start_date, input$end_date)

        withProgress(message = "Fetching Sentinel-1 data...", value = 0, {
          # Ensure AOI is selected
          aoi <- get_active_aoi()
          req(aoi)

          # Convert AOI to ee$Geometry
          sf_simplified <- sf::st_transform(aoi, 4326) %>% sf::st_union()
          aoi_ee <- rgee::sf_as_ee(sf_simplified)

          # Load Sentinel-1 data
          s1 <- ee$ImageCollection("COPERNICUS/S1_GRD")$
            filterBounds(aoi_ee)$
            filterDate(as.character(input$start_date), as.character(input$end_date))$
            filter(ee$Filter$listContains("transmitterReceiverPolarisation", "VV"))$
            filter(ee$Filter$eq("instrumentMode", "IW"))$
            select("VV")

          # Check if data is available
          img_count <- s1$size()$getInfo()
          if (img_count == 0) {
            showNotification("No Sentinel-1 images available for the selected date range and AOI.", type = "error")
            return()
          }

          image <- s1$mean()$clip(aoi_ee)

          # Get map visualization parameters
          vis_params <- list(min = -25, max = 0)

          # Update map
          leafletProxy("map", session) %>%
            clearImages() %>%
            addTiles(
              urlTemplate = image$getMapId(vis_params)$tile_fetcher$url_format,
              options = tileOptions(opacity = 0.7),
              layerId = "sentinel1"
            )
        })
      })

      # Debug info output
      output$debug <- renderPrint({
        list(
          AOI_available = !is.null(aoi_data()),
          Use_drawn = input$use_drawn_aoi,
          Start = input$start_date,
          End = input$end_date
        )
      })
    }
  )
}
