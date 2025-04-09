#' Fetch Sentinel-1 GRD Data from Google Earth Engine
#' @param aoi An sf object defining the area of interest (or a path to a shapefile)
#' @param start_date Start date (YYYYMMDD)
#' @param end_date End date (YYYYMMDD)
#' @return Asset ID of the exported Sentinel-1 image in GEE
#' @export
fetch_floodpulse <- function(aoi, start_date, end_date) {
  library(rgee)
  library(sf)

  tryCatch({
    # If aoi is a character (file path), read the shapefile; otherwise, assume it's an sf object
    if (is.character(aoi)) {
      # Check if the file is a shapefile
      file_ext <- tolower(tools::file_ext(aoi))
      if (file_ext != "shp") {
        stop("If aoi is a file path, it must be a shapefile (.shp)")
      }
      aoi_sf <- st_read(aoi, quiet = TRUE)
    } else if (inherits(aoi, "sf")) {
      aoi_sf <- aoi
    } else {
      stop("aoi must be either a path to a shapefile (.shp) or an sf object")
    }

    # Ensure the AOI is in WGS84 (EPSG:4326)
    if (!identical(st_crs(aoi_sf)$epsg, 4326)) {
      aoi_sf <- st_transform(aoi_sf, 4326)
    }

    # Convert the AOI to a GEE geometry
    aoi_geojson <- sf::st_as_sf(aoi_sf) %>% sf::st_combine() %>% sf::st_geometry()
    aoi_ee <- geojson_to_ee(aoi_geojson)

    # Access Sentinel-1 collection
    s1 <- ee$ImageCollection("COPERNICUS/S1_GRD")$
      filterBounds(aoi_ee)$
      filterDate(start_date, end_date)$
      filter(ee$Filter$eq("instrumentMode", "IW"))$  # Interferometric Wide swath mode
      filter(ee$Filter$eq("orbitProperties_pass", "DESCENDING"))$  # Common for flood mapping
      select("VV")  # Use VV polarization for flood detection

    # Check if any images are available
    count <- s1$size()$getInfo()
    if (count == 0) {
      stop("No Sentinel-1 images found for the specified date range and AOI.")
    }

    # Take the first image for simplicity (or you could mosaic, etc.)
    s1_image <- s1$first()

    # Define the Asset path (e.g., users/yourusername/FloodPulseR_S1_date)
    asset_id <- paste0("users/", ee_get_earthengine_user(), "/FloodPulseR_S1_", start_date, "_", end_date)

    # Export the image to an Asset
    task <- ee$batch$Export$image$toAsset(
      image = s1_image,
      description = paste0("FloodPulseR_S1_", start_date, "_", end_date),
      assetId = asset_id,
      region = aoi_ee,
      scale = 10,  # Sentinel-1 resolution
      maxPixels = 1e13
    )

    # Start the export task
    task$start()

    # Wait for the task to complete
    message("Exporting Sentinel-1 data to Earth Engine Asset. This may take a few minutes...")
    repeat {
      status <- task$status()$state
      if (status %in% c("COMPLETED", "FAILED")) break
      Sys.sleep(10)  # Check every 10 seconds
    }

    if (status == "FAILED") {
      stop("Export to Asset failed: ", task$status()$message)
    }

    # Return the Asset ID
    return(asset_id)
  }, error = function(e) {
    stop("Error fetching Sentinel-1 data from GEE: ", e$message)
  })
}

#' Download Sentinel-1 Data from GEE Asset
#' @param asset_id Asset ID from fetch_floodpulse
#' @param output_dir Local directory
#' @return Local file path
#' @export
download_floodpulse <- function(asset_id, output_dir = "downloads") {
  library(rgee)

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  tryCatch({
    # Load the Asset as an ee.Image
    s1_image <- ee$Image(asset_id)

    # Define a local path to save the GeoTIFF
    local_path <- file.path(output_dir, paste0(basename(asset_id), ".tif"))

    # Download the Asset as a GeoTIFF
    ee_image_to_local(
      ee_object = s1_image,
      region = s1_image$geometry(),
      scale = 10,
      fileFormat = "GeoTIFF",
      filePath = local_path,
      overwrite = TRUE
    )

    return(local_path)
  }, error = function(e) {
    stop("Error downloading Asset: ", e$message)
  })
}
