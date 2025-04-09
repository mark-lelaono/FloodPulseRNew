#' Process Sentinel-1 COG for Flood Analysis
#' @param cog_path Path to downloaded COG file (GeoTIFF from GEE with VV band)
#' @param output_path Output GeoTIFF path (default: "output.tif")
#' @param threshold Backscatter threshold in dB for flood detection (default: -15)
#' @param speckle_filter Apply speckle filtering to reduce noise (default: TRUE)
#' @return Processed file path
#' @export
process_floodpulse <- function(cog_path, output_path = "output.tif", threshold = -15, speckle_filter = TRUE) {
  library(terra)

  # Validate input file
  if (!file.exists(cog_path)) {
    stop("Input file does not exist: ", cog_path)
  }

  # Load the Sentinel-1 data
  tryCatch({
    rast <- rast(cog_path)
  }, error = function(e) {
    stop("Error loading GeoTIFF file: ", e$message)
  })

  # Check if the raster has at least one band
  if (nlyr(rast) < 1) {
    stop("Input GeoTIFF has no bands. Expected at least one band (VV polarization).")
  }

  # If the raster has multiple bands, select the first one (VV band as per fetch_floodpulse)
  if (nlyr(rast) > 1) {
    message("Multiple bands detected. Using the first band (assumed to be VV polarization).")
    rast <- rast[[1]]
  }

  # Apply speckle filtering to reduce noise (optional)
  if (speckle_filter) {
    message("Applying speckle filter to reduce noise...")
    # Use a simple 3x3 Lee filter for speckle reduction
    rast <- focal(rast, w = matrix(1, nrow = 3, ncol = 3), fun = mean, na.rm = TRUE)
  }

  # Flood detection: low backscatter indicates water
  message("Applying flood detection with threshold: ", threshold, " dB")
  flood_mask <- rast < threshold

  # Ensure the output directory exists
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Write the flood mask to a GeoTIFF file
  writeRaster(
    flood_mask,
    output_path,
    overwrite = TRUE,
    datatype = "INT1U",  # Binary mask (0 or 1)
    NAflag = 255,
    options = c("COMPRESS=LZW")  # Compress the output file
  )

  # Add metadata to the output file
  meta <- rast(output_path)
  meta <- set.names(meta, "FloodMask")
  meta <- set.extent(meta, ext(rast))  # Ensure the extent matches the input
  meta <- set.crs(meta, crs(rast))     # Ensure the CRS matches the input
  meta <- add_metadata(meta, list(
    processing_date = as.character(Sys.time()),
    threshold = as.character(threshold),
    speckle_filter = as.character(speckle_filter),
    source = "Google Earth Engine (Sentinel-1 VV)"
  ))
  writeRaster(meta, output_path, overwrite = TRUE)

  message("Flood mask saved to: ", output_path)
  return(output_path)
}

# Helper function to add metadata (not directly supported by terra, so we simulate it)
add_metadata <- function(rast, metadata_list) {
  # terra doesn't have a direct way to add custom metadata, so we can use descriptions
  desc <- paste(names(metadata_list), unlist(metadata_list), sep = ": ", collapse = "; ")
  rast <- set.names(rast, names(rast), description = desc)
  return(rast)
}
