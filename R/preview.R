#' Preview Sentinel-1 COG Metadata
#' @param cog_path Path to downloaded COG file
#' @return List of metadata
#' @export
preview_floodpulse <- function(cog_path) {
  library(terra)
  rast <- rast(cog_path)
  metadata <- list(
    title = basename(cog_path),
    extent = as.character(ext(rast)),
    resolution = res(rast)
  )
  return(metadata)
}
