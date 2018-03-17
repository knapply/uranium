get_green_blue_index <- function(raster_brick){
  green_band <- raster_brick[[2]]
  blue_band <- raster_brick[[1]]
  
  green_band / blue_band
}