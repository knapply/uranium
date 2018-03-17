sf_crop <- function(raster, sf){
  crop(raster, as(sf, "Spatial"))
}