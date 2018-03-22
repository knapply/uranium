build_hyperion_bricks <- function(date, directory, sf_cropper){
  files <- dir(directory, full.names = TRUE, recursive = TRUE) %>% 
    str_subset("\\.TIF$") %>% 
    str_subset(date)
  
  # band_name <- files %>% 
  #   str_extract("B\\d{3}")
  
  rasters <- files %>% 
    map(raster) %>% 
    keep(~ intersect(extent(.x), rect)) %>% 
    map(sf_crop, sf_cropper) %>% 
    brick()
  
  # bricks_10m <-  rasters %>% 
  #   keep(~ identical(res(.x), c(10, 10))) %>% 
  #   brick()
  # 
  # bricks_20m <- rasters %>%
  #   keep(~ identical(res(.x), c(20, 20))) %>% 
  #   brick() %>% 
  #   resample(bricks_10m[[1]])
  
  # brick(rasters)
  return(rasters)
}
