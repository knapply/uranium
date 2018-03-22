sent2_crs <- "+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

sentinel2_band_info <- read_rds("data/sentinel2_band_info.rds")

build_sentinel_bricks <- function(date, directory, 
                                  sf_cropper, 
                                  target_crs){
  files <- dir(directory, full.names = TRUE, recursive = TRUE) %>% 
    str_subset("IMG_DATA") %>% 
    str_subset("jp2$") %>% 
    str_subset(date) %>% 
    discard(~ str_detect(.x, "TCI"))
  
  rasters <- files %>% 
    map(raster) %>% 
    # map(projectRaster, crs = tummalapalle_sent_crs) %>% 
    # map_if(~ !identical(crs(.x), crs(tummalapalle_sent_crs)),
    #        ~ projectRaster(.x, crs = tummalapalle_sent_crs)) %>%
    # discard(~ is.null(intersect(extent(.x), sf_cropper))) %>%
    map(~ sf_crop(.x, st_transform(sf_cropper, crs = paste(crs(.x))))) %>% 
    map(trim) %>% 
    map_if(~ !identical(crs(.x), crs(tummalapalle_sent_crs)),
           ~ projectRaster(.x, crs = tummalapalle_sent_crs))
  
  # fasterized <- fasterize(sf_cropper, rasters[[1]])
  
  bricks_10m <-  rasters %>% 
    keep(~ identical(res(.x), c(10, 10))) %>% 
    brick()
  
  bricks_20m <- rasters %>%
    keep(~ identical(res(.x), c(20, 20))) %>% 
    brick() %>% 
    resample(bricks_10m[[1]])
  
  brick(list(bricks_10m, bricks_20m))
}

# fasterized <- fasterize::fasterize(tummalapalle_poly_sf,
                                   # sent_bricks[[1]][[1]])


