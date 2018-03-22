planet_crs <- "+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

prep_planet <- function(directory){
  orders_regex <- dir(directory, recursive = TRUE) %>%
    str_subset("zipped/") %>%
    str_extract("\\d+") %>%
    str_c(collapse = "|")

  dir(directory, full.names = TRUE, recursive = TRUE) %>%
    str_subset("zipped/") %>%
    str_subset(orders_regex) %>%
    walk(~ unzip(.x, exdir = directory))

  directory %>%
    map(dir, recursive = FALSE, full.names = TRUE) %>%
    unlist() %>%
    discard(~ str_detect(.x, "README|zipped")) %>%
    walk(clean_dirs)
}

clean_dirs <- function(directory, SR_or_MS = "SR"){
  SR_or_MS <- str_to_upper(SR_or_MS)
  if(!SR_or_MS %in% c("SR", "MS")){
    stop("Dude, it says `SR` or `MS`. Get it together.")

  } else SR_or_MS <- str_extract(SR_or_MS, "SR|MS")

  sr_ms_count <- directory %>%
    dir(recursive = TRUE, full.names = TRUE) %>%
    str_count(paste0(SR_or_MS, "\\.tif$")) %>%
    sum()

  if(sr_ms_count < 1) unlink(directory, recursive = TRUE)
}


# build_planet_bricks <- function(directory, valid_dates_regex, target_poly, SR_or_MS = "SR"){
#   SR_or_MS <- str_to_upper(SR_or_MS)
#   if(!SR_or_MS %in% c("SR", "MS")){
#     stop("Dude, it says `SR` or `MS`. Get it together.")
#   }
#   
#   # target_extent <- target_poly %>% 
#   #   as("Spatial") %>% 
#   #   extent()
#   
#   udms <- directory %>%
#     dir(full.names = TRUE, recursive = TRUE) %>%
#     str_subset("_udm\\.tif$") %>%
#     str_subset(valid_dates_regex) %>%
#     map(brick) # %>%
#   #   discard(~ is.null(raster::intersect(extent(.x), target_extent))) %>% 
#   #   map(sf_crop, target_poly)
#   
#   directory %>% 
#     dir(full.names = TRUE, recursive = TRUE) %>% 
#     str_subset(paste0(SR_or_MS, "\\.tif$")) %>% 
#     str_subset(valid_dates_regex) %>%
#     map(brick) %>% 
#     discard(~ is.null(raster::intersect(extent(.x), target_extent))) %>% 
#     map(sf_crop, target_poly) %>% 
#     map2(udms, mask) #%>% 
#     # discard(~ all(is.na(values(.x)))) # some are coming back all NA...
# }

build_planet_bricks <- function(date, directory, sf_cropper){
  dir(directory, full.names = TRUE, recursive = TRUE) %>% 
    str_subset("\\SR.tif$") %>% 
    # str_subset("SR") %>% 
    str_subset(date) %>%
    brick()
    # map(raster) %>% 
    # map(sf_crop, sf_cropper) %>% 
    # brick()
  
  # udm_rast <- rasters %>% 
  #   keep(~ str_detect(names(.x), "udm")) %>% 
  #   pluck(1)
  # 
  # sr_rast <- rasters %>% 
  #   keep(~ str_detect(names(.x), "SR")) %>% 
  #   pluck(1)
  # 
  # mask(sr_rast, udm_rast) %>% 
  #   sf_crop(sf_cropper)
}

build_planet_bricks <- function(date, directory, 
                                  sf_cropper, 
                                  target_crs){
  files <- dir(directory, full.names = TRUE, recursive = TRUE) %>% 
    str_subset("SR\\.tif$") %>% 
    str_subset(date)
  
  rasters <- files %>% 
    map(brick) %>% 
    # map(projectRaster, crs = tummalapalle_sent_crs) %>% 
    # map_if(~ !identical(crs(.x), crs(tummalapalle_sent_crs)),
    #        ~ projectRaster(.x, crs = tummalapalle_sent_crs)) %>%
    # discard(~ is.null(intersect(extent(.x), sf_cropper))) %>%
    map(~ sf_crop(.x, st_transform(sf_cropper, crs = paste(crs(.x))))) %>% 
    map(trim) %>% 
    map_if(~ !identical(crs(.x), crs(target_crs)),
           ~ projectRaster(.x, crs = target_crs)) 
  # %>% 
    # unlist()
  
  return(rasters)
  
  # fasterized <- fasterize(sf_cropper, rasters[[1]])
  
  # bricks_10m <-  rasters %>% 
  #   keep(~ identical(res(.x), c(10, 10))) %>% 
  #   brick()
  # 
  # bricks_20m <- rasters %>%
  #   keep(~ identical(res(.x), c(20, 20))) %>% 
  #   brick() %>% 
  #   resample(bricks_10m[[1]])
  # 
  # brick(list(bricks_10m, bricks_20m))
}






