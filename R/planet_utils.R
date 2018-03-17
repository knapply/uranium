prep_planet <- function(directory){
  orders_regex <- directory %>% 
    dir(recursive = TRUE) %>% 
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

plot_planet_rgb <- function(rast_brick, red = 3, green = 2, blue = 1, stretch = "lin", ...){
  plotRGB(rast_brick, r = red, g = green, b = blue, tck = 0,
          stretch = stretch, ...)
}

clean_dirs <- function(directory, SR_or_MS){
  sr_count <- directory %>% 
    dir(recursive = TRUE, full.names = TRUE) %>% 
    str_count(paste0(SR_or_MS, "\\.tif$")) %>% 
    sum()
  
  if(sr_count < 1) unlink(dir, recursive = TRUE)
}


build_planet_bricks <- function(directory, valid_dates_regex){
  udms <- directory %>% 
    dir(full.names = TRUE, recursive = TRUE) %>% 
    str_subset(., "_udm\\.tif$") %>% 
    str_subset(., valid_dates_regex) %>%
    map(brick) %>% 
    map(sf_crop, tumm_area)
}



