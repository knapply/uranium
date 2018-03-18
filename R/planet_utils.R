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


build_planet_bricks <- function(directory, valid_dates_regex, target_poly, SR_or_MS = "SR"){
  SR_or_MS <- str_to_upper(SR_or_MS)
  if(!SR_or_MS %in% c("SR", "MS")){
    stop("Dude, it says `SR` or `MS`. Get it together.")
  }
  
  udms <- directory %>%
    dir(full.names = TRUE, recursive = TRUE) %>%
    str_subset("_udm\\.tif$") %>%
    str_subset(valid_dates_regex) %>%
    map(brick) %>%
    map(sf_crop, target_poly)
  
  directory %>% 
    dir(full.names = TRUE, recursive = TRUE) %>% 
    str_subset(paste0(SR_or_MS, "\\.tif$")) %>% 
    str_subset(valid_dates_regex) %>%
    map(brick) %>% 
    map(sf_crop, target_poly) %>% 
    walk2(udms, mask)
}