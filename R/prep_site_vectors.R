prep_site_vectors <- function(country, site_name){
  if(!dir.exists(paste0("raw_data/", country, "/", site_name))){
    stop("`site_name` not found")
  } else message(paste0("raw_data/", country, "/", site_name), " found")
  
  files <- dir(paste0("raw_data/", country, "/", site_name),
               full.names = TRUE, recursive = TRUE)
  
  valid_regex <- c("effluent", "pond", "tailings",
                   "main_area", "open_pit", "spoils") %>% 
    str_c(collapse = "|")
  
  valid_files <- files %>% 
    str_extract_all(valid_regex) %>% 
    unlist() %>% 
    unique()
  
  if("effluent" %in% valid_files){
    effluent_sf <- files %>% 
      str_subset("effluent_\\d+\\.kml") %>% 
      map(read_sf) %>% 
      reduce(rbind) %>% 
      mutate(macro = "water",
             meso = "treatment",
             micro = "effluent")
  } else effluent_sf <- NULL
  
  if("pond" %in% valid_files){
    pond_sf <- files %>% 
      str_subset("pond_\\d+.kml") %>% 
      map(read_sf) %>% 
      reduce(rbind) %>% 
          mutate(macro = "water",
                 meso = "treatment",
                 micro = "pond")
  } else pond_sf <- NULL
  
  if("tailings" %in% valid_files){
  tailings_sf <- files %>% 
    str_subset("tailings\\.kml") %>% 
    map(read_sf) %>% 
    reduce(rbind) %>% 
        mutate(macro = "water",
               meso = "treatment",
               micro = "tailings")
  } else tailings_sf <- NULL
  
  # if("entrance" %in% valid_files){
  #   entrance_sf <- files %>% 
  #     str_subset("entrance") %>% 
  #     read_sf() %>% 
  #     mutate(macro = "layout",
  #        meso = NA,
  #        micro = "mine entrance")
  # } else entrance_sf <- NULL
  
  if("main_area" %in% valid_files){
    main_area_sf <- files %>% 
      str_subset("main_area\\.kml") %>% 
      read_sf() %>% 
      mutate(macro = "layout",
             meso = "main area",
             micro = NA)
  } else main_area_sf <- NULL
  
  if("open_pit" %in% valid_files){
    open_pit_sf <- files %>% 
      str_subset("open_pit\\.kml") %>% 
      read_sf() %>% 
      mutate(macro = "layout",
             meso = "open pit",
             micro = NA)
  } else open_pit_sf <- NULL
  
    if("spoils" %in% valid_files){
      spoils_sf <- files %>%
        str_subset("spoils_\\d+\\.kml") %>%
        read_sf() %>%
        mutate(macro = "layout",
               meso = "spoils",
               micro = NA)
  } else spoils_sf <- NULL
  
  list(effluent_sf, pond_sf, tailings_sf, main_area_sf,
       open_pit_sf) %>% 
    compact() %>% 
    reduce(rbind) %>% 
    st_zm()
  
}

# test it: 
# prep_site_vectors("in", "jaduguda")$geometry %>% plot()
# prep_site_vectors("in", "tummalapalle")$geometry %>% plot()
# prep_site_vectors("in", "turamdih_banduhurang")$geometry %>% plot()
# prep_site_vectors("in", "turamdih_banduhurang")$geometry %>% mapview::mapview()

sites <- c("jaduguda", "tummalapalle", "turamdih_banduhurang")

sites %>% 
  map(~ prep_site_vectors("in", .x)) %>% 
  walk2(sites, ~ write_rds(.x, paste0("data/", .y, "_vectors.rds")))



