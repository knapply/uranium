source("R/global.R")

#! sentinel 2 =============================================================================
#_ vars  =================================================================================
jaduguda_poly_sf <- "raw_data/in/jaduguda/jaduguda_poly.kml" %>% 
  read_sf() %>% 
  st_zm() %>% 
  st_transform(sent2_crs) %>% 
  st_buffer(set_units(2, km))

jaduguda_vectors_sf <- read_rds("data/jaduguda_vectors.rds") %>% 
  st_transform(sent2_crs)

jaduguda_water_sf <- jaduguda_vectors_sf %>% 
  select(geometry) %>% 
  st_cast("LINESTRING")

jaduguda_sent_dir <- "imagery/in/jaduguda/sentinel_2"

sent_dates <- dir(jaduguda_sent_dir, full.names = TRUE, recursive = TRUE) %>% 
  str_subset("IMG_DATA") %>% 
  str_subset("jp2$") %>% 
  discard(~ str_detect(.x, "TCI")) %>% 
  str_extract("\\d{8}") %>% 
  unique()

#_ build brick list ======================================================================
sent_bricks <- sent_dates %>% 
  map(build_sentinel_bricks, jaduguda_sent_dir)

#_ spectral bands ========================================================================
sent_bricks %>% 
  map(names) %>% 
  map(str_extract, "B.{2}$") %>% 
  unique() %>% 
  as_vector() %>% 
  tibble(band = .) %>% 
  mutate(description = case_when(band == "B02" ~ "blue",
                                 band == "B03" ~ "green",
                                 band == "B04" ~ "red",
                                 band == "B08" ~ "nir",
                                 band == "B05" ~ "veg_red_edge_1",
                                 band == "B06" ~ "veg_red_edge_2",
                                 band == "B07" ~ "veg_red_edge_3",
                                 band == "B8A" ~ "narrow_nir",
                                 band == "B11" ~ "swir_1",
                                 band == "B12" ~ "swir_2" )) %>% 
  mutate(index = row_number())

#_ NIRs =================================================================================
sent_nir <- sent_bricks %>% 
  map(~ .x[[4]]) %>% 
  set_names(sent_dates) %>% 
  brick()

plot(sent_nir, col = cust_pal, axes = FALSE, legend = FALSE)

#_ reds ==================================================================================
sent_reds <- sent_bricks %>% 
  map(~ .x[[3]]) %>% 
  brick()

plot(sent_reds, col = cust_pal, axes = FALSE, legend = FALSE)

#_ red edge ==================================================================================
get_cust_red_edge <- function(raster_brick){
  raster_brick[[5]]
}

sent_red_edges <- sent_bricks %>% 
  map(get_cust_red_edge) %>% 
  brick()

plot(sent_red_edges, col = cust_pal, axes = FALSE)

#_ indices ==============================================================================
sent_indices <- sent_bricks %>% 
  map(~ spectralIndices(.x, blue = 1, green = 2, red = 3,
                        nir = 4, swir2 = 9))

sent_indices %>% 
  map(names)

sent_indices %>% 
  pluck(1) %>% 
  .[[1]] %>% 
  image(col = cust_pal)
plot(jaduguda_water_sf$geometry, add = TRUE, col = "black", lwd = 1)

#___ NDVI ================================================================================
sent_ndvi <- sent_indices %>% 
  map(~ .x$NDVI) %>% 
  set_names(sent_dates) %>% 
  brick()

plot(sent_ndvi, col = cust_pal, axes = FALSE, legend = FALSE)

#___ NDWI ================================================================================
sent_ndwi <- sent_indices %>% 
  map(~ .x$NDWI) %>% 
  set_names(sent_dates) %>% 
  brick()

plot(sent_ndvi, col = cust_pal, axes = FALSE, legend = FALSE)
  
#___ green/blue ==========================================================================
get_sent_green_blue <- function(raster_brick){
  green_band <- raster_brick[[2]]
  blue_band <- raster_brick[[1]]
  green_band / blue_band
}

sent_green_blue <- sent_bricks %>% 
  map(get_sent_green_blue) %>% 
  set_names(sent_dates) %>% 
  brick()

plot(sent_green_blue, col = cust_pal, axes = FALSE, legend = FALSE)


#! planet ================================================================================
#_ vars ==================================================================================
jaduguda_poly_sf <- "raw_data/in/jaduguda/jaduguda_poly.kml" %>% 
  read_sf() %>% 
  st_zm() %>% 
  st_transform(planet_crs) %>% 
  st_buffer(set_units(1, km))

jaduguda_vectors_sf <- read_rds("data/jaduguda_vectors.rds") %>% 
  st_transform(planet_crs)

jaduguda_water_sf <- jaduguda_vectors_sf %>% 
  select(geometry) %>% 
  st_cast("LINESTRING")

jaduguda_planet_dir <- "imagery/in/jaduguda/planet"

planet_dates <- dir(jaduguda_planet_dir, full.names = TRUE, recursive = TRUE) %>% 
  str_subset("SR\\.tif$") %>% 
  str_extract("\\d{8}") %>% 
  unique()

#_ build brick list ======================================================================
planet_bricks <- jaduguda_planet_dir %>% 
  dir(full.names = TRUE, recursive = TRUE) %>% 
  str_subset("SR\\.tif$") %>% 
  map(brick) %>% 
 discard(~ is.null(raster::intersect(extent(.x),
                                     extent(as(jaduguda_poly_sf, "Spatial"))))) %>% 
  map(sf_crop, jaduguda_poly_sf) %>%  # some are coming back all NA...
  discard(~ all(is.na(values(.x))))

#_ indices ===============================================================================
planet_indices <- planet_bricks %>% 
  map(~ spectralIndices(.x, blue = 1, green = 2, red = 3, nir = 4))

plot(planet_indices[[1]], col = cust_pal, axes = FALSE, legend = FALSE)


planet_indices %>% 
  map(names)

#!!! maybe?====
# planet_indices %>%       
#   map(~ .x$RVI) %>% 
#   brick() %>% 
#   levelplot(par.settings = cust_theme)


#___ NDVI ================================================================================
planet_ndvi <- planet_indices %>% 
  map(~ .x$NDVI) %>% 
  set_names(planet_dates) %>% 
  brick()

# plot(planet_ndvi, col = cust_pal, axes = FALSE, legend = FALSE)
levelplot(planet_ndvi, par.settings = cust_theme, 
          main = "NDVI", sub = "Planet")

#___ NDWI ================================================================================
planet_ndwi <- planet_indices %>% 
  map(~ .x$NDWI) %>% 
  set_names(planet_dates) %>% 
  brick()

# plot(planet_ndvi, col = cust_pal, axes = FALSE, legend = FALSE)
levelplot(planet_ndwi, par.settings = cust_theme,
          main = "NDWI", sub = "Planet")

#___ green/blue ==========================================================================
get_planet_green_blue <- function(raster_brick){
  green_band <- raster_brick[[2]]
  blue_band <- raster_brick[[1]]
  green_band / blue_band
}

planet_green_blue <- planet_bricks %>% 
  map(get_planet_green_blue) %>% 
  set_names(planet_dates) %>% 
  brick()

levelplot(planet_green_blue, par.settings = cust_theme,
          main = "Green/Blue", sub = "Planet")
 
#! hyperion ==============================================================================
#_ vars ==================================================================================
hyperion_crs <- "+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

jaduguda_poly_sf <- "raw_data/in/jaduguda/jaduguda_poly.kml" %>% 
  read_sf() %>% 
  st_zm() %>% 
  st_transform(hyperion_crs)

jaduguda_vectors_sf <- read_rds("data/jaduguda_vectors.rds") %>% 
  st_transform(hyperion_crs)

jaduguda_water_sf <- jaduguda_vectors_sf %>% 
  dplyr::filter(macro == "water")# %>% 
  # dplyr::filter(Name != "tailings") 

jaduguda_water_lines_sf <- jaduguda_water_sf %>% 
  st_cast("LINESTRING")


hyperion_dir <- "imagery/in/jharkhand/hyperion"

hyperion_dates <- dir(hyperion_dir, full.names = TRUE, recursive = TRUE) %>% 
  str_subset("\\.TIF$") %>% 
  str_extract("(2010|2012)\\d+") %>% 
  unique()

hyperion_bands <- dir(hyperion_dir, full.names = TRUE, recursive = TRUE) %>% 
  str_subset("\\.TIF$") %>% 
  str_extract("B\\d{3}") %>% 
  unique()

#_ build brick list ======================================================================
hyperion_bands <- hyperion_dir %>% 
  dir(full.names = TRUE, recursive = TRUE) %>% 
  str_subset("\\.TIF$") %>%
  str_subset("EO1H1400442010055110KL_1T") %>% 
  unique() %>% 
  # str_subset(hyperion_dates[[1]]) %>% 
  map(raster) %>% 
  map(sf_crop, jaduguda_vectors_sf)

hyperion_brick <- hyperion_bands %>%
  brick()

animate(hyperion_brick, col = cust_pal,
        legend = FALSE,
        ann = FALSE, axes = FALSE)

k_means <- hyperion_brick %>% 
  # brick() %>% 
  cust_kmeans()

# plot(k_means$map)
image(k_means$map, col = cust_pal, axes = FALSE)
plot(jaduguda_water_lines_sf$geometry,
     add = TRUE, col = "white", lwd = 3)

mapview(k_means$map)

hyperion_masks <- hyperion_bricks %>% 
  map(~ raster::mask(.x, as(jaduguda_water_sf, "Spatial")))



sd_all <- hyperion_masks %>%
  # map(~ sd(values(.x), na.rm = TRUE)) %>% 
  map(~ mean(values(.x), na.rm = TRUE)) %>% 
  unlist() %>% 
  sort(decreasing = TRUE)

sd_top <- hyperion_masks %>% # hyperion_bricks %>% 
  set_names(hyperion_bands) %>%
  keep(~ mean(values(.x), na.rm = TRUE) %in% sd_all[1:9]) 

sd_top %>% 
  # hyperion_masks[1:20] %>% 
  brick() %>% 
  sf_crop(jaduguda_water_sf) %>% 
  levelplot(par.settings = cust_theme, axes = FALSE)

top_sd <- sd_top[[1]]
plot(sd_top[[1]], col = cust_pal, axes = FALSE, legend = FALSE)

#_ indices ==============================================================================
#___ uh? =================================================================================
#___ uh? =================================================================================



