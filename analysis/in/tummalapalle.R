#' ---
#' title: "Tummalapalle Remote Sensing"
#' author: "Brendan Knapp"
#' output: 
#'   html_document:
#'     code_folding: hide
#'     toc: true
#'     toc_float: true
#' ---
#' 
#' body .main-container {
#' max-width: 1920px;
#' }
#' 


# rmarkdown::render("analysis/in/tummalapalle.R", knit_root_dir = getwd())

knitr::opts_chunk$set(echo=TRUE, warnings=FALSE, message=FALSE,
                      fig.width = 10, fig.height = 7, fig.align = "center")



source("R/global.R")

#! sentinel 2 =============================================================================
#_ vars  =================================================================================
tummalapalle_sent_crs <- "+proj=utm +zone=43 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "

tummalapalle_poly_sf <- "raw_data/in/tummalapalle/tummalapalle.kml" %>% 
  read_sf() %>% 
  st_zm() %>% 
  st_transform(tummalapalle_sent_crs) # %>% 
  # st_buffer(set_units(2, km))

tummalapalle_vectors_sf <- read_rds("data/tummalapalle_vectors.rds") %>% 
  st_transform(tummalapalle_sent_crs)

tummalapalle_vectors_lines_sf <- tummalapalle_vectors_sf %>% 
  st_cast("LINESTRING")

tummalapalle_mill_area_sf <- tummalapalle_vectors_sf %>% 
  filter(Name == "tumm_mill_area")

tummalapalle_tailings_sf <- tummalapalle_vectors_sf %>% 
  filter(Name == "tailings")

tummalapalle_water_sf <- tummalapalle_vectors_sf %>% 
  filter(macro == "water") %>% 
  # select(geometry) %>% 
  st_cast("LINESTRING")

tummalapalle_sent_dir <- "imagery/in/tummalapalle/sentinel_2"

sent_dates <- dir(tummalapalle_sent_dir, full.names = TRUE, recursive = TRUE) %>% 
  str_subset("IMG_DATA") %>% 
  str_subset("jp2$") %>% 
  discard(~ str_detect(.x, "TCI")) %>% 
  str_extract("\\d{8}") %>% 
  unique()

### test

# tests <- dir(tummalapalle_sent_dir, full.names = TRUE, recursive = TRUE) %>%
#   str_subset("IMG_DATA") %>%
#   str_subset("jp2$") %>%
#   # str_subset(sent) %>%
#   discard(~ str_detect(.x, "TCI")) %>% 
#   map(raster)
# 
# tests %>% map(crs) %>% unique()

### test

#_ build brick list ======================================================================
pre_sent_bricks <- sent_dates %>% 
  map(~ build_sentinel_bricks(.x, tummalapalle_sent_dir,
                              tummalapalle_poly_sf, tummalapalle_sent_crs))

fasterized <- fasterize(tummalapalle_poly_sf, pre_sent_bricks[[1]])

pre_sent_bricks %>% map(res)

sent_bricks <- pre_sent_bricks %>% 
  map(resample, fasterized)

# sent_bricks %>% 
#   map(trim) %>% 
#   map(extent) %>% 
#   unique()

sent_bricks_mill <- sent_bricks %>% 
  map(sf_crop, tummalapalle_mill_area_sf)

sent_bricks_tailings <- sent_bricks %>% 
  map(sf_crop, tummalapalle_tailings_sf)

par(oma = c(2, 0, 2, 0))
plotRGB(sent_bricks[[2]], r = 3, g = 2, b = 1, stretch = "lin")
title(main = "Tummalapalle", outer = TRUE)
mtext(text = "Sentinel 2", side = 1, outer = TRUE)
plotRGB(sent_bricks[[2]], r = 3, g = 2, b = 1, stretch = "lin")
title(main = "Tummalapalle", outer = TRUE)
mtext(text = "Sentinel 2", side = 1, outer = TRUE)
plot(tummalapalle_vectors_lines_sf$geometry, lwd = 2, col = "red", add = TRUE)
plotRGB(sent_bricks_mill[[2]], r = 3, g = 2, b = 1, stretch = "lin")
plot(tummalapalle_water_sf$geometry, lwd = 1, col = "red", add = TRUE)
title(main = "Tummalapalle (Mine/Mill Area)", outer = TRUE)
mtext(text = "Sentinel 2", side = 1, outer = TRUE)
plotRGB(sent_bricks_tailings[[2]], r = 3, g = 2, b = 1, stretch = "lin")
plot(tummalapalle_vectors_lines_sf$geometry, lwd = 1, col = "red", add = TRUE)
title(main = "Tummalapalle (Tailings Pond)", outer = TRUE)
mtext(text = "Sentinel 2", side = 1, outer = TRUE)
par(oma = c(0, 0, 0, 0))

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

par(mfrow = c(2, 2))
sent_bricks[1:4] %>% 
  walk(~ plotRGB(.x, r = 3, g = 2, b = 1, stretch = "lin"))
par(mfrow = c(1, 1))
#_ NIRs =================================================================================
sent_nir <- sent_bricks %>% 
  map(unstack) %>% 
  flatten() %>% 
  keep(~ str_detect(names(.x), "B08")) %>% 
  set_names(sent_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(sent_nir, location = "Tummalapalle", 
           band_name = "B08",
           source = "sentinel2")
plot(sent_nir, col = cust_pal, axes = FALSE)

#_ reds ==================================================================================
sent_reds <- sent_bricks %>% 
  map(unstack) %>% 
  flatten() %>% 
  keep(~ str_detect(names(.x), "B04")) %>% 
  set_names(sent_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(sent_reds, location = "Tummalapalle", 
           band_name = "B04",
           source = "sentinel2")
plot(sent_reds, col = cust_pal, axes = FALSE)

sent_mill_reds <- sent_bricks_mill %>% 
  map(unstack) %>% 
  flatten() %>% 
  keep(~ str_detect(names(.x), "B04")) %>% 
  set_names(sent_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(sent_mill_reds, location = "Tummalapalle (Mine/Mill Area)", 
           band_name = "B04",
           source = "sentinel2")
plot(sent_mill_reds, col = cust_pal, axes = FALSE)


sent_tailings_reds <- sent_bricks_tailings %>% 
  map(unstack) %>% 
  flatten() %>% 
  keep(~ str_detect(names(.x), "B04")) %>% 
  set_names(sent_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(sent_tailings_reds, location = "Tummalapalle (Tailings Pond)", 
           band_name = "B04",
           source = "sentinel2")
plot(sent_tailings_reds, col = cust_pal, axes = FALSE)

#_ red edge ==============================================================================
sent_red_edges <- sent_bricks %>% 
  map(unstack) %>% 
  flatten() %>% 
  keep(~ str_detect(names(.x), "B06")) %>% 
  set_names(sent_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(sent_red_edges, location = "Tummalapalle", 
           band_name = "B06",
           source = "sentinel2")
plot(sent_red_edges, col = cust_pal, axes = FALSE)

sent_mill_red_edges <- sent_bricks_mill %>% 
  map(unstack) %>% 
  flatten() %>% 
  keep(~ str_detect(names(.x), "B06")) %>% 
  set_names(sent_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(sent_mill_red_edges, location = "Tummalapalle (Mine/Mill Area)", 
           band_name = "B06",
           source = "sentinel2")
plot_annotated(sent_mill_red_edges, tummalapalle_water_sf, 2, 3, source = "sentinel2")

sent_tailings_red_edges <- sent_bricks_tailings %>% 
  map(unstack) %>% 
  flatten() %>% 
  keep(~ str_detect(names(.x), "B06")) %>% 
  set_names(sent_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(sent_tailings_red_edges, location = "Tummalapalle (Tailings Pond)", 
           band_name = "B06",
           source = "sentinel2")
plot_annotated(sent_tailings_red_edges, tummalapalle_water_sf, 2, 3, source = "sentinel2")

#_ indices ==============================================================================
sent_indices <- sent_bricks %>% 
  map(~ spectralIndices(.x, blue = 1, green = 2, red = 3,
                        nir = 4, swir2 = 9)) %>% 
  set_names(sent_dates) %>% 
  .[sort(names(.))]

sent_indices %>% 
  walk2(sort(sent_dates),
        ~ plot_indices(.x, vector_sf = tummalapalle_water_sf, nrow = 3, ncol = 6,
                       title = .y, source = "sentinel2"))

sent_indices_mill <- sent_bricks_mill %>% 
  map(~ spectralIndices(.x, blue = 1, green = 2, red = 3,
                        nir = 4, swir2 = 9)) %>% 
  set_names(sent_dates) %>% 
  .[sort(names(.))]

sent_indices_mill %>% 
  walk2(sort(sent_dates),
        ~ plot_indices(.x, vector_sf = tummalapalle_water_sf, nrow = 3, ncol = 6,
                       title = .y, source = "sentinel2"))

sent_indices_tailings <- sent_bricks_tailings %>% 
  map(~ spectralIndices(.x, blue = 1, green = 2, red = 3,
                        nir = 4, swir2 = 9)) %>% 
  set_names(sent_dates) %>% 
  .[sort(names(.))]

sent_indices_tailings %>% 
  walk2(sort(sent_dates),
        ~ plot_indices(.x, vector_sf = tummalapalle_water_sf, nrow = 3, ncol = 6,
                       title = .y, source = "sentinel2"))

#___ NDVI ================================================================================
sent_ndvi <- sent_indices %>% 
  map(~ .x$NDVI) %>% 
  set_names(sent_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(sent_ndvi, location = "Tummalapalle", 
           index = "NDVI",
           source = "sentinel2")
plot_annotated(sent_tailings_red_edges, tummalapalle_water_sf, 2, 3,
               title = "NDVI", source = "sentinel2")

sent_ndvi_mill <- sent_indices_mill %>% 
  map(~ .x$NDVI) %>% 
  set_names(sent_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(sent_ndvi_mill, location = "Tummalapalle (Mine/Mill Area)", 
           index = "NDVI",
           source = "sentinel2")
plot_annotated(sent_ndvi_mill, tummalapalle_water_sf, 2, 3,
               title = "NDVI", source = "sentinel2")

sent_ndvi_tailings <- sent_indices_tailings %>% 
  map(~ .x$NDVI) %>% 
  set_names(sent_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(sent_ndvi_tailings, location = "Tummalapalle (Tailings Pond)", 
           index = "NDVI",
           source = "sentinel2")
plot_annotated(sent_ndvi_tailings, tummalapalle_water_sf, 2, 3,
               title = "NDVI", source = "sentinel2")

#___ NDWI ================================================================================
sent_ndwi <- sent_indices %>% 
  map(~ .x$NDWI) %>% 
  set_names(sent_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(sent_ndwi, location = "Tummalapalle", 
           index = "NDWI",
           source = "sentinel2")
plot_annotated(sent_ndwi, tummalapalle_water_sf, 2, 3,
               title = "NDWI", source = "sentinel2")


sent_ndwi_mill <- sent_indices_mill %>% 
  map(~ .x$NDWI) %>% 
  set_names(sent_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(sent_ndwi_mill, location = "Tummalapalle (Mine/Mill Area)", 
           index = "NDWI",
           source = "sentinel2")
plot_annotated(sent_ndwi_mill, tummalapalle_water_sf, 2, 3,
               title = "NDWI", source = "sentinel2")

sent_ndwi_tailings <- sent_indices_tailings %>% 
  map(~ .x$NDWI) %>% 
  set_names(sent_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(sent_ndwi_tailings, location = "Tummalapalle (Tailings Pond)", 
           index = "NDWI",
           source = "sentinel2")
plot_annotated(sent_ndwi_tailings, tummalapalle_water_sf, 2, 3,
               title = "NDWI", source = "sentinel2")
  
#___ green/blue ==========================================================================
get_sent_green_blue <- function(raster_brick){
  green_band <- raster_brick[[2]]
  blue_band <- raster_brick[[1]]
  green_band / blue_band
}

sent_green_blue <- sent_bricks %>% 
  map(get_sent_green_blue) %>% 
  set_names(sent_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(sent_green_blue, location = "Tummalapalle", 
           index = "Green/Blue",
           source = "sentinel2")
plot_annotated(sent_ndwi, tummalapalle_water_sf, 2, 3,
               title = "Green/Blue", source = "sentinel2")

sent_green_blue_mill <- sent_indices_mill %>% 
  map(get_sent_green_blue) %>% 
  set_names(sent_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(sent_green_blue_mill, location = "Tummalapalle", 
           index = "Green/Blue",
           source = "sentinel2")
plot_annotated(sent_green_blue_mill, tummalapalle_water_sf, 2, 3,
               title = "Green/Blue", source = "sentinel2")

sent_green_blue_tailings <- sent_indices_tailings %>% 
  map(get_sent_green_blue) %>% 
  set_names(sent_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(sent_green_blue_tailings, location = "Tummalapalle", 
           index = "Green/Blue",
           source = "sentinel2")
plot_annotated(sent_green_blue_tailings, tummalapalle_water_sf, 2, 3,
               title = "Green/Blue", source = "sentinel2")


#! planet ================================================================================
#_ vars ==================================================================================
tummalapalle_planet_crs <- "+proj=utm +zone=44 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

tummalapalle_poly_sf <- "raw_data/in/tummalapalle/tummalapalle.kml" %>% 
  read_sf() %>% 
  st_zm() %>% 
  st_transform(tummalapalle_planet_crs) %>% 
  st_buffer(set_units(1, km))


tummalapalle_poly_sf <- "raw_data/in/tummalapalle/tummalapalle.kml" %>% 
  read_sf() %>% 
  st_zm() %>% 
  st_transform(tummalapalle_planet_crs) # %>% 
  # st_buffer(set_units(2, km))

tummalapalle_vectors_sf <- read_rds("data/tummalapalle_vectors.rds") %>% 
  st_transform(tummalapalle_planet_crs)

tummalapalle_vectors_lines_sf <- tummalapalle_vectors_sf %>% 
  st_cast("LINESTRING")

tummalapalle_mill_area_sf <- tummalapalle_vectors_sf %>% 
  filter(Name == "tumm_mill_area")

tummalapalle_tailings_sf <- tummalapalle_vectors_sf %>% 
  filter(Name == "tailings")

tummalapalle_water_sf <- tummalapalle_vectors_sf %>% 
  filter(macro == "water") %>% 
  st_cast("LINESTRING")

tummalapalle_planet_dir <- "imagery/in/tummalapalle/planet"

planet_dates <- dir(tummalapalle_planet_dir, full.names = TRUE, recursive = TRUE) %>% 
  str_subset("SR\\.tif$") %>% 
  str_extract("\\d{8}") %>% 
  unique()

new_planet_dates <- planet_dates %>% 
  .[c(1, c(3:length(planet_dates)))]

#_ build brick list ======================================================================
pre_planet_bricks <- planet_dates %>% 
  map(~ build_planet_bricks(.x, tummalapalle_planet_dir,
                            tummalapalle_poly_sf, tummalapalle_planet_crs)) %>% 
  unlist() %>% 
  .[c(1, c(3:length(planet_dates)))]

fasterized <- fasterize(tummalapalle_poly_sf, pre_planet_bricks[[1]])

pre_planet_bricks %>% map(res)

planet_bricks <- pre_planet_bricks %>% 
  map(resample, fasterized)

planet_bricks %>% 
  walk(~ plotRGB(.x, r = 3, g = 2, b = 1, stretch = "lin"))

planet_bricks_mill <- planet_bricks %>% 
  map(sf_crop, tummalapalle_mill_area_sf)

planet_bricks_tailings <- planet_bricks %>% 
  map(sf_crop, tummalapalle_tailings_sf)

par(oma = c(2, 0, 2, 0))
plotRGB(planet_bricks[[4]], r = 3, g = 2, b = 1, stretch = "lin")
title(main = "Tummalapalle", outer = TRUE)
mtext(text = "Planet", side = 1, outer = TRUE)
plot(tummalapalle_vectors_lines_sf$geometry, lwd = 2, col = "red", add = TRUE)
plotRGB(planet_bricks_mill[[4]], r = 3, g = 2, b = 1, stretch = "lin")
plot(tummalapalle_water_sf$geometry, lwd = 1, col = "red", add = TRUE)
title(main = "Tummalapalle (Mine/Mill Area)", outer = TRUE)
mtext(text = "Planet", side = 1, outer = TRUE)
plotRGB(planet_bricks_tailings[[4]], r = 3, g = 2, b = 1, stretch = "lin")
plot(tummalapalle_vectors_lines_sf$geometry, lwd = 1, col = "red", add = TRUE)
title(main = "Tummalapalle (Tailings Pond)", outer = TRUE)
mtext(text = "Planet", side = 1, outer = TRUE)
par(oma = c(0, 0, 0, 0))

#_ indices ==============================================================================
planet_indices <- planet_bricks %>% 
  map(~ spectralIndices(.x, blue = 1, green = 2, red = 3,
                        nir = 4)) %>% 
  set_names(new_planet_dates) %>% 
  .[sort(names(.))]

planet_indices %>% 
  walk2(sort(new_planet_dates),
        ~ plot_indices(.x, vector_sf = tummalapalle_water_sf, nrow = 3, ncol = 5,
                       title = .y, source = "planet"))

planet_indices_mill <- planet_bricks_mill %>% 
  map(~ spectralIndices(.x, blue = 1, green = 2, red = 3,
                        nir = 4)) %>% 
  set_names(new_planet_dates) %>% 
  .[sort(names(.))]

planet_indices_mill %>% 
  walk2(sort(new_planet_dates),
        ~ plot_indices(.x, vector_sf = tummalapalle_water_sf,  nrow = 3, ncol = 5,
                       title = .y, source = "planet"))

planet_indices_tailings <- planet_bricks_tailings %>% 
  map(~ spectralIndices(.x, blue = 1, green = 2, red = 3,
                        nir = 4)) %>% 
  set_names(new_planet_dates) %>% 
  .[sort(names(.))]

planet_indices_tailings %>% 
  walk2(sort(new_planet_dates),
        ~ plot_indices(.x, vector_sf = tummalapalle_water_sf, nrow = 3, ncol = 5,
                       title = .y, source = "planet"))

#___ NDVI ================================================================================
planet_ndvi <- planet_indices %>% 
  map(~ .x$NDVI) %>% 
  set_names(new_planet_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(planet_ndvi, location = "Tummalapalle", 
           index = "NDVI",
           source = "planet")
plot_annotated(planet_ndvi, tummalapalle_water_sf, 2, 3,
               title = "NDVI", source = "planet")

planet_ndvi_mill <- planet_indices_mill %>% 
  map(~ .x$NDVI) %>% 
  set_names(new_planet_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(planet_ndvi_mill, location = "Tummalapalle (Mine/Mill Area)", 
           index = "NDVI",
           source = "planet")
plot_annotated(planet_ndvi_mill, tummalapalle_water_sf, 2, 3,
               title = "NDVI", source = "planet")

planet_ndvi_tailings <- planet_indices_tailings %>% 
  map(~ .x$NDVI) %>% 
  set_names(new_planet_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(planet_ndvi_tailings, location = "Tummalapalle (Tailings Pond)", 
           index = "NDVI",
           source = "planet")
plot_annotated(planet_ndvi_tailings, tummalapalle_water_sf, 2, 3,
               title = "NDVI", source = "planet")

#___ NDWI ================================================================================
planet_ndwi <- planet_indices %>% 
  map(~ .x$NDWI) %>% 
  set_names(new_planet_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(planet_ndwi, location = "Tummalapalle", 
           index = "NDWI",
           source = "planet")
plot_annotated(planet_ndwi, tummalapalle_water_sf, 2, 3,
               title = "NDWI", source = "planet")


planet_ndwi_mill <- planet_indices_mill %>% 
  map(~ .x$NDWI) %>% 
  set_names(new_planet_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(planet_ndwi_mill, location = "Tummalapalle (Mine/Mill Area)", 
           index = "NDWI",
           source = "planet")
plot_annotated(planet_ndwi_mill, tummalapalle_water_sf, 2, 3,
               title = "NDWI", source = "planet")

planet_ndwi_tailings <- planet_indices_tailings %>% 
  map(~ .x$NDWI) %>% 
  set_names(new_planet_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(planet_ndwi_tailings, location = "Tummalapalle (Tailings Pond)", 
           index = "NDWI",
           source = "planet")
plot_annotated(planet_ndwi_tailings, tummalapalle_water_sf, 2, 3,
               title = "NDWI", source = "planet")
  
#___ green/blue ==========================================================================
get_planet_green_blue <- function(raster_brick){
  green_band <- raster_brick[[2]]
  blue_band <- raster_brick[[1]]
  green_band / blue_band
}

planet_green_blue <- planet_bricks %>% 
  map(get_planet_green_blue) %>% 
  set_names(new_planet_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(planet_green_blue, location = "Tummalapalle", 
           index = "Green/Blue",
           source = "planet")
plot_annotated(planet_green_blue, tummalapalle_water_sf, 2, 3,
               title = "Green/Blue", source = "planet")

planet_green_blue_mill <- planet_indices_mill %>% 
  map(get_planet_green_blue) %>% 
  set_names(new_planet_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(planet_green_blue_mill, location = "Tummalapalle (Mine/Mill Area)", 
           index = "Green/Blue",
           source = "planet")
plot_annotated(planet_green_blue_mill, tummalapalle_water_sf, 2, 3,
               title = "Green/Blue", source = "planet")

planet_green_blue_tailings <- planet_indices_tailings %>% 
  map(get_planet_green_blue) %>% 
  set_names(new_planet_dates) %>% 
  .[sort(names(.))] %>% 
  brick()

level_plot(planet_green_blue_tailings, location = "Tummalapalle (Tailings Pond)", 
           index = "Green/Blue",
           source = "planet")
plot_annotated(planet_green_blue_tailings, tummalapalle_water_sf, 2, 3,
               title = "Green/Blue", source = "planet")
