library(raster)
library(rasterVis)
library(sf)
library(tidyverse)
library(units)
library(RStoolbox)
library(RColorBrewer)
library(mapview)
library(fasterize)

# source("R/prep_site_vectors.R")
source("R/plot_planet_rgb.R")
source("R/get_indices.R")
source("R/sf_crop.R")
source("R/planet_utils.R")
source("R/sentinel_utils.R")

cust_pal <- RColorBrewer::brewer.pal(11, "Spectral") %>% 
  rev() %>% 
  {colorRampPalette(.)(255)}

cust_theme <- rasterTheme(region = rev(brewer.pal(11, "Spectral")))
# cust_theme <- rasterTheme(region = cust_pal)


mapviewOptions(basemaps = c("Esri.WorldImagery", "OpenStreetMap"),
               raster.size = 10^15, mapview.maxpixels = 10^15,
               raster.palette = colorRampPalette(cust_pal))

level_plot <- function(raster, location, 
                       band_name = NULL, index = NULL,
                       source, ...){
  if(source == "sentinel2"){
    source <- "Sentinel 2"
    if(!is.null(band_name)){
      band_name <- enquo(band_name)
      filtered_band_info <- sentinel2_band_info %>% 
        filter(band_name == !!band_name) 
      
      band_wavelength <- filtered_band_info  %>% 
        pull(central_wavelength_nm)
      band_purpose <- filtered_band_info  %>% 
        pull(purpose)
      
      title <- paste0(location, ", ", band_purpose, " (", band_wavelength, " nm)")
    } else title <- location
  }
  if(source == "planet") source <- "Planet"
  if(source == "hyperion") source <- "Hyperion"
  
  if(!is.null(index)){
    title <- paste0(location, ", ", index)
  }
  
  levelplot(raster,
            par.settings = cust_theme, 
            scales = list(x = list(at = NULL),
                          y = list(at = NULL)),
            colorkey = list(space="bottom"),
            sub = source,
            main = title,
            ...)
}

plot_annotated <- function(raster_brick, vector_sf,
                           nrow, ncol,
                           lwd = 2, vector_color = "white",
                           title = NULL,
                           source){
  if(source == "sentinel2") source <- "Sentinel 2"
  if(source == "planet") source <- "Planet"
  if(source == "hyperion") source <- "Hyperion"
  
  plotter <- function(raster_layer, vector_sf){
    image(raster_layer,
          col = cust_pal, 
          ann = FALSE,
          axes = FALSE)
    title(main = names(raster_layer))
    plot(vector_sf$geometry, col = vector_color, lwd = lwd,
         axes = FALSE,
         add = TRUE)
  }
  
  raster_list <- unstack(raster_brick)
  
  par(mfrow = c(nrow, ncol))
  par(oma = c(2, 0, 2, 0))
  raster_list %>% 
    walk(plotter, vector_sf)
  title(main = title, outer = TRUE)
  mtext(text = source, side = 1, outer = TRUE)
  par(mfrow = c(1, 1))
  par(oma = c(0, 0, 0, 0))
}

plot_indices <- function(indices_brick, vector_sf, 
                         title = "Indices", 
                         lwd = 1,
                         nrow, ncol,
                         vector_color = "white",
                         source){
  if(source == "sentinel2") source <- "Sentinel 2"
  if(source == "planet") source <- "Planet"
  if(source == "hyperion") source <- "Hyperion"
  
  plotter <- function(raster_layer, vector_sf){
    image(raster_layer,
          col = cust_pal, 
          ann = FALSE,
          axes = FALSE)
    title(main = names(raster_layer))
    plot(vector_sf$geometry, col = vector_color, lwd = lwd,
         axes = FALSE,
         add = TRUE)
  }
  
  raster_list <- unstack(indices_brick)
    
  par(mfrow = c(nrow, ncol))
  par(oma = c(2, 0, 2, 0))
  raster_list %>% 
    walk(plotter, vector_sf)
  title(main = title, outer = TRUE)
  mtext(text = source, side = 1, outer = TRUE)
  par(mfrow = c(1, 1))
  par(oma = c(0, 0, 0, 0))
}

cust_kmeans <- function(raster){
  unsuperClass(img = raster, nIter = 10000000, nClasses = 10)
}
