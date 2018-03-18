library(raster)
library(sf)
library(tidyverse)
library(units)
library(RColorBrewer)

# source("R/prep_site_vectors.R")
source("R/plot_planet_rgb.R")
source("R/get_indices.R")
source("R/sf_crop.R")
source("R/planet_utils.R")

cust_pal <- RColorBrewer::brewer.pal(11, "Spectral") %>% 
  rev() %>% 
  {colorRampPalette(.)(255)}
