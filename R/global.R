library(raster)
library(sf)
library(tidyverse)
library(units)

source("R/prep_site.R")
source("R/plot_planet_rgb.R")
source("R/get_indices.R")
source("R/sf_crop.R")

cust_pal <- colorRampPalette(rev(brewer.pal(11, "Spectral")))(255)