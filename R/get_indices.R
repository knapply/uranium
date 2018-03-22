get_indices <- function(brick, data_source, ...){
  if(data_source == "planet"){
    blue_band <- 1
    green_band <- 2
    red_band <- 3
    nir_band <- 4
  
  indices <- spectralIndices(brick,
                  blue = blue_band, 
                  green = green_band,
                  red = red_band,
                  nir = nir_band,
                  ...)
  }
  
  if(data_source == "sentinel 2"){
    blue_band <- 2
    green_band <- 3
    red_band <- 3
    nir_band <- 4
  
  spectralIndices(brick,
                  blue = blue_band, 
                  green = green_band,
                  red = red_band,
                  nir = nir_band,
                  ...)
  }
}
