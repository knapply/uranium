get_indices <- function(brick, ...){
  spectralIndices(brick,
                  blue = 1, green = 2, red = 3,
                  nir = 4, ...)
}