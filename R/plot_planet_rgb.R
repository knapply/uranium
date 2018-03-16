plot_planet_rgb <- function(rast_brick, red = 3, green = 2, blue = 1, stretch = "lin", ...){
  plotRGB(rast_brick, r = red, g = green, b = blue, tck = 0,
          stretch = stretch, ...)
}