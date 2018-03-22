# png()
# par(oma = c(2, 0, 2, 0))
# # raster_list %>% 
#   # walk(plotter, vector_sf)
# # animate(hyperion_brick, col = cust_pal)
# image(hyperion_brick[[1]], ann = FALSE,
#       axes = FALSE, col = cust_pal)
# title(main = "So. Many. Bands", outer = TRUE)
# mtext(text = "Hyperion Data Products Site http://eo1.usgs.gov/sensors/hyperion", side = 1, outer = TRUE)
# par(mfrow = c(1, 1))
# par(oma = c(0, 0, 0, 0))


# dir.create("hyp_tumm")
setwd("hyp_tumm")

filtered <- hyperion_brick %>% 
  unstack() %>% 
  discard(~ identical(min(values(.x)), max(values(.x)))) %>% 
  brick()

# example 1: simple animated countdown from 10 to "GO!".
png(file="hyp_tumm%02d.png", width = 687.5, height = 500)

levelplot(filtered[[1]], #legend = FALSE,
                       par.settings = cust_theme,
                       scales = list(x = list(at = NULL),
                                     y = list(at = NULL)),
                       # scales=list(draw=FALSE),
                       colorkey = FALSE)#list(space="bottom"))

for (i in seq_along(1:nlayers(filtered))){
  message(i)
  plotted <- levelplot(filtered[[i]],
                       par.settings = cust_theme, 
                       scales = list(x = list(at = NULL),
                                     y = list(at = NULL)),
                       colorkey = FALSE)
                       # colorkey = list(space="bottom"))
  print(plotted)
  # title(main = "Beyond Red, Green, and Blue", outer = TRUE)
  # mtext(text = paste("Band", i), side = 1, outer = TRUE)
}
dev.off()


library(magick)
list.files(path = getwd(), pattern = "*.png", full.names = T) %>% 
  gtools::mixedsort() %>% 
  map(image_read) %>%
  image_join() %>%
  image_animate(fps = 10) %>%
  image_write("hyp_tumm.gif") 

# file.remove(list.files(pattern=".png"))
