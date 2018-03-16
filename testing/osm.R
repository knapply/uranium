library(rvest)
library(osmdata)

jharkhand_bbox <- read_sf("raw_data/in/jharkhand.kml") %>% 
  st_zm() %>% 
  st_transform(4326) %>% 
  st_bbox()

features_url <- "https://wiki.openstreetmap.org/wiki/Map_Features#Highway"
macro_feature_css <- ".mw-headline"

macro_features <- features_url %>% 
  read_html() %>% 
  html_nodes(macro_feature_css) %>% 
  html_text() %>% 
  str_to_lower() %>% 
  unique()

priority_features <- c(
  "railways", "other railways", "military", "man_made",
  "landuse", "highway", "geological", "power", "highway"
  )

line_features <- c(
  "railways", "other railways", "highway", "public transport",
  "tracks"
)

build_feature_sf <- function(macro_feature, bbox){
  q_0 <- opq(bbox = unlist(bbox))
  q_1 <- add_osm_feature(q_0, 
                       key = macro_feature)
  r_0 <-osmdata_sf(q_1)
}

jaduguda_buff <- "raw_data/in/jaduguda/effluent_1.kml" %>% 
  read_sf() %>% 
  st_transform(3857) %>% 
  st_buffer(set_units(3, km)) %>% 
  st_transform(4326) 

jaduguda_bbox <- jaduguda_buff %>% 
  st_bbox()

features_found <- macro_features %>% 
  map(~ build_feature_sf(.x, jaduguda_bbox))


test <- features_found %>% 
  pluck("osm_lines") %>% 
  reduce(rbind)
plot(test$geometry)

jaduguda_lines <- features_found %>% 
  pluck("osm_lines") %>% 
  map(as_tibble) %>% 
  map(mutate_if, is.factor, as.character) %>% 
  discard(~ nrow(.x) == 0)

lines_geom <- jaduguda_lines %>% 
  map(select, geometry) %>% 
  reduce(rbind)

lines_sf <- jaduguda_lines %>% 
  map(select, -geometry) %>% 
  reduce(bind_rows) %>% 
  mutate(geometry = lines_geom$geometry) %>% 
  st_as_sf()
%>% 
  st_join(jaduguda_buff, st_touches) %>% 
  drop_na()



jaduguda_points <- features_found %>% 
  pluck("osm_points") %>% 
  reduce(rbind)

jaduguda_polys <- features_found %>% 
  pluck("osm_polygons") %>% 
  reduce(rbind)



