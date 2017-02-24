library(leaflet)

groupColours <- colorFactor(palette = "Set1", city_coord$cluster)

groups = as.character(unique(city_coord$cluster))

map = leaflet(city_coord) %>% addProviderTiles("CartoDB.DarkMatterNoLabels")

for (i in groups) {
  d = city_coord[city_coord$cluster == i, ]
  map = map %>% addCircleMarkers(data = d, lng = ~longitude, lat = ~latitude, radius = 1, weight = 2, opacity = 0.2,
                                popup = ~as.character(i),  color = ~groupColours(cluster), group = i)
}

# map %>% addLayersControl(overlayGroups = groups)
map




testing_variable <- data.frame(lat = city_coord$latitude, lng = city_coord$longitude )

mapqk <- testing_variable %>% leaflet() %>%
  addProviderTiles("CartoDB.DarkMatterNoLabels") %>%
  addCircleMarkers(clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = TRUE), radius = 0.5)
  



mapqk






