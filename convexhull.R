.libPaths("library")

library(alphahull)
library(dplyr)
library(igraph)

# Find boundary points for each of category clusters higher than 0 (0 counts as noise)
category_cluster <- city_coord %>% filter(category > 0) %>% select(category) %>% unique()
category_cluster <- as.vector(category_cluster$category)
for (i in 1:length(category_cluster)) {
  
  convex_region_t <- data.frame()
  convex_region_t <- city_coord %>% filter(cluster == category_cluster[i]) %>%  select(latitude, longitude) 
  convex_region_t <- unique(convex_region_t[, c('latitude', 'longitude')])
  
  get_region_t <- chull(convex_region_t$latitude, convex_region_t$longitude)
  border_points_t <- c(get_region_t, get_region_t[1])
  
  map <-  map %>% addPolygons(data = convex_region_t[border_points_t, ], lng = ~longitude, lat = ~latitude, weight = 1, color = ~groupColours(i))
  
}





# convex_region <- ashape(convex_region$latitude, convex_region$longitude, 1)

get_region_t <- chull(convex_region_t$latitude, convex_region_t$longitude)
border_points_t <- c(get_region_t, get_region_t[1])
plot(convex_region, cex - 0.5)
lines(get_region[border_points, ])

ggplot(convex_region, aes(x = longitude, y = latitude)) + 
  geom_polygon(data = convex_region[border_points, ], col = "red") + geom_point()
coord_map() + theme_minimal()

                          