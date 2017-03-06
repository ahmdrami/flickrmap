.libPaths("library")

library(alphahull)
library(dplyr)
library(igraph)

convex_region_t <- city_coord %>% filter(cluster == 2) %>%  select(latitude, longitude) 

convex_region_t <- unique(convex_region_t[, c('latitude', 'longitude')])

# convex_region <- ashape(convex_region$latitude, convex_region$longitude, 1)

get_region_t <- chull(convex_region_t$latitude, convex_region_t$longitude)
border_points_t <- c(get_region_t, get_region_t[1])
plot(convex_region, cex - 0.5)
lines(get_region[border_points, ])

ggplot(convex_region, aes(x = longitude, y = latitude)) + 
  geom_polygon(data = convex_region[border_points, ], col = "red") + geom_point()
coord_map() + theme_minimal()

                          