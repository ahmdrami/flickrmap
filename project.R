
# Load libraries
library(jsonlite)
library(ggplot2)
library(dplyr)
library(maps)
library(dbscan)

# load data 
if (!exists("df_total")) {
  # Get all of directories in the data folder
  pic_path <- list.dirs(path = "./DATA")
  
  print("test");
  pic_path <- pic_path[-1]
  
  # Get specific variables only
  impTags <-  c("longitude", "latitude", "tags")
  
  # create an empty dataframe
  df_total <- data.frame()
  for (x in pic_path) {
    lf <-  list.files(x)
    for (y in lf) {
      add <- paste0(x,"/",y)
      print(add)
      jd <- fromJSON(add, flatten = TRUE)
      jd <- jd$photos$photo[impTags]
      df <- data.frame(jd)
      df_total <- rbind(df_total, df)
    }
  }
}

# Convert coordinates to numeric
df_total$longitude <-  as.numeric(as.character(df_total$longitude))
df_total$latitude <-  as.numeric(as.character(df_total$latitude))

# Get all of the coordinates not equal to 0 
filter_coord <- df_total %>% 
  select(latitude, longitude) %>%
  filter(latitude != 0)

# Run dbscan to cluster the data
filter_cord_clus <-  dbscan(filter_coord, eps = 0.01, minPts = 15)

# Assign cluster variables to filter coord
filter_coord <- mutate(filter_coord, category = filter_cord_clus$cluster)

# Find most frequent points
freq_coord <- table(filter_coord$category)
freq_coord <- names(freq_coord)[which.max(freq_coord)]

#  Filter the data based on most frequent category. This is to find out where most of the coordinates are coming from
city_coord <- filter(filter_coord, category == freq_coord)

#  Get all of the noise values with in the city coordinates region
noise_coord <- filter_coord %>% 
  filter(category == 0 & (latitude <= max(city_coord$latitude)) & (latitude >= min(city_coord$latitude)) &
        (longitude <= max(city_coord$longitude)) & (longitude >= min(city_coord$longitude)) 
        )

# Merge noise values in city range with city data
city_coord <- rbind(city_coord, noise_coord)

# lon_filter <- dbscan(city_coord, eps = 0.0001, minPts = 5)
find_poi <- select(city_coord, longitude, latitude)
lon_filter <- dbscan(find_poi, eps = 0.0009, minPts = 20)
city_coord$cluster <- lon_filter$cluster

most_visited <- city_coord %>% filter(cluster != 0)
noise <- city_coord %>% filter(cluster == 0)

most_visited <- as.data.frame(table(most_visited$cluster))
names(most_visited) <- c("numbers", "times")
most_visited$rank <- rank(-most_visited$times, ties.method = "max")
most_visited <- most_visited[order(most_visited$rank, decreasing = FALSE),]


groupColours <- colorFactor(palette = "Set1", city_coord$cluster)

groups = as.character(unique(city_coord$cluster))

map = leaflet(city_coord) %>% addProviderTiles("CartoDB.DarkMatterNoLabels")

for (i in groups) {
  d = city_coord[city_coord$cluster == i, ]
  map = map %>% addCircleMarkers(data = d, lng = ~longitude, lat = ~latitude, radius = 1.3, weight = 1, opacity = 0.2,
                                 popup = ~as.character(i),  color = ~groupColours(cluster), group = i)
}

# map %>% addLayersControl(overlayGroups = groups)
map
# # Plot data 
# ggplot(city_coord, aes(x = longitude, y = latitude)) + 
#   geom_point(aes(fill = "grey"), data = noise) + 
#   geom_point(aes(colour = cluster), data = most_visited) + 
#   coord_map()




 
city_coord$category <-  unlist(lapply(city_coord$cluster, function(x) {
  for (i in 1:length(most_visited$numbers)) {
    if (x == most_visited$numbers[[i]]) {
      return(most_visited$times[[i]]) 
    } else if ((i + 1) > length(most_visited$numbers)) {
      return(0)
    }
  }
}))
 
city_coord$category <- unlist(lapply(city_coord$category, function(x){
  if (x >= 30) { return(x) } 
  else { return (0) }
  
}))



