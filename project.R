
# Load libraries
library(jsonlite)
library(ggplot2)
library(dplyr)
library(dbscan)
library(leaflet)

# load data only if it does'nt exist
if (!exists("df_total")) { # start of loading data
  # Get all of directories in the data folder
  pic_path <- list.dirs(path = "./DATA")
  
  
  pic_path <- pic_path[-1]
  
  # Get specific variables only
  impTags <-  c("longitude", "latitude", "tags")
  
  # create an empty dataframe to add data read from JSON files
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
} # end of loading data

# Cleaning the data
# Convert coordinates to numeric, this is required for the algorithm to compute
df_total$longitude <-  as.numeric(as.character(df_total$longitude))
df_total$latitude <-  as.numeric(as.character(df_total$latitude))

# Get all of the coordinates not equal to 0. Coordinates with 0 values are useless
filter_coord <- df_total %>% 
  select(latitude, longitude, tags) %>%
  filter(latitude != 0)

# Run dbscan to cluster the data
# This process will create different clusters when applied to the whole data contains coordinate from around the world
filter_cord_clus <-  dbscan(select(filter_coord, latitude, longitude), eps = 0.01, minPts = 15)

# Assign cluster values as a category variable to filter coord dataframe to use in order to determine the most frequent cluster
filter_coord <- mutate(filter_coord, category = filter_cord_clus$cluster)

# Find the most frequent cluster value. This way I will separate the highest value from the rest
#  of the coordinates based on cluster/category to determine the city or location to focus on (London in my case)
freq_coord <- table(filter_coord$category)
freq_coord <- names(freq_coord)[which.max(freq_coord)]

#  Filter the data based on most frequent cluster/category. This is to find out where most of the coordinates are coming from
city_coord <- filter(filter_coord, category == freq_coord)

#  Get all of the noise values within the city (london) coordinates region. The algorithm will assign noise to points
#  that dont have any neighbour points around within 0.01 radius minimum 15 points atleast. To have all of the points lies 
#  within the given city, I have included noise points that lies between city coordinates so I dont lose any extra coordinates.
noise_coord <- filter_coord %>% 
  filter(category == 0 & (latitude <= max(city_coord$latitude)) & (latitude >= min(city_coord$latitude)) &
        (longitude <= max(city_coord$longitude)) & (longitude >= min(city_coord$longitude)) 
        )

# Merge noise dataframe with city_coord dataframe
city_coord <- rbind(city_coord, noise_coord)

# dbscan algorithm is runned again on the city_coord dataframe to look for more clusters. This time the input values differ
#  the radius is reduced further as mainly focused to find point of interest with atleast 20 neighbours within 0.0009 raidus
find_poi <- select(city_coord, longitude, latitude)
lon_filter <- dbscan(find_poi, eps = 0.0009, minPts = 20)
city_coord$cluster <- lon_filter$cluster # create a new cluster variable in city_coord to assign clusters for each coord 

# this is to determine the most visited places in the city based on cluster points frequency 
most_visited <- city_coord %>% filter(cluster != 0)
noise_coord <- city_coord %>% filter(cluster == 0)


# To check frequency of clusters
cluster_freq <- as.data.frame(table(most_visited$cluster))
names(cluster_freq) <- c("numbers", "times")
cluster_freq$rank <- rank(-cluster_freq$times, ties.method = "max")
cluster_freq <- cluster_freq[order(cluster_freq$rank, decreasing = FALSE),]

# Keep frequency higher or equal to 30
cluster_freq <- cluster_freq[cluster_freq$times >= 30,]

# Keep clusters in a category column appeared more than 30 times 
city_coord$category <-  unlist(lapply(city_coord$cluster, function(x) {
  for (i in 1:length(cluster_freq$numbers)) {
    if (x != 0 & x == cluster_freq$numbers[[i]]) {
      return(x)
    } else if ((i + 1) > length(cluster_freq$numbers)) {
      return(0)
    }
  }
}))

# create group of colours in order to highlight different clusters on the map
groupColours <- colorFactor(palette = "Set1", most_visited$cluster)
groups = as.character(unique(most_visited$cluster))

# create a leaflet map
map = leaflet()
  # addProviderTiles("CartoDB.DarkMatterNoLabels")

# assign a different color to each point to visualise it better 
for (i in groups) {
  d = most_visited[most_visited$cluster == i, ]
  map = map %>% addCircleMarkers(data = d, lng = ~longitude, lat = ~latitude, radius = 1.3, weight = 1, opacity = 0.2,
                                 popup = ~as.character(tags),  color = ~groupColours(cluster), group = i)
}


# map <-  map %>% addPolygons(data = convex_region[border_points, ], lng = ~longitude, lat = ~latitude, weight = 1, color = ~groupColours(16))
# map <-  map %>% addPolygons(data = convex_region_t[border_points_t, ], lng = ~longitude, lat = ~latitude, weight = 1, color = ~groupColours(2))
# map %>% addLayersControl(overlayGroups = groups)
map %>% addCircleMarkers(data = noise_coord, lng = ~longitude, lat = ~latitude, radius = 1, weight = 1, opacity = 0.2, color = "grey" )
# map


# Plot data
ggplot(city_coord, aes(x = longitude, y = latitude)) +
  geom_point(aes(fill = "grey"), data = noise) +
  geom_polygon(data = convex_region$ashape.obj)
  coord_map()



 

 




