
library(ggplot2)
library(cluster)
library(dbscan)
library(tidyr)

# Exploring data with K-means
# --------------------------
london_kmeans <- kmeans(filter_coord[, 1:2], 10)
# Plot on a plot
london_kmeans$cluster <- as.factor(london_kmeans$cluster)
ggplot(filter_coord[, 1:2], aes(longitude, latitude, color = london_kmeans$cluster)) + geom_point()


# Exploring data with Hierarchical Agglomerative method
# -----------------------------------------------------
lon_df_agg <-  filter_coord[1:50,]
london_hie_agg <- hclust(dist(lon_df_agg[, 1:2]))
plot(london_hie_agg)

# Exploring data with Hierarchical Divisive method
# -----------------------------------------------------
lon_df_div <-  filter_coord[1:100,]
london_hie_div <- diana(lon_df_div)
plot(london_hie_div)


# Exploring data using DBSCAN method # 1
# -----------------------------------------------------
london_dbscan <- dbscan(most_visited[, 1:2], eps = 0.02, minPts = 10)
london_dbscan$cluster <-  as.factor(london_dbscan$cluster)
ggplot(most_visited[, 1:2], aes(longitude, latitude, color = london_dbscan$cluster)) + 
  geom_point(size = 1, alpha = .5) +
  coord_map() + theme(legend.position = "none")

# Exploring data using CLARANS method
# -----------------------------------------------------

london_claran <- clara(most_visited[, 1:2], 9)
london_claran$cluster <- as.factor(london_claran$clustering)

ggplot(most_visited[, 1:2], aes(longitude, latitude, color = london_claran$cluster)) + 
  geom_point(size = 1, alpha = .5) +
  coord_map() + theme(legend.position = "none")


# Exploring data using DBSCAN method # 2
# -----------------------------------------------------
london_dbscan <- dbscan(most_visited[, 1:2], eps = 0.0002, minPts = 3)
london_dbscan$cluster <-  as.factor(london_dbscan$cluster)
ggplot(most_visited[, 1:2], aes(longitude, latitude, color = london_dbscan$cluster)) + 
  geom_point(size = .2) +
  coord_map() + theme(legend.position = "none")


# Comparison of DBSCAN with CLARANS 1
dbscan_time <- system.time(dbscan(most_visited[, 1:2], eps = 0.0009, minPts = 5))
clarans_time <- system.time(clara(most_visited[, 1:2], 79))


# Comparison of DBSCAN with CLARANS 2
dbscan_time <- system.time(dbscan(most_visited[, 1:2], eps = 0.0002, minPts = 3))
dbscan_time

clarans_time <- system.time(clara(most_visited[, 1:2], 410))
clarans_time

# Plot test information
time_df <- data.frame(
  tests = c(1, 2),
  dbscan = c(0.15, 0.61),
  clarans = c(1.29, 75.44)
)

time_df %>% gather(key, value, dbscan, clarans) %>%
  ggplot(aes(x = tests, y = value, color = key)) + geom_line(size = 2)

