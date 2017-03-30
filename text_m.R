.libPaths("library")
library("tm")
library("wordcloud2")
library("dplyr")

clean_corpus <- function(c) {

  c <- tm_map(c, removePunctuation)
  c <- tm_map(c, tolower)
  c <- tm_map(c, removeWords, c(stopwords("en"), Top100Words, Top200Words, "nikon"))
  c <- tm_map(c, stripWhitespace)
  c <- tm_map(c, PlainTextDocument)
  return(c)
}

# Load and clean London Point of Interest database. This will be used to keep tags that are valuable
london_poi <- read.csv("london-attraction.csv")
london_poi <- removeNumbers(as.vector(london_poi$lng))
london_poi <- removePunctuation(london_poi)
london_poi <- london_poi[london_poi != ""]
london_poi <- london_poi[nchar(london_poi) > 1]
london_poi <- tolower(london_poi)
london_poi <- stripWhitespace(london_poi)

custom_stopwords <- c("london", "england", "uk")
clean_corpus <- function(c) {
  
  c <- tm_map(c, removePunctuation)
  c <- tm_map(c, tolower)
  c <- tm_map(c, removeWords, c(stopwords("en"), Top100Words, Top200Words, custom_stopwords))
  c <- tm_map(c, stripWhitespace)
  c <- tm_map(c, PlainTextDocument)
  return(c)
}

specific_cluster <- city_coord %>% filter(category == 66 & (tags != "" )) %>% select(tags)
specclus_vecsrc <- DataframeSource(specific_cluster)
specclus_corps <- VCorpus(specclus_vecsrc)
clean_tag <- clean_corpus(specclus_corps)

# specclus_dtm <- DocumentTermMatrix(clean_tag)
specclus_tdm <- TermDocumentMatrix(clean_tag)

# m_dtm <- as.matrix(specclus_dtm)
m_tdm <- as.matrix(specclus_tdm)

term_freq <- rowSums(m_tdm)
term_freq <- sort(term_freq, decreasing = TRUE)
tag_freq <- data.frame(term = names(term_freq[1:100]) , num = term_freq[1:100])
# wordcloud2(tag_freq, size = 1)


tag_freq_vector <- as.vector(names(term_freq[1:100]))
for (i in 1:length(tag_freq_vector)) {
  if (length(which(grepl((tag_freq_vector[i]), london_poi, fixed = TRUE))) < 1 ) {
    tag_freq_vector[i] <- FALSE
  } 
  
}
tag_freq_vector



clean_tag[[28]][1]
specific_cluster[[1]][28]

max_north <- max(specific_cluster$latitude)
max_east <- max(specific_cluster$longitude)
min_south <- min(specific_cluster$latitude)
min_west <- min(specific_cluster$longitude)

max_north
max_east
min_south
min_west

api <- osmsource_api()
# map_boundary <- corner_bbox(-0.1, 51.59, 0.0, 51.6)
map_boundary <- corner_bbox(min_west, min_south, max_east, max_north)
testosm <- get_osm(map_boundary, source = api, way(tags(k == "name")))




