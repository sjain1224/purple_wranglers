# Summary Info
library(dplyr)

test_df <- read.csv("data/albumlist.csv")
get_summary_info <- function(data_set) {
  artist_1 <- data_set %>%
    filter(Number == 1) %>%
    pull(Artist)
  album_1 <- data_set %>%
    filter(Number == 1) %>%
    pull(Album)
  artist_most_num <- data_set %>%
    group_by(artist) %>%
    mutate(num_albums = n()) %>%
    filter(num_albums == max(num_albums)) %>%
    pull(artist)
  most_recent_album <- data_set %>%
    filter(Year == max(Year)) %>%
    pull(Album)
  year_with_most <- data_set %>%
    group_by(Year) %>%
    mutate(num_albums = n()) %>%
    filter(num_albums == max(num_albums)) %>%
    pull(Year)
  
}

