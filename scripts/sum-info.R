# Summary Info
library(dplyr)

get_summary_info <- function(data_set) {
  artist_1 <- data_set %>%
    filter(Number == 1) %>%
    pull(Artist)
  album_1 <- data_set %>%
    filter(Number == 1) %>%
    pull(Album)
  artist_most_num <- data_set %>%
    group_by(Artist) %>%
    summarize(num_albums = n()) %>%
    filter(num_albums == max(num_albums)) %>%
    pull(Artist)
  most_recent_album <- data_set %>%
    filter(Year == max(Year)) %>%
    pull(Album)
  year_with_most <- data_set %>%
    group_by(Year) %>%
    summarize(num_albums = n()) %>%
    filter(num_albums == max(num_albums)) %>%
    pull(Year)
 return(list(art_1 = artist_1, alb_1 = album_1,
             art_most_num = artist_most_num, most_recent = most_recent_album,
             most_year = year_with_most)) 
}