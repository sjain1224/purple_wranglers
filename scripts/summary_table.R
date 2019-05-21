# Midpoint Deliverable - Summary Table

# Import the "dplyr" library
library("dplyr")

# Create a table of summary information, with columns containing each decade,
# the total number of albums produced during it, it's average song rank, and
# its most popular artist(s).
summary_table <- function(df) {
  table <- df %>%
    mutate("decade" = floor(Year / 10)*10) %>% 
    group_by(decade) %>% 
    add_count(decade, name = "total_albums") %>% 
    mutate("average_song_rank" = round(mean(Number, na.rm = T),2)) %>% 
    group_by(decade, Artist) %>% 
    add_count(Artist, name = "appearances") %>%
    group_by(decade) %>% 
    filter(appearances == max(appearances, na.rm = T)) %>% 
    distinct(decade, total_albums, Artist, average_song_rank, appearances) %>% 
    mutate(most_popular = toString(Artist)) %>% 
    distinct(decade, total_albums, average_song_rank, most_popular)
  return(table)
}