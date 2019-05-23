# Bar chart - Genre count 
library(R.utils)
library(dplyr)
library(data.table)
library(ggplot2)

plot1 <- function(dataset) {
  genre_count <- dataset %>%
    group_by(Genre) %>%
    summarise(count = n()) %>%
    top_n(10)
  ggplot(data = genre_count, aes(genre_count$Genre, genre_count$count, 
                                 fill = genre_count$Genre)) + 
    geom_bar(colour = "black", stat = "identity") +
    labs(title = "Top 10 Album Genres", x = "Genre", y = "Number of Albums") +
    coord_flip() + theme(legend.position="none")
}