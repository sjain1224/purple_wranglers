# Graph 2: Violin plot of # of Albums by year 

library(dplyr)
library(ggplot2)

plot2 <- function(dataset) {
  artist_data <- dataset %>%
    mutate("decade" = floor(Year / 10) * 10) 
  artist_data$decade <- as.factor(artist_data$decade)
  plot_ly(x = ~artist_data$decade, y = ~artist_data$Number,
    split = ~artist_data$decade, type = 'violin',
    box = list(visible = T), meanline = list(visible = T)) %>% 
    layout(title = "Album Ranks by Decade",
      xaxis = list(title = "Rank"),
      yaxis = list(title = "Decade", zeroline = F)) 
}