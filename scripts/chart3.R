# Graph: Timeline of Rolling Stone's 500 Greatest Albums
library(dplyr)
library(plotly)

plot3 <- function(data_set) {
  organize_year <- data_set %>%
    group_by(Year) %>%
    summarise(count_year = n())
  plot_ly(organize_year,
          x = ~organize_year$Year,
          y = ~organize_year$count_year,
          type = 'bar',
          name = "Albums") %>%
    layout(title = "Timeline of Rolling Stone's 500 Greatest Albums",
           xaxis = list(title = "Album's Release Year"),
           yaxis = list(title = "Numbers of Albums Released"),
           barmode = 'stack')
}