# Graph: Timeline of Rolling Stone's 500 Greatest Albums
library(dplyr)
library(plotly)

plot3 <- function(data_set) {
  year_data <- data_set %>%
    group_by(Year) %>%
    summarise(count = n())
    plot_ly(year_data, x = ~year_data$Year, y = ~year_data$count,
          marker = list(size = 10, color = 'rgba(255, 182, 193, .9)',
                        line = list(color = 'rgba(152, 0, 0, .8)',
                                    width = 2)), type = 'scatter', 
          mode = 'markers') %>%
    layout(title = 'Albums Released per Year',
           yaxis = list(title = 'Albums', zeroline = FALSE),
           xaxis = list(title = 'Year', zeroline = FALSE))
  }