# Graph 2: Violin plot of # of Albums by year 

library(dplyr)
library(ggplot2)

plot2 <- function(dataset) {
  year_data<- dataset %>%
    group_by(Year) %>%
    summarise(count = n())
  ggplot(year_data, aes(x = year_data$Year, y = year_data$count)) + 
    geom_violin(fill = "green") + 
    labs(title = "Number of Albums Released per Year",
         x = "Year", y = "Number of Albums") 
}