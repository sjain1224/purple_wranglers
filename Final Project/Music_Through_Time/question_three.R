# Data Question Three
library(dplyr)
library(shiny)
three_data <- read.csv("../../data/albumlist.csv", stringsAsFactors = F)

displayThreePlot <- function(input) {
  
  # Filter to the selected Genre; all of the albums are condensed to 
  # these 10 core genres
  if (input$genre == "Blues") {
    three_data_plot <- three_data %>% 
      filter(Genre == "Blues" | Genre == "Blues, Folk, World, & Country")
  } else if (input$genre == "Classical") {
    three_data_plot <- three_data %>%
      filter(Genre == "Classical, Stage & Screen")
  } else if (input$genre == "Electronic") {
    three_data_plot <- three_data %>%
      filter(Genre == "Electronic" | Genre == "Electronic, Funk / Soul" |
               Genre == "Electronic, Funk / Soul, Pop" | 
               Genre == "Electronic, Hip Hop, Funk / Soul" |
               Genre == "Electronic, Hip Hop, Funk / Soul, Pop" |
               Genre == "Electronic, Hip Hop, Pop" |
               Genre == "Electronic, Hip Hop, Reggae, Pop" | 
               Genre == "Electronic, Pop" | Genre == "Electronic, Reggae" |
               Genre == "Electronic, Rock" |
               Genre == "Electronic, Rock, Funk / Soul, Blues, Pop" |
               Genre == "Electronic, Rock, Funk / Soul, Pop" |
               Genre == "Electronic, Rock, Funk / Soul, Stage & Screen" |
               Genre == "Electronic, Rock, Pop")
  } else if (input$genre == "Folk") {
    three_data_plot <- three_data %>%
      filter(Genre == "Folk, World, & Country")
  } else if (input$genre == "Funk / Soul") {
    three_data_plot <- three_data %>%
      filter(Genre == "Funk / Soul" | Genre == "Funk / Soul, Blues" |
               Genre == "Funk / Soul, Folk, World, & Country" |
               Genre == "Funk / Soul, Pop" | 
               Genre == "Funk / Soul, Stage & Screen" |
               Genre == "Funk / Soul,ÊFolk, World, & Country" |
               Genre == "Latin, Funk / Soul")
  } else if (input$genre == "Hip Hop") {
    three_data_plot <- three_data %>%
      filter(Genre == "Hip Hop" | Genre == "Hip Hop, Funk / Soul" |
               Genre == "Hip Hop, Rock" | Genre == "Hip Hop, Rock, Funk / Soul")
  } else if (input$genre == "Jazz") {
    three_data_plot <- three_data %>%
      filter(Genre == "Jazz" | Genre == "Jazz, Funk / Soul" |
               Genre == "Jazz, Pop" | Genre == "Jazz, Rock" | 
               Genre == "Jazz, Pop, Folk, World, & Country" |
               Genre == "Jazz, Rock, Blues, Folk, World, & Country" |
               Genre == "Jazz, Rock, Funk / Soul, Blues" |
               Genre == "Jazz, Rock, Funk / Soul, Folk, World, & Country" |
               Genre == "Jazz, Rock, Funk / Soul, Pop, Folk, World, & Country" |
               Genre == "Jazz, Rock, Pop")
  } else if (input$genre == "Pop") {
    three_data_plot <- three_data %>%
      filter(Genre == "Pop" | Genre == "Pop, Folk, World, & Country")
  } else if (input$genre == "Reggae") {
    three_data_plot <- three_data %>%
      filter(Genre == "Reggae" | 
               Genre == "Reggae,ÊPop,ÊFolk, World, & Country,ÊStage & Screen")
  } else if (input$genre == "Rock") {
    three_data_plot <- three_data %>%
      filter(Genre == "Rock" | Genre == "Rock, Blues, Folk, World, & Country" |
               Genre == "Rock, Blues, Pop" | 
               Genre == "Rock, Folk, World, & Country" |
               Genre == "Rock, Funk / Soul" |
               Genre == "Rock, Funk / Soul, Blues" |
               Genre == "Rock, Funk / Soul, Blues, Pop, Folk, World, & Country" |
               Genre == "Rock, Funk / Soul, Folk, World, & Country" |
               Genre == "Rock, Funk / Soul, Pop" |
               Genre == "Rock, Latin" | Genre == "Rock, Latin, Funk / Soul" |
               Genre == "Rock, Pop" | 
               Genre == "Rock, Pop, Folk, World, & Country" |
               Genre == "Rock, Reggae" | Genre == "Rock, Reggae, Latin" |
               Genre == "Rock, Stage & Screen" | Genre == "Rock,ÊBlues" |
               Genre == "Rock,ÊPop")
  } else {
    three_data_plot <- three_data
  }
  
  # Creates a Scatterplot based on a Genre and its albums from
  # 1955 - 2011
  rs_plot <- plot_ly(three_data_plot, x = ~Year, y = ~Number, 
                     type = 'scatter', mode = 'markers',  
                     marker = list(size=10 , opacity=0.5), color = ~Number, 
                     colors  = c("green", "blue"),
                     hoverinfo = 'text', 
                     text = ~paste('Rank:', Number, '<br>Album:', Album,
                                   '<br>Artist:', Artist, '<br>Year:', Year)) %>%
    hide_colorbar() %>%
    layout(title = 'Rolling Stones Top 500 Albums', margin = list(t = "110"), 
           xaxis = list(title = 'Year', showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(title = 'Rank', showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
}