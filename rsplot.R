# Rolling Stones Dataset

library(dplyr)
library(plotly)

# Read in Rolling Stones Dataset
rollingstones <- read.csv("albumlist.csv", stringsAsFactors = FALSE)

displayRSPlot <- function(input) {
  is.na(rollingstones) <- rollingstones==''
  
  # Filter to the selected Genre; all of the albums are condensed to 
  # these 10 core genres
  if (input$genre == "Blues") {
    rollingstones_plot <- rollingstones %>% 
      filter(Genre == "Blues" | Genre == "Blues, Folk, World, & Country")
  } else if (input$genre == "Classical") {
    rollingstones_plot <- rollingstones %>%
      filter(Genre == "Classical, Stage & Screen")
  } else if (input$genre == "Electronic") {
    rollingstones_plot <- rollingstones %>%
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
    rollingstones_plot <- rollingstones %>%
      filter(Genre == "Folk, World, & Country")
  } else if (input$genre == "Funk / Soul") {
    rollingstones_plot <- rollingstones %>%
      filter(Genre == "Funk / Soul" | Genre == "Funk / Soul, Blues" |
             Genre == "Funk / Soul, Folk, World, & Country" |
             Genre == "Funk / Soul, Pop" | 
             Genre == "Funk / Soul, Stage & Screen" |
             Genre == "Funk / Soul,ÊFolk, World, & Country" |
             Genre == "Latin, Funk / Soul")
  } else if (input$genre == "Hip Hop") {
    rollingstones_plot <- rollingstones %>%
      filter(Genre == "Hip Hop" | Genre == "Hip Hop, Funk / Soul" |
               Genre == "Hip Hop, Rock" | Genre == "Hip Hop, Rock, Funk / Soul")
  } else if (input$genre == "Jazz") {
    rollingstones_plot <- rollingstones %>%
      filter(Genre == "Jazz" | Genre == "Jazz, Funk / Soul" |
             Genre == "Jazz, Pop" | Genre == "Jazz, Rock" | 
             Genre == "Jazz, Pop, Folk, World, & Country" |
             Genre == "Jazz, Rock, Blues, Folk, World, & Country" |
             Genre == "Jazz, Rock, Funk / Soul, Blues" |
             Genre == "Jazz, Rock, Funk / Soul, Folk, World, & Country" |
             Genre == "Jazz, Rock, Funk / Soul, Pop, Folk, World, & Country" |
             Genre == "Jazz, Rock, Pop")
  } else if (input$genre == "Pop") {
    rollingstones_plot <- rollingstones %>%
      filter(Genre == "Pop" | Genre == "Pop, Folk, World, & Country")
  } else if (input$genre == "Reggae") {
    rollingstones_plot <- rollingstones %>%
      filter(Genre == "Reggae" | 
             Genre == "Reggae,ÊPop,ÊFolk, World, & Country,ÊStage & Screen")
  } else if (input$genre == "Rock") {
    rollingstones_plot <- rollingstones %>%
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
    rollingstones_plot <- rollingstones
  }
  
  # Creates a Scatterplot based on a Genre and the albums of that 
  # Genre in a certain Decade
  rs_plot <- plot_ly(rollingstones_plot, x = ~Year, y = ~Number, 
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
  suppressWarnings(print(rs_plot))
}