# Data Question Three
library(plotly)
library(readr)

three_data <- read_csv("../../data/albumlist.csv",
                       local = locale(encoding = "latin1"))
                       
# Breaking down Genres into 10 core genres (Tedious, Extensive)

# Blues
three_data$Genre[three_data$Genre ==
                   "Blues, Folk, World, & Country" ] <- "Blues"

# Classical
three_data$Genre[three_data$Genre ==
                   "Classical, Stage & Screen"] <- "Classical"

# Electronic 
three_data$Genre[three_data$Genre ==
                   "Electronic, Funk / Soul"] <- "Electronic"
three_data$Genre[three_data$Genre ==
                   "Electronic, Funk / Soul, Pop"] <- "Electronic"
three_data$Genre[three_data$Genre ==
                   "Electronic, Hip Hop, Funk / Soul"] <- "Electronic"
three_data$Genre[three_data$Genre ==
                   "Electronic, Hip Hop, Funk / Soul, Pop"] <- "Electronic"
three_data$Genre[three_data$Genre ==
                   "Electronic, Hip Hop, Pop"] <- "Electronic"
three_data$Genre[three_data$Genre ==
                   "Electronic, Hip Hop, Reggae, Pop"] <- "Electronic"
three_data$Genre[three_data$Genre == "Electronic, Pop"] <- "Electronic"
three_data$Genre[three_data$Genre == "Electronic, Reggae"] <- "Electronic"
three_data$Genre[three_data$Genre == "Electronic, Rock"] <- "Electronic"
three_data$Genre[three_data$Genre ==
                   "Electronic,
                 Rock, Funk / Soul, Blues, Pop"] <- "Electronic"
three_data$Genre[three_data$Genre ==
                   "Electronic, Rock, Funk / Soul, Pop" ] <- "Electronic"
three_data$Genre[three_data$Genre ==
                   "Electronic, Rock,
                 Funk / Soul, Stage & Screen"] <- "Electronic"
three_data$Genre[three_data$Genre == "Electronic, Rock, Pop"] <- "Electronic"
three_data$Genre[three_data$Genre ==
                   "Electronic,ÊStage & Screen"] <- "Electronic"
three_data$Genre[three_data$Genre ==
                   "Funk / Soul,ÊFolk, World, & Country"] <- "Electronic"

# Folk 
three_data$Genre[three_data$Genre == "Folk, World, & Country"] <- "Folk"


three_data$Genre[three_data$Genre == "Funk / Soul, Blues"] <- "Funk / Soul"
three_data$Genre[three_data$Genre ==
                   "Funk / Soul, Folk, World, & Country"] <- "Funk / Soul"
three_data$Genre[three_data$Genre == "Funk / Soul, Pop"] <- "Funk / Soul"
three_data$Genre[three_data$Genre ==
                   "Funk / Soul, Stage & Screen"] <- "Funk / Soul"
three_data$Genre[three_data$Genre ==
                   "Funk / Soul,ÊFolk, World, & Country"] <- "Funk / Soul"
three_data$Genre[three_data$Genre == "Latin, Funk / Soul"] <- "Funk / Soul"


# Hip Hop
three_data$Genre[three_data$Genre == "Hip Hop, Funk / Soul"] <- "Hip Hop"
three_data$Genre[three_data$Genre == "Hip Hop, Rock"] <- "Hip Hop"
three_data$Genre[three_data$Genre ==
                   "Hip Hop, Rock, Funk / Soul"] <- "Hip Hop"

# Jazz
three_data$Genre[three_data$Genre == "Jazz, Funk / Soul"] <- "Jazz"
three_data$Genre[three_data$Genre == "Jazz, Pop"] <- "Jazz"
three_data$Genre[three_data$Genre == "Jazz, Rock"] <- "Jazz"
three_data$Genre[three_data$Genre ==
                   "Jazz, Pop, Folk, World, & Country"] <- "Jazz"
three_data$Genre[three_data$Genre ==
                   "Jazz, Rock, Blues, Folk, World, & Country"] <- "Jazz"
three_data$Genre[three_data$Genre ==
                   "Jazz, Rock, Funk / Soul, Blues" ] <- "Jazz"
three_data$Genre[three_data$Genre ==
                   "Jazz, Rock, Funk / Soul,
                 Folk, World, & Country"] <- "Jazz"
three_data$Genre[three_data$Genre ==
                   "Jazz, Rock, Funk / Soul,
                 Pop, Folk, World, & Country"] <- "Jazz"
three_data$Genre[three_data$Genre == "Jazz, Rock, Pop"] <- "Jazz"

# Pop
three_data$Genre[three_data$Genre == "Pop, Folk, World, & Country"] <- "Pop"

# Reggae 
three_data$Genre[three_data$Genre ==
                   "Reggae,ÊPop,ÊFolk,World, &
                 Country,ÊStage & Screen"] <- "Reggae"

# Rock
three_data$Genre[three_data$Genre ==
                   "Rock, Blues, Folk, World, & Country"] <- "Rock"
three_data$Genre[three_data$Genre == "Rock, Blues, Pop"] <- "Rock"
three_data$Genre[three_data$Genre ==
                   "Rock, Folk, World, & Country"] <- "Rock"
three_data$Genre[three_data$Genre == "Rock, Funk / Soul"] <- "Rock"
three_data$Genre[three_data$Genre == "Rock, Funk / Soul, Blues"] <- "Rock"
three_data$Genre[three_data$Genre == "Rock, Funk / Soul, Blues,
                 Pop, Folk, World, & Country"] <- "Rock"
three_data$Genre[three_data$Genre ==
                   "Rock, Funk / Soul, Folk, World, & Country"] <- "Rock"
three_data$Genre[three_data$Genre == "Rock, Funk / Soul, Pop"] <- "Rock"
three_data$Genre[three_data$Genre == "Rock, Latin"] <- "Rock"
three_data$Genre[three_data$Genre ==
                   "Rock, Latin, Funk / Soul"] <- "Rock"
three_data$Genre[three_data$Genre == "Rock, Pop"] <- "Rock"
three_data$Genre[three_data$Genre ==
                   "Rock, Pop, Folk, World, & Country"] <- "Rock"
three_data$Genre[three_data$Genre == "Rock, Reggae"] <- "Rock"
three_data$Genre[three_data$Genre == "Rock, Reggae, Latin"] <- "Rock"
three_data$Genre[three_data$Genre == "Rock, Stage & Screen"] <- "Rock"
three_data$Genre[three_data$Genre == "Rock, Blues"] <- "Rock"
three_data$Genre[three_data$Genre == "Rock,ÊBlues"] <- "Rock"
three_data$Genre[three_data$Genre == "Rock,ÊPop"] <- "Rock"

# Fixing other characters
three_data$Artist[three_data$Artist ==
                    "Stan GetzÊ/ÊJoao
                  GilbertoÊfeaturingÊAntonio
                  Carlos Jobim"] <- "Stan Getz
and Joao Gilberto featuring Antonio Carlos Jobim"

three_data$Album[three_data$Album ==
                   "Honky Chteau"] <- "Honky Chateau"

three_data <- three_data %>%
  mutate("Decade" = floor(Year / 10) * 10)

# Function for Server to filter Genre and generate plot
displayThreePlot1 <- function(input) {

# Filter to the selected Genre; all of the albums are condensed to
# these 10 core genres
  if (input$genre == "Blues") {
    three_data_plot <- three_data %>%
      filter(Genre == "Blues")
  } else if (input$genre == "Classical") {
    three_data_plot <- three_data %>%
      filter(Genre == "Classical")
  } else if (input$genre == "Electronic") {
    three_data_plot <- three_data %>%
      filter(Genre == "Electronic")
  } else if (input$genre == "Folk") {
    three_data_plot <- three_data %>%
      filter(Genre == "Folk")
  } else if (input$genre == "Funk / Soul") {
    three_data_plot <- three_data %>%
      filter(Genre == "Funk / Soul")
  } else if (input$genre == "Hip Hop") {
    three_data_plot <- three_data %>%
      filter(Genre == "Hip Hop")
  } else if (input$genre == "Jazz") {
    three_data_plot <- three_data %>%
      filter(Genre == "Jazz")
  } else if (input$genre == "Pop") {
    three_data_plot <- three_data %>%
      filter(Genre == "Pop")
  } else if (input$genre == "Reggae") {
    three_data_plot <- three_data %>%
      filter(Genre == "Reggae")
  } else if (input$genre == "Rock") {
    three_data_plot <- three_data %>%
      filter(Genre == "Rock")
  } else {
    three_data_plot <- three_data
  }

  # Creates a Scatterplot based on a Genre and its albums from
  MakeRSPlot <- plot_ly(three_data_plot,
                        x = ~Year, y = ~Number, color = ~Genre,
                        type = "scatter", mode = "markers",
                        size = ~Number,
                        marker = list(size = 10, opacity = 0.5),
                        text = ~paste("Rank:", Number,
                                      "<br>Album:", Album,
                                      "<br>Artist:", Artist,
                                      "<br>Year:", Year),
                        hoverinfo = "text") %>%
    hide_colorbar() %>%
    layout(title = "Rolling Stones Top 500 Albums Timeline",
           margin = list(t = "110"),
           xaxis = list(title = "Year", showgrid = FALSE,
                        zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(title = "Year", showgrid = FALSE,
                        zeroline = FALSE, showticklabels = FALSE))
}

displayThreePlot2 <- function(input) {
  if (input$decade == "1950") {
    three_data_plot <- three_data %>%
      group_by(Genre) %>%
      filter(Decade == "1950") %>%
      summarise(count = n())
  } else if (input$decade == "1960") {
    three_data_plot <- three_data %>%
      group_by(Genre) %>%
      filter(Decade == "1960") %>%
      summarise(count = n())
  } else if (input$decade == "1970") {
    three_data_plot <- three_data %>%
      group_by(Genre) %>%
      filter(Decade == "1970") %>%
      summarise(count = n())
  } else if (input$decade == "1980") {
    three_data_plot <- three_data %>%
      group_by(Genre) %>%
      filter(Decade == "1980") %>%
      summarise(count = n())
  } else if (input$decade == "1990") {
    three_data_plot <- three_data %>%
      group_by(Genre) %>%
      filter(Decade == "1990") %>%
      summarise(count = n())
  } else if (input$decade == "2000") {
    three_data_plot <- three_data %>%
      group_by(Genre) %>%
      filter(Decade == "2000") %>%
      summarise(count = n())
  } else if (input$decade == "2010") {
    three_data_plot <- three_data %>%
      group_by(Genre) %>%
      filter(Decade == "2010") %>%
      summarise(count = n())
  } else {
    three_data_plot <- three_data %>%
      group_by(Genre) %>%
      summarise(count = n())
  }

  MakeRSPie <- plot_ly(three_data_plot,
                       labels = ~Genre,
                       values = ~count,
                       type = "Pie") %>%
    layout(title = "Rolling Stones Genre Breakdown",
           xaxis = list(showgrid = FALSE,
                        zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE,
                        zeroline = FALSE, showticklabels = FALSE))
}

response_three <- tabPanel("Genre Popularity Over the Years",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("genre", "Genre:",
                                           choices = c("All", "Blues",
                                                       "Classical",
                                                       "Electronic", "Folk",
                                                       "Funk / Soul",
                                                       "Hip Hop", "Jazz",
                                                       "Pop",
                                                       "Reggae", "Rock"),
                                           selected = "All"),
                               selectInput("decade", "Decade:",
                                           choices = c("All", "1950", "1960",
                                                       "1970", "1980", "1990",
                                                       "2000", "2010"),
                                           selected = "All")
                             ),
                             mainPanel(
                               plotlyOutput("MakeRSPlot"),
                               br(),
                               plotlyOutput("MakeRSPie")
                             )
                           ),
                           fluidRow(
                             column(width = 1),
                             column(width = 10,
                                    div(h1("Analysis:"),
                                        p("For this section, we looked at the
                                        Rolling Stones Top 500 Album List.
                                        More specifically, we focused on
                                        analyzing how albums and their
                                        respective genres fared between
                                        the years 1955 and 2011. Overall,
                                        as a trend, older albums were more
                                        likely to make the list and that newer
                                        albums struggled to get on. This shows
                                        that older albums tend to be regarded as
                                        the greatest. Also, in regards to genre
                                        popularity, Rock has consistently
                                        performed well over the years and has
                                          the most appearances on the list with
                                          318 albums alone; which makes up
                                          roughly 64% of the entire list."),
                                        p("As greatest is subjective, this
                                        could be due to a variety of reasons,
                                        whether it is because older albums are
                                        just better than the music being
                                        released more recently, a bias from
                                        the critics, who probably grew up
                                        listening the older albums and those
                                        album's genre being Rock. Or could be
                                        fact that newer albums and other genres
                                        have not had as much time to get
                                        established as a great. All we can say
                                        for certain is that the older albums
                                        and rock albums were generally
                                        regarded as the Greatest 500 Albums
                                          of All Time as decided by
                                          Rolling Stones!"),
                             column(width = 1))))
)