library(dplyr)
library(ggplot2)
library(RColorBrewer)


# Variable to store axis options
axis_vars <- list("Year" = "year", "Rank" = "rank",
                  "Album Length (minutes)" = "track_min")


# Creates the database with all the columns needed
one_data <- read.csv("data/billboardalbum200.csv",
                     stringsAsFactors = F)
one_data <- one_data %>%
  mutate(track_min = round(track_length / 100 / 60, digits = 1)) %>%
  mutate(year = substr(date, 1, 4)) %>%
  mutate(month = substr(date, 6, 7)) %>%
  group_by(album) %>%
  select(album, rank, artist, year, month, track_min, length) %>%
  mutate(mean_rank = round(mean(rank, na.rm = T), 0),
         track_length = round(mean(track_min, na.rm = T), 1),
         num_tracks = round(mean(length, na.rm = T), 0), num_wks = n())

# Drop the first row
one_data <- one_data[-1, ]

# Create graph of specified variables ----------------------------------------

# Get specified variables from user
var <- sliderInput("filter1",
                   label = "Maximum length of album (in minutes)",
                   min = 5,
                   max = 800,
                   value = 100,
                   step = 5)
var_1 <- sliderInput("filter2", "Maximum number of tracks on album",
                      1, 666, 200, step = 1)
var_3 <- sliderInput("filter3", "Number of weeks on Billboard Top 200",
                    0, 52, c(0, 52), step = 1)

display <-  selectInput( "y_var_1", "By length in minutes or tracks",
  choices = list("Minutes" = "track_length",
                 "Number of Tracks" = "num_tracks"),
  selected = "track_length")

# Create tabPanel for the page
response_one <- tabPanel(
  "Data By Length",
  titlePanel("How Length (Number and Time) Relate to Popularity"),
  sidebarLayout(
    sidebarPanel(
      #Accepts user input
      filter1 <- var,
      filter2 <- var_1,
      filter3 <- var_3,
      y_var_1 <- display,
      br(), hr(), h4("What Does This Mean?"),
      br(), p("Explore the graph above by seeing which album falls within",
              "all of those categories. Once you have selected what you want",
              "to see, you may notice a few things... follow to our",
              "Summary page to see if you saw what we saw.")
    ),
    mainPanel(
      plotOutput(outputId = "bar_graph")
    )
  )
)
