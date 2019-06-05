library(dplyr)
library(ggplot2)
library(RColorBrewer)


# Variable to store axis options
axis_vars <- list("Year" = "year", "Rank" = "rank",
                  "Album Length (minutes)" = "track_min")


# Creates the database with all the columns needed
one_data <- read.csv("../../data/billboardalbum200.csv",
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

# Create graph of specified variables ----------------------------------------

# Get specified variables from user
var <- sliderInput("one_min_length", "Maximum length of album (in minutes)",
                    3, 800, 100, step = 5)
var_1 <- sliderInput("one_track_num", "Minimum number of tracks on album",
                      0, 100, 18, step = 1)
# var_2 <- sliderInput("one_year", "Year on Billboard Top 200", 1963, 2019,
#                      value = c(1963, 2019),
#                      sep = "")
# var_3 <- sliderInput("one_time_on", "Number of weeks on Billboard Top 200",
#                      0, 52, c(0, 52), step = 1)

# Create tabPanel for the page
response_one <- tabPanel(
  "Page 1",
  titlePanel("How Length (Number and Time) Relate to Popularity"),
  sidebarLayout(
    sidebarPanel(
      #Accepts user input
      filter1 <- var,
      filter2 <- var_1,
      selectInput(
        "y_var_1",
        "By length in minutes or tracks",
        choices = list("Minutes" = "track_length", "Number of Tracks" = "num_tracks"),
        selected = "track_length"
      )
      # filter3 <- var_3[[1]],
      # filter4 <- var_3[[2]]
    ),
    mainPanel(
      plotOutput(outputId = "bar_graph")
    )
  )
)




# 
# navbarPage(
#   title = ("Do Track Number or Length Matter?"),
#   sidebarPanel(
#     column(3,
#            wellPanel(
#              h4("Filter Fields"),
#              sliderInput("one_min_length", "Minimum length of album (in minutes)",
#                          3, 800, 100, step = 5),
#              sliderInput("one_track_num", "Minimum number of tracks on album",
#                          0, 700, 18, step = 1),
#              sliderInput("one_year", "Year on Billboard Top 200", 1963, 2019, value = c(1963, 2019),
#                          sep = ""),
#              sliderInput("one_time_on", "Number of weeks on Billboard Top 200",
#                          0, 52, c(0, 52), step = 1)
#            ),
#            wellPanel(
#              selectInput("xvar", "X-axis variable", axis_vars, selected = "year"),
#              selectInput("yvar", "Y-axis variable", axis_vars, selected = "track_min"),
#              tags$small(paste0(
#                "Note: Some help found through movie explorer",
#                "shiny from R studio help graph by Winston Chang"
#              ))
#            )
#     ),
#     column(9,
#            ggvisOutput("plot1")
#     )
#   )
# )

