# Data Question One
library(dplyr)
library(shiny)
library(RColorBrewer)

one_data <- read.csv("../../data/billboardalbum200.csv", stringsAsFactors = F)
colors <- brewer.pal(3, "Accent")
colors <- list("Seafoam" = colors[1], "Lilac" = colors[2], "Peach" = colors[3])

#create new column with track length in minutes called length_min
one_data <- one_data %>%
  mutate(length_min = track_length / 100 / 60)

# This page will explore: How has the length of albums (in minutes)
# in the Top 200 Per Week changed over time for specific rankings? 
# Do some number of tracks have a higher liklihood of staying in the
# billboard 200 for longer? How long OR how many songs are likely for
# a given year?

# The first graph will: look at all instances of a user chosen rank
# (for example: 5th place) and using a smoothed line graph show the trend of
# albumn length (in seconds) over time.
# The x variable will be time (years)
# The y variable will be the length_min

one_smooth_sidebar <- sidebarPanel(
  # Widget grabbing the rank of interest
  rank_looking_at <- textInput(inputId = "rank_looking_at",
                          label = "What rank would you like to see? (1-200)"),
  
  # Filtering data frame to look at rank of interest 
  # (i.e. the purpose of the graph), and gather the data into years
  one_data_rank <- one_data %>%
    filter(rank == as.integer(rank_looking_at)) %>%
    mutate(year = substr(date, 1, 4)) %>%
    group_by(year) %>%
    summarize(mean_length = mean(length_min, na.rm = TRUE)),
  
  # Decide on a color for the graph line
  selectInput(
    "one_smooth_color",
    "Color of line",
    choices = colors,
    selected = colors[[2]]
  )
)

one_smooth_main <- mainPanel(plotOutput("one_smooth"))


# The second graph will: Look at the number of tracks within the top
# user selected number of chosen ranked album each week and which number
# is most popular for each rank
one_count_sidebar <- sidebarPanel(
  
  # Select range of ranks
  rank_selected <- sliderInput(
    "one_count_slider",
    "Up to what rank?",
    min = 1, max = 200, value = 10
  ),
  
  # Get dataframe of interest
  one_data_rank <- one_data %>%
    filter(rank <= rank_selected) %>%
    select(rank, length),
  
  # Select Color of dots
  selectInput(
    "one_count_color",
    "Color of dots",
    choices = colors,
    selected = colors[[3]]
  )
)

one_count_main <- mainPanel(plotOutput("one_count"))

# The last graph will: bubble plot where year = categorical variable and
# continuous is either num of tracks OR length in mins

one_scatter_sidebar <- sidebarPanel(
  radioButtons(
    "one_scatter_y_var",
    "What Measurement?",
    choices = list("Number of tracks" = "length",
                   "Length of album (minutes)" = "length_min"),
    selected = "length"
  ),
  
  one_rank <- sliderInput(
    "one_scatter_rank",
    "Which Rank?",
    min = 1, max = 200, value = 1
  ),
  
  one_data_rank <- one_data %>%
    filter(rank == one_rank)
)
  




