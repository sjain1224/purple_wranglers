# Data Question One
library(dplyr)
library(shiny)
one_data <- read.csv("../../data/billboardalbum200.csv", stringsAsFactors = F)

#create new column with track length in minutes called length_min
one_data <- one_data %>%
  mutate(length_min = track_length / 100 / 60)

# This page will ask: How has the length of albums
# (by number of tracks and track length)
# in the Top 200 Per Week changed over time?

# The first graph will: look at all instances of a user chosen rank
# (for example: 5th place) and using a smoothed line graph show the trend of
# albumn length (in seconds) over time.
# The x variable will be time (years)
# The y variable will be the length_min

# Widget grabbing the rank of interest
rank_looking_at <- textInput(inputId = "rank_looking_at",
                             label = "What rank would you like to see? (1-200)")
# Filtering data frame to look at rank of interest 
# (i.e. the purpose of the graph), and gather the data into years
one_data_rank <- one_data %>%
  filter(rank == as.integer(rank_looking_at)) %>%
  mutate(year = substr(date, 1, 4)) %>%
  group_by(year) %>%
  summarize(mean_length = mean(length_min, na.rm = TRUE))

  




