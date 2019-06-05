# Data Question Two

# Preparation -----------------------------------------------------------------

# Load in packages
library("dplyr")
library("shiny")
library("tidyr")
library("tidytext")
library("wordcloud")
library("ggplot2")

# Read in my csv file
two_data <- read.csv(
  "../../data/billboard_lyrics_1964-2015.csv", stringsAsFactors = F)


# Create Table of Top Common Words by Decade ----------------------------------

# Remove unnecessary rows
useful <- two_data %>%
  drop_na() %>% # Drop na values
  filter(nchar(Lyrics) > 5) # Drop rows where lyrics are less than 5 chars

# Create a data frame of unimportant words
bad_words <- c("dont", "yeah", "im", "wanna", "ya", "youre", "hey", "ill",
               "ooh", "aint", "gonna", "la", "ive", "uh", "na", "wont",
               "gotta", "da", "call", "em", "youve", "id")
unimportant <- data.frame(bad_words, stringsAsFactors = FALSE)


# Rename column name to "word"
colnames(unimportant) <- "word"

# Get a full list of words used, removing stop and other unimportant words
non_stop_words <- useful %>%
  unnest_tokens(word, Lyrics) %>%
  anti_join (stop_words, by = "word") %>%
  anti_join(unimportant, by = "word")

# Get top ten words, with no stop words, used each year
top_ten_words <- non_stop_words %>%
  mutate(decade = floor(Year / 10) * 10) %>%
  group_by(decade) %>%
  add_count(word, sort = TRUE) %>%
  distinct(decade, word, n) %>%
  top_n(10, n) %>%
  arrange(-n) %>%
  mutate(words = toString(word)) %>%
  distinct(decade, words) %>%
  arrange(decade)

# Create a wordcloud of top fifty words -----------------------------------------
cloud_list <- non_stop_words %>%
  select(word) %>%
  count(word, sort = TRUE) %>%
  ungroup()

pal <- brewer.pal(8, "Dark2")


cloud <- cloud_list %>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors = pal,
                 scale = c(1, .5)))

# Create line chart for each decade's top ten words ---------------------------

# Create a vector containing top twenty words over time
top_20 <- non_stop_words %>%
  group_by(word) %>%
  add_count(word, name = "occurrences") %>%
  distinct(word, occurrences) %>%
  ungroup() %>%
  top_n(20, occurrences) %>%
  arrange(-occurrences) %>%
  pull(word)

# Create a table showing top twenty word popularity over time
word_table <- non_stop_words %>%
  mutate(decade = floor(Year / 10) * 10) %>%
  filter(word %in% top_20) %>%
  select(word, Year, decade) %>%
  group_by(word, Year) %>%
  add_count(word, name = "year_occurrences") %>%
  ungroup() %>%
  group_by(word, decade) %>%
  add_count(word, name = "decade_occurrences") %>%
  distinct(word, Year, decade, year_occurrences, decade_occurrences) %>%
  rename(term = word) %>%
  arrange(decade)

# Create a list of each decade
decades <- seq(1960, 2015, 10)

# Store a selectInput() for the variable to appear on the y-axis of my chart
y_axis_var <- selectInput(inputId = "y_var",
                          label = "Select Word",
                          choices = top_20,
                          selected = top_20[1])

# Store a selectInput() to determine the color of the chart's points
color_input <- selectInput(inputId = "color",
                           label = "Choose Color",
                           choices = c("black", "blue", "red", "orange",
                                       "green", "purple"))

# Create tabPanel for page ----------------------------------------------------
response_two <- tabPanel(
  "Lyrics",
  titlePanel("Lyrics Through the Decades"),
  sidebarLayout(
    sidebarPanel(
      # Accept y-axis input
      y_var <- y_axis_var,
      color <- color_input,
      br(), hr(), h4("What Does This Show?"),
      br(), p("These charts show the popularity of the top 20 words used in",
              "songs each decade, from the 1960s through the 2010s. The top",
              "chart shows the number of occurrences of the word each decade;",
              "the bottom one shows the distribution of its usage each year.")
    ),
    mainPanel(
      # Plot the output with the name "line"
      plotOutput(outputId = "line_two"),
      plotOutput(outputId = "point_two")
    )
  )
)
