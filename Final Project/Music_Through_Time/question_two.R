# Data Question Two

# Preparation -----------------------------------------------------------------

# Load in the "dplyr" package
library("dplyr")

# Load in the "shiny" package
library("shiny")

# Load in the "tidyr" package
library("tidyr")

# Load in the "tidytext" package
library("tidytext")

# Load in the "wordcloud" package
library("wordcloud")

# Read in my csv file
two_data <- read.csv(
  "../../data/billboard_lyrics_1964-2015.csv", stringsAsFactors = F)


# Create Table of Top Common Words by Decade ----------------------------------

# Remove unnecessary rows
useful <- two_data %>%
  drop_na() %>% # Drop na values
  filter(nchar(Lyrics) > 5) # Drop rows where lyrics are less than 5 chars

# Create a data frame of unimportant words
words <- c("dont", "yeah", "im", "wanna", "ya", "youre", "hey", "ill", "ooh",
           "aint", "gonna", "la", "ive", "uh", "na", "wont", "gotta", "da",
           "call", "em", "youve", "id")
unimportant <- data.frame(words, stringsAsFactors = FALSE)


# Rename column name to "word"
colnames(unimportant) <- "word"

# Get a full list of words used, removing stop and other unimportant words
non_stop_words <- useful %>% 
  unnest_tokens(word, Lyrics) %>% 
  anti_join (stop_words, by = "word") %>%
  anti_join(unimportant, by = "word")

# Get top ten words, with no stop words, used each year
top_ten_words <- non_stop_words %>%
  mutate(decade = floor(Year/10) * 10) %>% 
  group_by(decade) %>% 
  add_count(word, sort = TRUE) %>% 
  distinct(decade, word, n) %>% 
  top_n(10) %>%
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

# Create a list containing top ten words for each decade
top_tens = non_stop_words %>%
  mutate(decade = floor(Year/10) * 10) %>% 
  group_by(decade) %>% 
  add_count(word, sort = TRUE) %>% 
  distinct(decade, word, n) %>% 
  top_n(10) %>%
  ungroup() %>%
  distinct(word) %>% 
  pull(word)

# Create a list of each decade
decades <- seq(1960, 2015, 10)

# Store a selectInput() for the variable to appear on the y-axis of my chart
y_axis_var <- selectInput(inputId = "y_bar",
                          label = "Select Word",
                          choices = top_tens,
                          selected = top_tens[1])




# Create tabPanel for page ----------------------------------------------------
response_two <- tabPanel(
  "Page 2",
  titlePanel("Create A Bar Chart"),
  sidebarLayout(
    sidebarPanel(
      # Accept y-axis and x-axis inputs
      y_bar <- y_axis_var,
      x_bar <- decades
    ),
    mainPanel(
      # Plot the output with the name "line"
      plotOutput(outputId = "line_two")
    )
  )
)