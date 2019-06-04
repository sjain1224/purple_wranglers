library(shiny)
source("question_one.R")
source("question_two.R")
source("question_three.R")
source("overview_page.R")

# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "American Music Through Time",
  overview
)