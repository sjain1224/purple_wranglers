library(ggvis)

axis_vars <- list("Year" = "year", "Rank" = "rank", "Album Length (minutes)" = "track_min")


fluidPage(
  titlePanel("Do Track Number or Length Matter?"),
  fluidRow(
    column(3,
           wellPanel(
             h4("Filter Fields"),
             sliderInput("one_min_length", "Minimum length of album (in minutes)",
                         3, 800, 100, step = 5),
             sliderInput("one_track_num", "Minimum number of tracks on album",
                         0, 700, 18, step = 1),
             sliderInput("one_year", "Year on Billboard Top 200", 1963, 2019, value = c(1963, 2019),
                         sep = ""),
             sliderInput("one_time_on", "Number of weeks on Billboard Top 200",
                         0, 52, c(0, 52), step = 1)
           ),
           wellPanel(
             selectInput("xvar", "X-axis variable", axis_vars, selected = "year"),
             selectInput("yvar", "Y-axis variable", axis_vars, selected = "track_min"),
             tags$small(paste0(
               "Note: Some help found through movie explorer",
               "shiny from R studio help graph by Winston Chang"
             ))
           )
    ),
    column(9,
           ggvisOutput("plot1")
    )
  )
)