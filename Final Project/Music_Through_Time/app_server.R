library(ggvis)
library(dplyr)

# Set up handles to database tables on app start
one_data <- read.csv("../../data/billboardalbum200.csv", stringsAsFactors = F)
one_data <- one_data %>%
  mutate(track_min = round(track_length / 100 / 60, digits = 1)) %>%
  mutate(year = substr(date, 1, 4)) %>%
  mutate(month = substr(date, 6, 7)) %>%
  group_by(album) %>%
  select(album, rank, artist, year, month, track_min, length) %>%
  mutate(mean_rank = round(mean(rank, na.rm = T), 0),
         track_length = round(mean(track_min, na.rm = T), 1),
         num_tracks = round(mean(length, na.rm = T), 0), num_wks = n())


server <- function(input, output) {
  # 
  # # Filter the movies, returning a data frame
  # one <- reactive({
  #   # Due to dplyr issue #318, we need temp variables for input values
  #   time <- input$one_min_length
  #   track <- input$one_track_num
  #   minyear <- input$one_year[1]
  #   maxyear <- input$one_year[2]
  #   minduration <- input$one_time_on[1]
  #   maxduration <- input$one_time_on[2]
  #   
  #   # Apply filters
  #   m <- one_data %>%
  #     filter(
  #       track_length >= time,
  #       num_tracks >= track,
  #       year >= minyear,
  #       year <= maxyear,
  #       num_wks >= minduration,
  #       num_wks <= maxduration
  #     )
  #   
  #   m <- as.data.frame(m)
  #   
  #   # Add column which says whether the movie won any Oscars
  #   # Be a little careful in case we have a zero-row data frame
  #   m$above_10 <- character(nrow(m))
  #   m$above_10[m$mean_rank <= 10] <- "Yes"
  #   m$above_10[m$mean_rank > 10] <- "No"
  #   m
  # })
  # 
  # # Function for generating tooltip text
  # movie_tooltip <- function(x) {
  #   if (is.null(x)) return(NULL)
  #   if (is.null(x$ID)) return(NULL)
  #   
  #   # Pick out the movie with this ID
  #   one_data <- isolate(one_data())
  #   one_data_temp <- one_data[one_data$ID == x$ID, ]
  #   
  #   paste0("<b>", one_data_temp$album, "</b><br>",
  #          one_data_temp$artist,"</b>")
  # }
  # 
  # # A reactive expression with the ggvis plot
  # vis <- reactive({
  #   # Lables for axes
  #   xvar_name <- names(axis_vars)[axis_vars == input$xvar]
  #   yvar_name <- names(axis_vars)[axis_vars == input$yvar]
  #   
  #   # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
  #   # but since the inputs are strings, we need to do a little more work.
  #   xvar <- prop("x", as.symbol(input$xvar))
  #   yvar <- prop("y", as.symbol(input$yvar))
  #   
  #   one_data %>%
  #     ggvis(x = xvar, y = yvar) %>%
  #     layer_points(size := 50, size.hover := 200,
  #                  fillOpacity := 0.2, fillOpacity.hover := 0.5,
  #                  stroke = ~above_10, key := ~ID) %>%
  #     add_tooltip(movie_tooltip, "hover") %>%
  #     add_axis("x", title = xvar_name) %>%
  #     add_axis("y", title = yvar_name) %>%
  #     add_legend("stroke", title = "Average Rank was Above 10", values = c("Yes", "No")) %>%
  #     scale_nominal("stroke", domain = c("Yes", "No"),
  #                   range = c("sienna2", "saddlebrown")) %>%
  #     set_options(width = 500, height = 500)
  # })
  # 
  # vis %>% bind_shiny("plot1")
  # 
  # output$n_movies <- renderText({ nrow(one_data()) })
  # 
  # Create the line chart for question two
  output$line_two <- renderPlot({
    # Plot the Scatterplot
    my_line <- ggplot(data = top_tens) +
      geom_point(mapping = aes_string(x = input$decades, y = input$y_line),
                 color = input$color)
    my_line
  })
}
