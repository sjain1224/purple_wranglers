library(ggvis)
library(dplyr)


server <- function(input, output) {
  output$bar_graph <- renderPlot({
    graph_info <- one_data %>% 
      filter((track_length <= input$filter1) & (num_tracks <= input$filter2) &
             (num_wks >= input$filter3[1]) & (num_wks <= input$filter3[2]))
    
    my_graph <- ggplot(graph_info) +
      geom_count(mapping = aes_string(x = "year", y = input$y_var_1)) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5))
    my_graph
  })

  # Create the line chart for question two
  output$line_two <- renderPlot({
    # Filter to the specified word
    line_info <- word_table %>%
      filter(term == input$y_var)
    
    # Plot the line chart
    my_line <- ggplot(data = line_info) +
      geom_line(mapping = aes(x = decade, y = decade_occurrences),
                color = input$color) +
      theme_classic() +
      labs(title = paste0("Occurrences of \"", input$y_var, "\" over Decades"),
           x = "Decade",
           y = "Occurrences") +
      theme(panel.border = element_rect(color = "black", fill = NA, 
                                        size = 3),
            plot.title = element_text(hjust = 0.5))
    my_line
  })
  
  output$summ_top_ten <- renderDataTable({
    top_ten_words
  })
  
  output$point_two <- renderPlot({
    # Filter to the specified word
    line_info <- word_table %>% 
      filter(term == input$y_var)
    
    
    # Plot the scatterplot
    my_point <- ggplot(data = line_info) +
      geom_point(mapping = aes(x = Year, y = year_occurrences, alpha = 0.2,
                               size = year_occurrences), color = input$color) +
      scale_size_continuous(range = c(0.5, 16)) +
      theme_classic() +
      theme(panel.border = element_rect(color = "black", fill = NA, 
                                        size = 3),
            plot.title = element_text(hjust = 0.5)) +
      labs(title = paste0("Occurrences of \"", input$y_var, "\" over Years"),
           x = "Year",
           y = "Occurrences")
    my_point
  })
}
