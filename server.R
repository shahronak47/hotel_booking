library(tidyverse)
library(plotly)

server <- function(input, output) {
  hotel_data <- read_csv('hotel_bookings.csv')
  
  output$top10_monthly_tourist <- renderPlot({
    hotel_data %>%
      dplyr::filter(arrival_date_month == input$month, is_canceled == 0) %>%
      group_by(country, hotel) %>%
      summarise(total_people = sum(adults + children + babies)) %>%
      mutate(total = sum(total_people)) %>%
      ungroup %>%
      arrange(desc(total)) %>%
      dplyr::filter(country %in% head(unique(country), 10)) %>%
      mutate(country = factor(country, unique(country))) %>%
      ggplot() + aes(country, total_people, fill = hotel) + 
      geom_col(position = 'dodge') + 
      labs(title = paste0('Number of visitors* in the month of ', input$month), 
           x = 'Country', 
           y = 'Count', 
           caption = '*Visitors include adults and small childrens of all age') +
      scale_fill_manual(values = c('red', 'blue'), name = 'Type of Hotel') + 
      theme_bw() + 
      theme(legend.position=c(.9,.75))
  })
  
  output$plot2 <- renderPlot({
    hotel_data %>%
      dplyr::filter(is_canceled == 0) %>%
      group_by(hotel, arrival_date_month) %>%
      summarise(total_visitors = sum(adults + children + babies, na.rm = TRUE)) %>%
      mutate(arrival_date_month = factor(arrival_date_month, month.name)) %>%
      ggplot(aes(arrival_date_month, total_visitors, color = hotel, group = hotel)) + 
      geom_line() + 
      labs(title = "Number of visitors* by month",
           x = 'Month', 
           y = 'Count',
           caption = '*Visitors include adults and small childrens of all age') +
      scale_color_manual(values = c('red', 'blue'), name = 'Type of Hotel') + 
      theme_bw() + 
      theme(legend.position=c(.9,.75))
  })
  
}
