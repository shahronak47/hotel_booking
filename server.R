library(tidyverse)
library(plotly)
library(DT)

server <- function(input, output) {
  #hotel_data <- read_csv('hotel_bookings.csv')
  
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
  
  
  output$average_stay <- DT::renderDataTable({
    hotel_data %>%
      mutate(`Number of children` = children + babies, 
             total_time = stays_in_weekend_nights + stays_in_week_nights) %>%
      group_by(`Number of children`) %>%
      summarise(`Average Stay (in days)` = round(mean(total_time, na.rm = TRUE), 2), 
                Count = n()) %>%
      na.omit() -> res
    
    datatable(res, options = list(dom = 't'), rownames = NULL, selection = 'none')
  })
  
  
  output$average_stay_plot <- renderPlot({
    hotel_data %>%
      mutate(num = children + babies, 
             total_time = stays_in_weekend_nights + stays_in_week_nights) %>%
      group_by(num = factor(num)) %>%
      summarise(avg_stay = round(mean(total_time, na.rm = TRUE), 2)) %>%
      na.omit() %>%
      ggplot(aes(num, avg_stay, group = 1)) + geom_line() + geom_point() + 
      labs(title = "Average stay duration",
           x = 'Number of children', 
           y = 'Number of days') +
      theme_bw()
  })
  
  output$plot3 <- renderPlot({
    hotel_data %>%
      mutate(total = adults + children + babies) %>%
      group_by(country) %>%
      summarise(total = sum(total, na.rm = TRUE)) %>%
      slice_max(total, n = 6) %>%
      left_join(hotel_data, by = 'country') %>%
      count(country, market_segment) %>%
      group_by(country) %>%
      mutate(n = prop.table(n) * 100) %>%
      ggplot() + aes(market_segment, n) + 
      geom_col() + facet_wrap(~country, scales = 'free_x') + 
      labs(x = 'Market Segment', y = 'Percentage of bookings', 
           title = 'Which marketing segment is used by top 5 countries ?') + 
      theme_bw()
  })
}
