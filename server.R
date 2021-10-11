library(tidyverse)
library(plotly)
library(DT)
library(gganimate)

#Create a look up table to get complete country name from code. 
lookup <- data.frame(country = c('PRT', 'ESP', 'FRA', 'GBR', 'ITA', 'DEU', 'BRA', 'NLD', 'IRL', 
                                 'BEL', 'AUT', 'CHE', 'CHN', 'NOR', 'POL', 'RUS', 'SWE', 'USA', 'ROU', 'ISR'),
                     country_name = c('Portugal', 'Spain', 'France', 'Great Britain', 'Italy', 'Germany', 
                                      'Brazil', 'Netherlands', 'Ireland', 'Belgium', 'Austria', 
                                      'Switzerland', 'China', 'Norway', 'Poland', 'Russia', 'Sweden', 'USA', 'Romania', 'Israel'))

hotel_data <- read_csv('hotel_bookings.csv')
#Join to get complete country names
hotel_data <- hotel_data %>% left_join(lookup, by = 'country')

#define server function
server <- function(input, output) {
  #top 10 countries to visit
  output$top10_monthly_tourist <- renderPlot({
    hotel_data %>%
      #select rows for selected month and whose booking is not cancelled. 
      dplyr::filter(arrival_date_month == input$month, is_canceled == 0) %>%
      group_by(country, country_name, hotel) %>%
      #Add adults, children and babies as total visitos
      summarise(total_people = sum(adults + children + babies)) %>%
      mutate(total = sum(total_people)) %>%
      ungroup %>%
      arrange(desc(total)) %>%
      #Select top 10 countries
      dplyr::filter(country %in% head(unique(country), 10)) %>%
      mutate(country_name = factor(country_name, unique(country_name))) %>%
      ggplot() + aes(country_name, total_people, fill = hotel) + 
      geom_col(position = 'dodge') + 
      labs(title = paste0('Number of visitors* in the month of ', input$month), 
           x = 'Country', 
           y = 'Count', 
           caption = '*Visitors include adults and small childrens of all age') +
      scale_fill_manual(values = c('tomato1', 'royalblue2'), name = 'Type of Hotel') + 
      theme_bw() + 
      #Take legend inside plot
      theme(legend.position=c(.9,.75))
  })
  
  output$plot2 <- renderPlot({
    hotel_data %>%
      dplyr::filter(is_canceled == 0) %>%
      group_by(hotel, arrival_date_month) %>%
      summarise(total_visitors = sum(adults + children + babies, na.rm = TRUE)) %>%
      mutate(arrival_date_month = factor(arrival_date_month, month.name)) %>%
      ggplot(aes(arrival_date_month, total_visitors, color = hotel, group = hotel)) + 
      geom_line(size = 1.5) + 
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
      count(country_name, market_segment) %>%
      group_by(country_name) %>%
      mutate(n = prop.table(n) * 100) %>%
      ggplot() + aes(market_segment, n) + 
      geom_col(fill = "steelblue4") + facet_wrap(~country_name, scales = 'free_x') + 
      labs(x = 'Market Segment', y = 'Percentage of bookings', 
           title = 'Which marketing segment is used by top 5 countries ?') + 
      theme_bw()
  })
  
  output$home_plot <- renderPlotly({
    hotel_data %>%
      group_by(country, country_name) %>%
      summarise(total = sum(adults + children + babies, na.rm = TRUE))  %>%
      filter(total > 1000) %>% na.omit() -> tmp
    
    plot_ly(tmp, labels = ~country_name, values = ~total, type = 'pie') %>%
      layout(legend = list(orientation = "h", xanchor = "center",  x = 0.1 ,y = 1.6))  
  })
  
}
