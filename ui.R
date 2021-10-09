library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinycssloaders)

hotel_data <- readr::read_csv('hotel_bookings.csv')
#Get unique month-year combinations
month_data <- unique(with(hotel_data, paste(arrival_date_year, arrival_date_month)))

#Calculate valueBox statistics
v1 <- nrow(hotel_data)
v2 <- paste0(round(mean(hotel_data$is_canceled) * 100, 2), '%')
v3 <- HTML(paste(month_data[1], month_data[length(month_data)], sep = '-<br>'))
v4 <- paste0(round(mean(hotel_data$children > 0 | hotel_data$babies > 0, na.rm = TRUE) * 100, 2), '%')

ui <- dashboardPage(title = 'Hotel Booking',
                    
  dashboardHeader(title = shinyDashboardLogo(
                    theme = "flat_red",
                    mainText = h3("Hotel Booking"),
                    boldText = "",
                    badgeText = ""
                  )),
  
  dashboardSidebar(sidebarMenu(
    #Declare menu sub items
    menuItem("Stats", tabName = "stats", icon = icon("sort-numeric-down-alt")),
    menuItem("Findings", icon = icon("binoculars"), tabName = "findings")
  )),
  dashboardBody(
    shinyDashboardThemes(
      #Include the theme of the app
      theme = "blue_gradient"
    ),
    tabItems(
      #Tab 1
      tabItem(tabName = "stats",
        valueBox(v1, 'Total data points', icon = icon('globe'), width = 3), 
        valueBox(v2, 'Cancellation percentage', icon = icon('times'), width = 3, color = 'red'), 
        valueBox(v3, 'Range of data', icon = icon('calendar'), width = 3, color = 'purple'), 
        valueBox(v4, 'Visitors with kids', icon = icon('baby'), width = 3, color = 'green')
      ), 
      #Tab2
      tabItem(tabName = "findings",
        h2("Monthly Visits"),
        br(),
        h3('Top 10 countries to visit'),
        selectInput('month', 'Select Month : ', month.name),
        br(),
        withSpinner(plotOutput('top10_monthly_tourist'), type = 5),
      HTML('<br/> <br/>
            <ul> Observations - 
              <br/>
              <li> Overall, Portugal has the highest number of visitors. It is almost double than the visitors from the country at second place i.e Great Britain. So marketing team should focus on advertising more in Portugal than any other country. </li>
              <li> There are more city hotel visitors than resort hotel ones. </li>
            </ul>  
           '), 
          br(), 
          br(), 
          h3('Trend of visitors in different types of hotel by month'),
          plotOutput('plot2'),
      HTML('<br/> <br/>
            <ul> Observations - 
              <br/>
              <li> It can be clearly seen from the plot that June - August are the hottest month for bookings. So maybe we can plan our launch around May so that we get initial traction of visitors</li>
              <li> At any given point city hotel has higher visitors than resort hotel ones. So if we are planning to start a bussiness we should focus opening a city hotel. </li>
            </ul>  
           '),
      )
    )
  )
)