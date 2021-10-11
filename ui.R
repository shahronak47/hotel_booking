library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinycssloaders)
library(plotly)

hotel_data <- readr::read_csv('hotel_bookings.csv')
#Get unique month-year combinations
month_data <- unique(with(hotel_data, paste(arrival_date_month, arrival_date_year)))

#Calculate valueBox statistics
v1 <- nrow(hotel_data)
v2 <- HTML(c('<span style = "font-size : 20px">', paste(month_data[1], month_data[length(month_data)], sep = ' - '), '</span>'))
v3 <- paste0(round(mean(hotel_data$is_canceled) * 100, 2), '%')
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
    menuItem("Findings1", icon = icon("binoculars"), tabName = "findings1"), 
    menuItem("Findings2", icon = icon("binoculars"), tabName = "findings2")
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
        valueBox(v2, 'Range of data', icon = icon('calendar'), width = 3, color = 'purple'), 
        valueBox(v3, 'Cancellation percentage', icon = icon('times'), width = 3, color = 'red'), 
        valueBox(v4, 'Visitors with kids', icon = icon('baby'), width = 3, color = 'green'), 
        br(), br(),br(), br(), br(), br(),br(), br(),
        h3('Visitors breakdown by country'),
        fluidRow(
          column(6, withSpinner(plotlyOutput('home_plot', height = '110%'), type = 5)), 
          column(6, img(src="file.gif"), type = 5)
          ), 
        HTML('<br/> <br/>
            <ul> Observations - 
              <br/>
              <li> Quite clearly Portugal has the highest percentage of visitors as they dominate the pie chart with over 40%. Next is followed by Great Britain and Spain</li>
              <li> The animation on the right shows arrival of tourists on weekly basis. We can see that the animation rises on the peak at around 33-35th week.</li>
            </ul>  
           ')
        ), 
      #Tab2
      tabItem(tabName = "findings1",
        h2("Monthly Visits"),
        br(),
        h3('Top 10 countries as visitor'),
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
      ), 
      tabItem(tabName = "findings2",
              h3('Marketing segment preference by visitors'),
              br(), br(),
              withSpinner(plotOutput('plot3'),type = 5),
              HTML('<br/> <br/>
            <ul> Observations - 
              <br/>
              <li> This plot shows marketting segments preferred for booking by top 6 countries. The plot is in percentage. </li>
              <li> One interesting observation, in most of the countries online travel agents are the preferred channel (constituting around 50% of all bookings) however, the biggest country (Portugal) uses groups booking as preffered method. They have online and offline travel agents at the same ratio more or less.</li>
              <li> Focussing on online travel agents is required but for Portugal we might need to prepare a different startegy as it is an important market. </li>
            </ul>  
           '),
              br(), br(),
              h3('Does number of children affect the length of stay?'),
              br(), br(),
              fluidRow(column(6, withSpinner(plotOutput('average_stay_plot', height = '300px'), type = 5)),
                       column(6, withSpinner(DT::dataTableOutput('average_stay', height = '300px'), type = 5))),
              HTML('<br/> <br/>
            <ul> Observations - 
              <br/>
              <li> There is no impact of children on length of stay. The average length of stay remains the same (in the same range) irrespective if you are visiting with or without children. We can see the average length of stay to be 3-4 days. </li>
              <li> Bringing 9-10 children is very rare as we can see there are only 3 instances of it and it can be considred as an outlier. </li>
            </ul>  
           ')
      )
    )
  )
)