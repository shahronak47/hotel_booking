library(shiny)
library(shinydashboard)
library(dashboardthemes)

ui <- dashboardPage(title = 'Hotel Booking',
  dashboardHeader(title = shinyDashboardLogo(
                    theme = "flat_red",
                    mainText = h3("Hotel Booking"),
                    boldText = "",
                    badgeText = ""
                  )),
  dashboardSidebar(sidebarMenu(
    menuItem("Stats", tabName = "stats", icon = icon("sort-numeric-down-alt")),
    menuItem("Findings", icon = icon("binoculars"), tabName = "findings")
  )),
  dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    tabItems(
      tabItem(tabName = "stats",
        valueBox(nrow(hotel_data), 'Total bookings', icon = icon('globe'), width = 3), 
        valueBox(paste0(round(mean(hotel_data$is_canceled) * 100, 2), '%'), 'Cancellation percentage', 
                icon = icon('times'), width = 3, color = 'red')
      ), 
      tabItem(tabName = "findings",
        
      )
    )
  )
)