# Hotel Booking Demand

This repository aims at performing an exploratory data analysis (EDA) on the hotel booking demand dataset available on [Kaggle](https://www.kaggle.com/jessemostipak/hotel-booking-demand) . This has been developed as a shiny app which can be found [here](https://shahronak.shinyapps.io/hotel_booking/). 

EDA has been performed assuming that a company wants to start their own hotel chain and has contacted a Data Analyst/Scientist to provide actionable insights into the data which will be helpful while launching their own hotel chain. 

Below is brief description of the files in the repository. 

1. ui.R - User Interface (UI) code for shiny application. 
2. server.R - server code for shiny application. 
3. hotel_booking.Rproj - Application R project file. 
4. hotel_bookings.csv - Data. 
5. hotel_booking.Rmd - R markdown file was used as testing file to test the code before using it in shiny app. 
6. hotel_booking.html - Corresponding output of Rmd file. 

This application uses `ggplot` and `plotly` libraries for plotting. Apart from that it also uses `gganimate` to generate the animation. `tidyverse` libraries (mostly `dplyr`, `tidyr`, `readr` ) is used for data manipulation and transformation. As far as shiny is concerned it uses `shiny` and `shinydashboard` for basic shiny functions and layout of the app. `dashboardthemes` is used to apply theme to the app and `shinycssloaders` to show loading animation while showing plots and tables. `DT` package is used to display the tables. 
