if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

p_load(shiny, shinydashboard, readr, dplyr, ggplot2, plotly, 
       stringr, magrittr, lubridate, RColorBrewer)

colors <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

walmart <- read_csv("Walmart_Store_sales.csv")
walmart

# Cleaning
names(walmart) %<>%
  str_to_lower()

walmart %<>%
  mutate(date = dmy(date),
         store = as.factor(store),
         weekly_sales = weekly_sales / 1e3)

walmart_average_stores <- walmart |> 
  summarise(weekly_sales_average = mean(weekly_sales),
            holiday_flag = mean(holiday_flag),
            temperature = mean(temperature),
            fuel_price = mean(fuel_price),
            cpi = mean(cpi),
            .by = c(date))

# Dashboard
# Header, Sidebar, and Body
header <- dashboardHeader()

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard",
             tabName = "dashboard"),
    menuItem("Data", 
             tabName = "data")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      "dashboard",
      fluidRow(
        valueBoxOutput("mean_sales"),
        valueBoxOutput("num_weeks"),
        valueBoxOutput("stores")
      ),
      fluidRow(
        plotlyOutput("sales_over_weeks")
      ),
      fluidRow(
        plotlyOutput("sales_vs_temp")
      )
    ),
    tabItem(
      "data",
      DT::DTOutput("data_table")
    )
  )
)


# User Interface
ui <- dashboardPage(header, sidebar, body)


# Server
server <- function(input, output) {
  output$data_table <- DT::renderDataTable(
    walmart
  )
  
  output$mean_sales <- renderValueBox({
    mean <- round(mean(walmart$weekly_sales), 2)
    valueBox(
      value = mean,
      subtitle = "Weekly Sales Average (thousand $)",
      icon = icon("dollar-sign"),
      color = "maroon"
    )
  })
    
  output$num_weeks <- renderValueBox({
    num_unique_weeks <- length(unique(walmart$date))
    valueBox(
      value = num_unique_weeks,
      subtitle = "Weeks",
      icon = icon("calendar"),
      color = "maroon"
    )
  })
  
  output$stores <- renderValueBox({
    num_unique_store <- length(unique(walmart$store))
    valueBox(
      value = num_unique_store,
      subtitle = "Stores",
      icon = icon("store"),
      color = "maroon"
    )
  })
  
  output$sales_over_weeks <- renderPlotly({
    walmart_average_stores |> 
      plot_ly(x = ~date, y = ~weekly_sales_average) |> 
      add_lines() |> 
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Weekly Sales (thousand $)"),
             title = "Average of Weekly Sales Across Stores over Time",
             height = 300)
  })
  
  output$sales_vs_temp <- renderPlotly({
    walmart_average_stores |> 
      plot_ly(x = ~temperature, y = ~weekly_sales_average) |> 
      add_markers() |> 
      layout(xaxis = list(title = "Temperature"),
             yaxis = list(title = "Weekly Sales (thousand $)"),
             title = "Average of Weekly Sales vs. Temperature",
             height = 300,
             width = 600)
  })
}

# Running the App
shinyApp(ui, server)
