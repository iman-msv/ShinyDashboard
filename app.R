if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

p_load(shiny, shinydashboard, readr, dplyr, ggplot2, plotly, stringr, magrittr,
       lubridate)

walmart <- read_csv("Walmart_Store_sales.csv")
walmart

# Cleaning
names(walmart) %<>%
  str_to_lower()

walmart %<>%
  mutate(date = dmy(date),
         store = as.factor(store),
         holiday_flag = as.factor(holiday_flag),
         weekly_sales = weekly_sales / 1e3)

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
        valueBoxOutput("num_weeks")
      ),
      fluidRow(
        plotlyOutput("sales_over_weeks_store1")
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
  
  output$sales_over_weeks_store1 <- renderPlotly({
    sales_store1_plot <- walmart |> 
      ggplot(aes(date, weekly_sales, group = store)) + 
      geom_line(aes(color = store), alpha = 0.8) + 
      labs(
        x = "Date", 
        y = "Weekly Sales (thousand $)", 
        title = "Weekly Sales over Time in Store 1"
      ) + 
      theme(legend.position = "none")
      
      ggplotly(sales_store1_plot)
  })
}

# Running the App
shinyApp(ui, server)
