if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

p_load(shiny, shinydashboard, readr, dplyr, ggplot2, stringr, magrittr,
       lubridate)

walmart <- read_csv("Walmart_Store_sales.csv")
walmart

# Cleaning
names(walmart) %<>%
  str_to_lower()

walmart %<>%
  mutate(date = dmy(date),
         store = as.factor(store),
         holiday_flag = as.factor(holiday_flag))

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
        valueBoxOutput("age")
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
  
  output$age <- renderValueBox({
    mean <- round(mean(walmart$weekly_sales), 2)
    
    valueBox(
      value = mean,
      subtitle = "Weekly Sales Average",
      icon = icon("image-portrait"),
      color = "maroon"
    )
  })
}

# Running the App
shinyApp(ui, server)
