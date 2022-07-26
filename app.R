## app.R ##
library(shinydashboard)



header <- dashboardHeader(title = "Basic dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "about"),
    menuItem("Data Exploration", tabName = "data_exp"),
    menuItem("Modeling", tabName = "model",
             menuSubItem("Modeling Info", tabName = "info"),
             menuSubItem("Model Fitting", tabName = "fit"),
             menuSubItem("Prediction", tabName = "pred")
             ),
    menuItem("Data", tabName = "data")
  )
)

body <- dashboardBody(
  tabItems(
    # About
    tabItem(tabName = "about",
            fluidRow(
              box(plotOutput("plot1", height = 250)),
              
              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
            )
    ),
    
    # Data Exploration
    tabItem(tabName = "data_exp",
            h2("Data Exploration")
    ),
    
    # Modeling Info
    tabItem(tabName = "info",
            h2("Modeling Info")
    ),
    
    # Model Fitting
    tabItem(tabName = "fit",
            h2("Model Fitting")
    ),
    
    # Prediction
    tabItem(tabName = "pred",
            h2("Prediction")
    ),
    
    # Data
    tabItem(tabName = "data",
            h2("Data")
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)