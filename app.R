## app.R ##
library(shinydashboard)
library(tidyverse)

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
            h2("About"),
            p("The purpose of this app is to explore the Data Scientist Salaries dataset, 
              build a model and make predictions using that model"),
            p("The data set is from kaggle and can be found",
              a("here", href = "https://www.kaggle.com/datasets/ruchi798/data-science-job-salaries"),
              "."),
            p("It includes things like the year the salary was paid, 
              the experience level fo the employee, the empoyment type, 
              job title, and more."),
            p(strong("Data Exploration"),
              "- This tab will allow you to explore the data using 
              numerical and graphical summaries"),
            p(strong("Modeling"),
              "- This tab has three sub tabs and will allow you to find information 
              about the models available (Modeling Info), fit the data using the models available (Model Fitting), 
              and to make predictions using one of the models (Prediction)."),
            p(strong("Data"),
              "- This tab will allow you to scroll through the data set, 
              subset the data and save the dataset")
            # fluidRow(
            #   box(plotOutput("plot1", height = 250)),
            #   
            #   box(
            #     title = "Controls",
            #     sliderInput("slider", "Number of observations:", 1, 100, 50)
            #   )
            # )
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