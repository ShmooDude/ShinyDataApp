## app.R ##
library(datamods)
library(shinydashboard)
library(tidyverse)
library(skimr)
library(magrittr)
library(mathjaxr)

# Load CSV
neo <- read_csv("neo.csv", col_select = c(-1, -2, -7, -8))

my_skim <- skim_with(factor = sfl(top_counts = ~top_counts(., max_char = 100)))

# Dashboard
header <- dashboardHeader(title = "Shiny Data App", titleWidth = 300)

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(id = "sidebar_id",
    menuItem("About", tabName = "about"),
    menuItem("Data Exploration", tabName = "exp"),
    menuItem("Modeling", tabName = "model",
             menuSubItem("Modeling Info", tabName = "info"),
             menuSubItem("Model Fitting", tabName = "fit"),
             menuSubItem("Prediction", tabName = "pred")
    ),
    menuItem("Data", tabName = "data"),
    conditionalPanel(
      "input.sidebar_id != 'about'",
      fluidRow(
        column(
          width = 6, align = "center",
          actionButton("saveFilterButton","Save Filter")
        ),
        column(
          width = 6,
          actionButton("loadFilterButton","Load Filter")
        )
        
      ),
      filter_data_ui("filtering", max_height = 500),
      conditionalPanel(
        "input.sidebar_id == 'data'",
        br(),
        column(
          width = 12, align = "center",
          downloadButton("data_download")
        )
      )
    )
  )
)

body <- dashboardBody(
  conditionalPanel(
    "input.sidebar_id != 'about'",
    fluidRow(
      box(
        width = 12,
        checkboxGroupInput("select_vars", "Select Which Variables", names(neo),
                           inline = TRUE, selected = names(neo[]))
      
      )
    )
  ),
  tabItems(
    # About
    tabItem(
      tabName = "about",
      h2("About"),
      p("The purpose of this app is to explore the near earth objects dataset, 
              build a model and make predictions using that model to predict 
              if an object is hazardous"),
      p("The data set is from kaggle and can be found",
        a("here", href = "https://www.kaggle.com/datasets/ruchi798/data-science-job-neo"),
        "."),
      p("The data set includes an estimated minimum and maximum diameter, 
              relative velocity, miss distance, whether it is a sentry object, 
              absolute maginitude and whether the object was classified hazardous."),
      p(strong("Data Exploration"),
        "- This tab will allow you to explore the data using 
              numerical and graphical summaries"),
      p(strong("Modeling"),
        "- This tab has three sub tabs and will allow you to find information 
              about the models available (Modeling Info), fit the data using the models available (Model Fitting), 
              and to make predictions using one of the models (Prediction)."),
      p(strong("Data"),
        "- This tab will allow you to scroll through the data set, 
              subset the data and save the dataset"),
      img(src="dataset-cover.jpg")
    ),
    
    # Data Exploration
    tabItem(
      tabName = "exp",
      fluidRow(
        box(
          width = 12,
          column(
            width = 3,
            radioButtons("exp_type", "Type of Summary:",
                         c("Numeric" = "num",
                           "Graphical" = "graph")),
            conditionalPanel(
              "input.exp_type == 'num'",
              radioButtons("exp_summary_type", "Type of variable to summarize:",
                           c("Continuous" = "num",
                             "Discrete" = "logi"))
            ),
            conditionalPanel(
              "input.exp_type == 'graph'",
              radioButtons("exp_plot_type", "Type of plot:",
                           c("Univariate" = "one",
                             "Bivariate" = "two")),
              selectInput("exp_xaxis", "X-axis Variable:", names(neo)),
              conditionalPanel(
                "input.exp_plot_type == 'two'", 
                selectInput("exp_yaxis", "Y-axis Variable:", names(neo))
              ),
              conditionalPanel(
                "input.exp_plot_type == 'one' 
                  && (input.exp_xaxis != 'hazardous' )",
                sliderInput("exp_bins", "Number of Bins", 5, 50, 10)
              )
            )
          ),
          column(
            width = 9,
            conditionalPanel(
              "input.exp_type == 'num'",
              conditionalPanel(
                "input.exp_summary_type == 'num'",
                reactable::reactableOutput(outputId = "exp_table_numeric"),
              ),
              conditionalPanel(
                "input.exp_summary_type == 'logi'",
                reactable::reactableOutput(outputId = "exp_table_logical")
              )
            ),
            conditionalPanel(
              "input.exp_type == 'graph'",
              plotOutput("exp_plot")
            )
          )
        )
      )
    ),
    
    # Modeling Info
    tabItem(
      tabName = "info",
      h4("Generalized Linear Regression Model"),
      withMathJax(),
      p("Logistic Regression uses the log odds of success in order to 
            predict the probability of success.  In other words:"),
      helpText("$$P(hazardous|x_1,x_2,...)=\\frac{e^{\\beta_0+\\beta_1 x_1+\\beta_2 x_2,...}}
               {1+e^{\\beta_0+\\beta_1 x_1+\\beta_2 x_2,...}}$$"),
      p("Some of the advantages are that it is easy to implement, interpret and very efficient to train"),
      p("Some of the disadvantages are that it assumes linearity between the dependent variable and the independent variables"),
      h4("Classification Tree Model"),
      p("A classification tree starts by splitting the tree at the top, 
            and then procedes to split both sides of the tree so that you can 
            follow the splits down to get to your prediction.  
            For example, the first split might be:"),
      helpText("$$absolute\\_magnitiude <20$$"),
      p("and then further split that by "),
      helpText("$$relative\\_velocity<20000 ------ relative\\_velocity<25000$$"),
      p("and so on"),
      p("Some of the advantages are that it can capture nonlinear relationships 
            and that they are simple and so easy to understand"),
      p("Some of the disadvantages are that they can be easy to overfit and 
            sometimes small variations in the data can change a classification 
            tree significantly"),
      h4("Random Forest Model"),
      p("Random forest models start with the idea of bagging which is short 
            for bootstrap aggregation.  Bootstrapping resamples either from the data 
            (non-parametric) or a fitted model (parametric) and applies the method
            of estimation to each sample.  For classification trees, 
            we create a bootstrap sample, train the tree on that sample"),
      helpText("$$\\hat{y}^{*1}(x)$$"),
      p("repeat that process many times (say B=1000)"),
      helpText("$$\\hat{y}^{*j}(x),j=1,...,B$$"),
      p("and then makes a decision using all of the trees.  
            A Random forest method extends this by using a random subset of the 
            predictors for each bootstrap sample/tree fit"),
      p("This method has advatages over the classification tree by reducing 
            correlation resulting in a better reduction in variance than 
            classification trees."),
      p("One of the primary disadvatages is that this is much more 
            computationally heavy than either of our previous methods.")
    ),
    
    # Model Fitting
    tabItem(
      tabName = "fit",
      h2("Model Fitting")
    ),
    
    # Prediction
    tabItem(
      tabName = "pred",
      h2("Prediction")
    ),
    
    # Data
    tabItem(
      tabName = "data",
      fluidRow(
        box(
          width = 12,
          reactable::reactableOutput(outputId = "data_table")
        )
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  # Filtering Code
  savedFilterValues <- reactiveVal()
  
  observeEvent(input$saveFilterButton,{
    savedFilterValues <<- filtered_data$values()
  },ignoreInit = T)
  
  filter_defaults <- reactive({
    input$loadFilterButton
    savedFilterValues
  })
  
  filtered_data <- filter_data_server(
    id = "filtering",
    data = reactive(neo[,input$select_vars]),
    name = reactive("neo"),
    vars = reactive(NULL),
    defaults = filter_defaults,
    widget_num = "range",
    widget_date = "range",
    label_na = "Missing",
    drop_ids = FALSE
  )
  
  # Exploration Tab
  output$exp_table_logical <- reactable::renderReactable({
    reactable::reactable(
      partition(my_skim(filtered_data$filtered()))$logical %>% 
        select(-n_missing, -complete_rate) %>%
        rename(Variable = skim_variable,
               "Mean" = mean,
               "Count" = count)
    )
  })
  
  output$exp_table_numeric <- reactable::renderReactable({
    reactable::reactable(
      partition(skim_without_charts(filtered_data$filtered()))$numeric %>% 
        select(-n_missing, -complete_rate) %>%
        mutate(across(is.numeric, round, 5)) %>%
        rename("Name of Variable" = skim_variable,
               Mean = mean,
               "Standard Deviation" = sd,
               Minimum = p0,
               Median = p50,
               Maximum = p100)
    )
  })
  
  observeEvent(input$select_vars, {
    updateSelectInput(session, "exp_xaxis", choices = names(neo[,input$select_vars]))
    updateSelectInput(session, "exp_yaxis", choices = names(neo[,input$select_vars]))
  })
  
  output$exp_plot <- renderPlot({
    options(scipen=10000)
    g <- ggplot(filtered_data$filtered()) + 
      aes_string(x = input$exp_xaxis, fill = input$exp_xaxis) 
    if (input$exp_plot_type == "one") {
      if (is.numeric(neo[[input$exp_xaxis]]))
        g + geom_histogram(bins = input$exp_bins) 
      else
        g + geom_bar()
    }
    else {
      if (is.numeric(neo[[input$exp_xaxis]]) && is.numeric(neo[[input$exp_yaxis]]))
        g + aes_string(y = input$exp_yaxis) + geom_point() + geom_smooth()
      else
        g + aes_string(y = input$exp_yaxis) + geom_boxplot()
    }
  })
  
  # Data Tab
  output$data_table <- reactable::renderReactable({
    reactable::reactable(filtered_data$filtered())
  })
  
  output$data_download <- downloadHandler(
    filename = "neo.csv",
    content = function(file) {
      write_csv(filtered_data$filtered(), file)
    }
  )
}

shinyApp(ui, server)