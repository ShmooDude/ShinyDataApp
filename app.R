## app.R ##
library(datamods)
library(shinydashboard)
library(tidyverse)
library(skimr)
library(magrittr)

# Load CSV
salaries <- read_csv("ds_salaries.csv", col_select = -1, col_types = "-ccffnfnfcfc")
salaries$work_year %<>% factor(levels = c(2020, 2021, 2022),
                               ordered = TRUE)
salaries$experience_level %<>% factor(levels = c("EN", "MI", "SE", "EX"),
                                      ordered = TRUE)
salaries$remote_ratio %<>% factor(levels = c(0, 50, 100),
                                  ordered = TRUE)
salaries$company_size %<>% factor(levels = c("S", "M", "L"),
                                  ordered = TRUE)

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
        checkboxGroupInput("select_vars", "Select Which Variables", names(salaries),
                           inline = TRUE, selected = names(salaries))
      
      )
    )
  ),
  tabItems(
    # About
    tabItem(
      tabName = "about",
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
              subset the data and save the dataset"),
      img(src="dataset-cover.png")
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
                             "Discrete" = "fact"))
            ),
            conditionalPanel(
              "input.exp_type == 'graph'",
              selectInput("exp_xaxis", "X-axis Variable:", names(salaries)),
              radioButtons("exp_plot_type", "Type of plot:",
                           c("Univariate" = "one",
                             "Bivariate" = "two")),
              conditionalPanel(
                "input.exp_plot_type == 'one' 
                  && (input.exp_xaxis == 'salary' || input.exp_xaxis == 'salary_in_usd')",
                sliderInput("exp_bins", "Number of Bins", 5, 50, 10)
              ),
              conditionalPanel(
                "input.exp_plot_type == 'two'",
                checkboxInput("exp_violin", "Violin")
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
                "input.exp_summary_type == 'fact'",
                reactable::reactableOutput(outputId = "exp_table_factor")
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
      h2("Modeling Info")
      
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
    data = reactive(salaries[,input$select_vars]),
    name = reactive("salaries"),
    vars = reactive(NULL),
    defaults = filter_defaults,
    widget_num = "range",
    widget_date = "range",
    label_na = "Missing",
    drop_ids = FALSE
  )
  
  # Exploration Tab
  output$exp_table_factor <- reactable::renderReactable({
    reactable::reactable(
      partition(my_skim(filtered_data$filtered()))$factor %>% 
        select(skim_variable, top_counts, n_unique, ordered) %>%
        rename(Variable = skim_variable,
               "Top Counts" = top_counts,
               "Number of Levels" = n_unique,
               Ordered = ordered)
    )
  })
  
  output$exp_table_numeric <- reactable::renderReactable({
    reactable::reactable(
      partition(skim_without_charts(filtered_data$filtered()))$numeric %>% 
        select(-n_missing, -complete_rate) %>%
        mutate(across(mean:sd, round, 1)) %>%
        rename(Variable = skim_variable,
               Mean = mean,
               "Standard Deviation" = sd,
               Minimum = p0,
               Median = p50,
               Maximum = p100)
    )
  })
  
  observeEvent(input$select_vars, {
    updateSelectInput(session, "exp_xaxis", choices = names(salaries[,input$select_vars]))
  })
  
  output$exp_plot <- renderPlot({
    g <- ggplot(filtered_data$filtered()) + 
      aes_string(x = input$exp_xaxis, fill = input$exp_xaxis) + 
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
    if (input$exp_plot_type == "one") {
      if (input$exp_xaxis == "salary" | input$exp_xaxis == "salary_in_usd")
        g + geom_histogram(bins = input$exp_bins) + 
        scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
      else
        g + geom_bar()
    }
    else {
      if (input$exp_violin)
        g + aes(y = salary_in_usd) + geom_violin()
      else
        g + aes(y = salary_in_usd) + geom_boxplot()
    }
  })
  
  # Data Tab
  output$data_table <- reactable::renderReactable({
    reactable::reactable(filtered_data$filtered())
  })
  
  output$data_download <- downloadHandler(
    filename = "ds_salaries.csv",
    content = function(file) {
      write_csv(filtered_data$filtered(), file)
    }
  )
}

shinyApp(ui, server)