## app.R ##
library(datamods)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
#library(DT)
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
header <- dashboardHeader(title = "Shiny Data App")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebar_id",
    menuItem("About", tabName = "about"),
    menuItem("Data Exploration", tabName = "exp"),
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
          checkboxGroupInput("exp_vars", "Select Which Variables", names(salaries),
                             inline = TRUE, selected = names(salaries))
        ),
        div(
          class = "col-sm-6 col-md-5 col-lg-4",
          box(
            width = "100%",
            radioButtons("exp_type", "Type of Summary:", inline = TRUE,
                         c("Numeric" = "num",
                           "Graphical" = "graph")),
            actionButton("exp_saveFilterButton","Save Filter Values"),
            actionButton("exp_loadFilterButton","Load Filter Values"),
            br(),
            filter_data_ui("exp_filtering")
          )
        ),
        div(
          class = "col-sm-6 col-md-7 col-lg-8",
          box(
            width = "100%",
            conditionalPanel(
              "input.exp_type == 'num'",
              radioButtons("exp_summary_type", "Type of variable to summarize:", inline = TRUE,
                           c("Continuous" = "num",
                             "Discrete" = "fact")),
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
              column(
                width = 4,
                radioButtons("exp_plot_type", "Type of plot:", inline = TRUE,
                             c("Univariate" = "one",
                               "Bivariate" = "two"))
              ),
              column(
                width = 4,
                conditionalPanel(
                  "input.exp_plot_type == 'one' 
                  && (input.exp_xaxis == 'salary' || input.exp_xaxis == 'salary_in_usd')",
                  sliderInput("exp_bins", "Number of Bins", 5, 50, 10)
                ),
                conditionalPanel(
                  "input.exp_plot_type == 'two'",
                  checkboxInput("exp_violin", "Violin")
                )
              ),
              column(
                width = 4,
                selectInput("exp_xaxis", "X-axis Variable:", names(salaries))
              ),
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
          checkboxGroupInput("data_vars", "Select Which Variables", names(salaries),
                             inline = TRUE, selected = names(salaries))
        ),
        div(
          class = "col-sm-6 col-md-5 col-lg-4",
          box(
            width = "100%",
            actionButton("data_saveFilterButton","Save Filter Values"),
            actionButton("data_loadFilterButton","Load Filter Values"),
            br(),
            filter_data_ui("data_filtering"),
            downloadButton("data_download")
          )
        ),
        div(
          class = "col-sm-6 col-md-7 col-lg-8",
          box(
            width = "100%",
            progressBar(
              id = "data_pbar", value = 100,
              total = 100, display_pct = TRUE
            ),
            reactable::reactableOutput(outputId = "data_table"),
          )
        )
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  # Exploration
  savedFilterValues <- reactiveVal()
  
  observeEvent(input$exp_saveFilterButton,{
    savedFilterValues <<- exp_filter$values()
  },ignoreInit = T)
  
  exp_defaults <- reactive({
    input$exp_loadFilterButton
    savedFilterValues
  })
  
  exp_filter <- filter_data_server(
    id = "exp_filtering",
    data = reactive(salaries[,input$exp_vars]),
    name = reactive("salaries"),
    vars = reactive(NULL),
    defaults = exp_defaults,
    widget_num = "range",
    widget_date = "range",
    label_na = "Missing",
    drop_ids = FALSE
  )
  
  output$exp_table_factor <- reactable::renderReactable({
    reactable::reactable(
      partition(my_skim(exp_filter$filtered()))$factor %>% 
        select(skim_variable, top_counts, n_unique, ordered) %>%
        rename(Variable = skim_variable,
               "Top Counts" = top_counts,
               "Number of Levels" = n_unique,
               Ordered = ordered)
    )
  })
  
  output$exp_table_numeric <- reactable::renderReactable({
    reactable::reactable(
      partition(skim_without_charts(exp_filter$filtered()))$numeric %>% 
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
  
  observeEvent(input$exp_vars, {
    updateSelectInput(session, "exp_xaxis", choices = names(salaries[,input$exp_vars]))
  })
  
  output$exp_plot <- renderPlot({
    g <- ggplot(exp_filter$filtered()) + aes_string(x = input$exp_xaxis, fill = input$exp_xaxis) 
    if (input$exp_plot_type == "one") {
      if (input$exp_xaxis == "salary" | input$exp_xaxis == "salary_in_usd")
        g + geom_histogram(bins = input$exp_bins)
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
  
  # Data
  observeEvent(input$data_saveFilterButton,{
    savedFilterValues <<- data_filter$values()
  },ignoreInit = T)
  
  data_defaults <- reactive({
    input$data_loadFilterButton
    savedFilterValues
  })
  
  data_filter <- filter_data_server(
    id = "data_filtering",
    data = reactive(salaries[,input$data_vars]),
    name = reactive("salaries"),
    vars = reactive(NULL),
    defaults = data_defaults,
    widget_num = "range",
    widget_date = "range",
    label_na = "Missing",
    drop_ids = FALSE
  )
  
  observeEvent(data_filter$filtered(), {
    updateProgressBar(
      session = session, id = "data_pbar",
      value = nrow(data_filter$filtered()), total = nrow(data())
    )
  })
  
  output$data_table <- reactable::renderReactable({
    reactable::reactable(data_filter$filtered())
  })
  
  output$data_download <- downloadHandler(
    filename = "ds_salaries.csv",
    content = function(file) {
      write_csv(data_filter$filtered(), file)
    }
  )
}

shinyApp(ui, server)