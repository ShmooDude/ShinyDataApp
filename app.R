## app.R ##
library(datamods)
library(shinydashboard)
library(tidyverse)
library(skimr)
library(mathjaxr)
library(caret)
library(parallel)
library(doSNOW)
library(rpart.plot)
library(waiter)
library(randomForest)

# Load CSV
neo <- read_csv("neo.csv", col_select = c(-1, -2, -7, -8))
neo$hazardous <- as.factor(neo$hazardous)

my_skim <- skim_with(factor = sfl(top_counts = ~top_counts(., max_char = 100)))

# Parallel Computing Setup
cores <- detectCores() - 1

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

body <- dashboardBody(
  autoWaiter(),
  useWaiter(),
  fluidRow(
    box(
      width = 12,
      checkboxGroupInput("select_vars", "Select Which Variables", names(neo),
                         inline = TRUE, selected = names(neo))
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
        a("here", href = "https://www.kaggle.com/datasets/sameepvani/nasa-nearest-earth-objects"),
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
                selectInput("exp_yaxis", "Y-axis Variable:", names(neo)),
                sliderInput("exp_size", "Size of Points:", 0.1, 1, 0.5),
                sliderInput("exp_alpha", "Alpha of Points:", 0.1, 1, 0.5)
              ),
              conditionalPanel(
                "input.exp_plot_type == 'one' 
                  && (input.exp_xaxis != 'hazardous' )",
                sliderInput("exp_bins", "Number of Bins:", 5, 50, 10)
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
      h4("Generalized Linear Regression Model"),
      withMathJax(),
      p("Logistic Regression is like linear regression except it uses the 
            log odds of success in order to predict the probability of success.  
            In other words:"),
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
      fluidRow(
        div(
          class = "col-sm-5 col-md-4 col-lg-3", 
          actionButton("model_run", "Create Models", width = "100%"),
          box(
            title = "Classification Tree Settings",
            width = "100%",
            collapsible = TRUE,
            sliderInput("model_rpart_range", "cp Range:", 
                        min = 0, 
                        max = 0.1, 
                        value = c(0, 0.1),
                        step = 0.001),
            numericInput("model_rpart_step", "cp Step:", 
                         value = 0.001, 
                         min = 0.0001, 
                         max = 0.01, 
                         step = 0.0001)            
          ),
          box(
            title = "Random Forest Settings",
            width = "100%",
            collapsible = TRUE,
            sliderInput("model_rf_mtry", "mtry:", 
                        value = 1,
                        min = 1, 
                        max = 6, 
                        step = 1)
          ),
          box(
            title = "Accuracy",
            width = "100%",
            collapsible = TRUE,
            tableOutput("model_accuracy")
          )
        ),
        div(
          class = "col-sm-7 col-md-8 col-lg-9", 
          tabBox(
            id = "model_tabbox",
            width = "100%",
            tabPanel(
              "Logistic Regression",
              verbatimTextOutput("model_glm_summary", placeholder = TRUE),
            ),
            tabPanel(
              "Classification Tree",
              plotOutput("model_rpart_plot"),
            ),
            tabPanel(
              "Random Forest",
              plotOutput("model_rf_plot"),
            ),
            tabPanel(
              "Fit Statistics",
              fluidRow(
                div(
                  class = "col-sm-12 col-md-12 col-lg-4", 
                  box(
                    title = "Logistic Regression",
                    width = "100%",
                    collapsible = TRUE,
                    verbatimTextOutput("model_glm_fit")
                  )
                ),
                div(
                  class = "col-sm-12 col-md-12 col-lg-4", 
                  box(
                    title = "Classification Tree",
                    width = "100%",
                    collapsible = TRUE,
                    verbatimTextOutput("model_rpart_fit")
                  )
                ),
                div(
                  class = "col-sm-12 col-md-12 col-lg-4", 
                  box(
                    title = "Random Forest",
                    width = "100%",
                    collapsible = TRUE,
                    verbatimTextOutput("model_rf_fit")
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # Prediction
    tabItem(
      tabName = "pred",
      h2("Prediction"),
      fluidRow(
        div(
          class = "col-sm-5 col-md-4 col-lg-3", 
          box(
            title = "Specify Values",
            width = "100%",
            conditionalPanel(
              "input.select_vars.includes('est_diameter_min')",
              sliderInput("pred_dia_min", "est_diameter_min", 
                          min = round(min(neo$est_diameter_min), 2),
                          max = round(quantile(neo$est_diameter_min, 0.75)*1.5, 2),
                          value = 0.13,
                          step = 0.01)
            ),
            conditionalPanel(
              "input.select_vars.includes('est_diameter_max')",
              sliderInput("pred_dia_max", "est_diameter_max", 
                          min = round(min(neo$est_diameter_max), 2),
                          max = round(quantile(neo$est_diameter_max, 0.75)*1.5, 2),
                          value = 0.28,
                          step = 0.01)
            ),
            conditionalPanel(
              "input.select_vars.includes('relative_velocity')",
              sliderInput("pred_rel_vel", "relative_velocity", 
                          min = round(min(neo$relative_velocity), 0),
                          max = round(max(neo$relative_velocity), 0),
                          value = 48000,
                          step = 1000)
            ),
            conditionalPanel(
              "input.select_vars.includes('miss_distance')",
              sliderInput("pred_mis_dis", "miss_distance", 
                          value = 37000000,
                          min = round(min(neo$miss_distance), 0),
                          max = round(max(neo$miss_distance), 0),
                          step = 1000000)
            ),
            conditionalPanel(
              "input.select_vars.includes('absolute_magnitude')",
              sliderInput("pred_abs_mag", "absolute_magnitude", 
                          min = round(min(neo$absolute_magnitude), 0),
                          max = round(max(neo$absolute_magnitude), 0),
                          value = 24,
                          step = 1)
            ),
          )
        ),
        div(
          class = "col-sm-7 col-md-8 col-lg-9", 
          fluidRow(
            div(
              class = "col-sm-12 col-md-12 col-lg-4", 
              box(
                title = "Logistic Regression",
                width = "100%",
                collapsible = TRUE,
                tableOutput("pred_glm")
              )
            ),
            div(
              class = "col-sm-12 col-md-12 col-lg-4", 
              box(
                title = "Classification Tree",
                width = "100%",
                collapsible = TRUE,
                tableOutput("pred_rpart")
              )
            ),
            div(
              class = "col-sm-12 col-md-12 col-lg-4", 
              box(
                title = "Random Forest",
                width = "100%",
                collapsible = TRUE,
                tableOutput("pred_rf")
              )
            )
          )
        )
      )
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
  saved_filter_values <- reactiveVal()
  
  observeEvent(input$saveFilterButton,{
    saved_filter_values <<- filtered_data$values()
  },ignoreInit = T)
  
  filter_defaults <- reactive({
    input$loadFilterButton
    saved_filter_values
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
  output$exp_table_factor <- reactable::renderReactable({
    reactable::reactable(
      partition(my_skim(filtered_data$filtered()))$factor %>% 
        select(skim_variable, top_counts) %>%
        rename(Variable = skim_variable,
               "Counts" = top_counts)
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
  
  # Univariate and Bivariate plots
  output$exp_plot <- renderPlot({
    g <- ggplot(filtered_data$filtered()) + 
      aes_string(x = input$exp_xaxis)
    
    if (input$exp_plot_type == "one") {
      if (is.numeric(neo[[input$exp_xaxis]]))
        g + geom_histogram(bins = input$exp_bins,
                           aes(fill = hazardous))
      else
        g + geom_bar(aes(fill = hazardous))
    }
    else {
      if (is.numeric(neo[[input$exp_xaxis]]) && is.numeric(neo[[input$exp_yaxis]]))
        g + aes_string(y = input$exp_yaxis) + 
        geom_point(size = input$exp_size, 
                   alpha = input$exp_alpha,
                   aes(color = hazardous, fill = hazardous)) + 
        geom_smooth(aes(linetype = hazardous)) +
        guides(color = guide_legend(override.aes = list(size=1, alpha=1), order = 1))
      else
        g + aes_string(y = input$exp_yaxis) + geom_boxplot()
    }
  })
  
  # Model Tab
  model_storage <- eventReactive(input$model_run, {
    if (is.null(filtered_data$filtered()$hazardous))
      stop("You cannot create models without the 'hazardous' variable.")
    if (length(filtered_data$filtered())<2)
      stop("You cannot create models without predictor variables.")
    # Split the data set
    neoIndex <- createDataPartition(filtered_data$filtered()$hazardous, p = 0.7, list = FALSE)
    neoTrain <- filtered_data$filtered()[neoIndex,]
    neoTest <- filtered_data$filtered()[-neoIndex,]
    
    model <- list(neoTrain = neoTrain, neoTest = neoTest)
    
    # Start Parallel Computing
    cl <- makeCluster(cores)
    registerDoSNOW(cl)
    
    withProgress(
      message = "Running Models", value = 0, {
        incProgress(0, detail = "Logistic Model")
        
        # Train the GLM model
        model$glm_model <- train(form = as.factor(hazardous) ~ ., 
                                data = neoTrain,
                                family = binomial, 
                                preProcess = c("center", "scale"),
                                trControl = trainControl(method = "cv", number = 5),
                                method = "glm")
        incProgress(amount = 0.33, detail = "Classification Tree")
        
        # Train the Classification Tree model
        model$rpart_model <- train(form = as.factor(hazardous) ~ ., 
                                  data = neoTrain,
                                  preProcess = c("center", "scale"),
                                  trControl = trainControl(method = "cv", number = 5),
                                  method = "rpart",
                                  tuneGrid = data.frame(cp = seq(input$model_rpart_range[1], 
                                                                 input$model_rpart_range[2], 
                                                                 input$model_rpart_step)))
        incProgress(amount = 0.33, detail = "Random Forest")
        
        # Train the Random Forest model
        model$rf_model <- train(form = as.factor(hazardous) ~ .,
                               data = neoTrain,
                               preProcess = c("center", "scale"),
                               trControl = trainControl(method = "cv", number = 5),
                               method = "rf",
                               tuneGrid = data.frame(mtry = input$model_rf_mtry))  
        incProgress(amount = 0.33, detail = "Confusion Matrix")
        
        # Generate Confusion Matrix for Fit Statistics
        model$glmCm <- confusionMatrix(data = model$neoTest$hazardous, 
                                       reference = predict(model$glm_model, 
                                                           newdata = model$neoTest))
        model$rpartCm <- confusionMatrix(data = model$neoTest$hazardous, 
                                         reference = predict(model$rpart_model, 
                                                             newdata = model$neoTest))
        model$rfCm <- confusionMatrix(data = model$neoTest$hazardous, 
                                      reference = predict(model$rf_model, 
                                                          newdata = model$neoTest))
      })

    
    # Stop Parallel Computing
    stopCluster(cl)
    rm(cl)
    gc()
    
    # Save List
    model
  })
  
  # Update Model Information on Run
  observeEvent(input$model_run, {
    output$model_glm_summary <- renderPrint({
      model <- model_storage()
      summary(model$glm_model)
    })
    
    output$model_glm_fit <- renderPrint({
      model <- model_storage()
      model$glmCm
    })
    
    output$model_rpart_plot <- renderPlot({
      model <- model_storage()
      rpart.plot(model$rpart_model$finalModel)
    })
    
    output$model_rpart_fit <- renderPrint({
      model <- model_storage()
      model$rpartCm
    })
    
    output$model_rf_plot <- renderPlot({
      model <- model_storage()
      varImpPlot(model$rf_model$finalModel)
    })
    
    output$model_rf_fit <- renderPrint({
      model <- model_storage()
      model$rfCm
    })
    
    output$model_accuracy <- renderTable({
      model <- model_storage()
      data.frame("Model" = c("Logistic Regression",
                             "Classification Tree",
                             "Random Forest"), 
                 "Accuracy" = c(model$glmCm$overall["Accuracy"],
                                model$rpartCm$overall["Accuracy"],
                                model$rfCm$overall["Accuracy"]))
    },
    digits = 3)
  })
  
  # Prediction Tab
  observe({
    output$pred_glm <- renderTable({
      model <- model_storage()
      predict(model$glm_model, data.frame(est_diameter_min = input$pred_dia_min,
                                          est_diameter_max = input$pred_dia_max,
                                          relative_velocity = input$pred_rel_vel,
                                          miss_distance = input$pred_mis_dis,
                                          absolute_magnitude = input$pred_abs_mag))
    },
    colnames = FALSE)
    
    output$pred_rpart <- renderTable({
      model <- model_storage()
      predict(model$rpart_model, data.frame(est_diameter_min = input$pred_dia_min,
                                            est_diameter_max = input$pred_dia_max,
                                            relative_velocity = input$pred_rel_vel,
                                            miss_distance = input$pred_mis_dis,
                                            absolute_magnitude = input$pred_abs_mag))
    },
    colnames = FALSE)
    
    output$pred_rf <- renderTable({
      model <- model_storage()
      predict(model$rf_model, data.frame(est_diameter_min = input$pred_dia_min,
                                         est_diameter_max = input$pred_dia_max,
                                         relative_velocity = input$pred_rel_vel,
                                         miss_distance = input$pred_mis_dis,
                                         absolute_magnitude = input$pred_abs_mag))
    },
    colnames = FALSE)
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