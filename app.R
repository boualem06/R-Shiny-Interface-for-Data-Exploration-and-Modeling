# Load necessary libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
library(glmnet)
library(corrplot)
library(GGally)
library(pROC)

#install.packages("ggplot2")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dataset Overview"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Upload Dataset", tabName = "upload", icon = icon("upload")),
      menuItem("Dataset Show", tabName = "dataset_show", icon = icon("table")),
      menuItem("Dataset Preview", tabName = "dataset_preview", icon = icon("search")),
      # menuItem("Impute", tabName = "impute", icon = icon("calculator")),
      menuItem("Visualisation", tabName = "visualisation", icon = icon("chart-bar")), # New Visualisation menu
      menuItem("Univariate Analysis", tabName = "univariate", icon = icon("chart-bar")),
      menuItem("Bivariate Analysis", tabName = "Bivariate", icon = icon("chart-bar")),
      menuItem("Correlation Analysis", tabName = "correlation", icon = icon("chart-line")),
      menuItem("Preprocessing", tabName = "preprocessing", icon = icon("filter")),
      menuItem("Model Selection", tabName = "model", icon = icon("chart-bar")),
      menuItem("Model Training", tabName = "train", icon = icon("cogs")),
      menuItem("Model Visualization", tabName = "viz", icon = icon("eye"))
      
      
    )
  ),
  dashboardBody(
    #fluidRow(
     # column(
      #  width = 12,
       # actionButton("btn_impute", "Go to Impute", class = "btn-primary"),
        #actionButton("btn_dataset_show", "Go to Dataset Show", class = "btn-info"),
        #actionButton("btn_dataset_preview", "Go to Dataset Preview", class = "btn-success"),
        #br(), br()
      #)
    #),
    
    tabItems(
      tabItem(tabName = "upload",
              #h3("Upload Section"),
              
              fluidRow(
                box(title = "Upload Your Dataset", status = "primary", solidHeader = TRUE, width = 6,
                    fileInput("file", "Choose CSV File", accept = c(".csv")),
                    helpText("Please upload a CSV file to analyze.")
                ),
                box(title = "Dataset Information", status = "info", solidHeader = TRUE, width = 6,
                    h4("Summary"),
                    verbatimTextOutput("summary")
                )
              ),
              fluidRow(
                valueBoxOutput("rowsBox", width = 3),
                valueBoxOutput("colsBox", width = 3),
                valueBoxOutput("categoricalBox", width = 3),
                valueBoxOutput("numericalBox", width = 3)
              ),
              fluidRow(
                valueBoxOutput("missingBox", width = 3),
                valueBoxOutput("outliersBox", width = 3)
              ),
              fluidRow(
                valueBoxOutput("searchBox", width = 12)
              ),
              fluidRow(
                valueBoxOutput("imputationBox", width = 12)
              ),
             
         ),
      
      #tabItem(tabName = "impute",
              #h3("Impute Section"),
              
       #      fluidRow(
        #        box(title = "Imputation Options", status = "warning", solidHeader = TRUE, width = 12,
         #           uiOutput("imputationUI"),
          #          actionButton("impute", "Apply Imputation", class = "btn-primary")
           #     )
            #  ),
             # fluidRow(
              #  box(title = "Download Imputed Dataset", status = "success", solidHeader = TRUE, width = 12,
               #     downloadButton("downloadData", "Download Imputed Dataset")
                #)
              #),
      #),
      
      tabItem(tabName = "dataset_show",
              
              #h3("Dataset Show Section"),
              
              fluidRow(
                box(title = "Dataset Rows ", status = "info", solidHeader = TRUE, width = 12,
                    DTOutput("dataPreview"),
                    actionButton("showAll", "Show All", class = "btn-primary")
                )
              ),
      ),
      
      tabItem(tabName = "dataset_preview",
              #h3("Dataset Summary Section"),
              
              fluidRow(
                box(
                  title = "Dataset Summary", 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 12,
                  #actionButton("generate_summary", "Generate Summary", class = "btn-primary"),
                  DTOutput("summaryTable")
                )
              )
      ),
      #),
      
      #===========================================================page of visualizations====================================================
      
      tabItem(tabName = "visualisation", # New tab for visualizations
              fluidRow(
                box(title = "Visualisation Options", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("var_x", "X-axis Variable:", choices = NULL),
                    selectInput("var_y", "Y-axis Variable:", choices = NULL),
                    selectInput("plot_type", "Select Plot Type:", 
                                choices = c("Scatter Plot" = "scatter", 
                                            "Bar Plot" = "bar", 
                                            "Histogram" = "hist",
                                            "Box Plot" = "box")),
                    actionButton("generate_plot", "Generate Plot", class = "btn-success")
                )
              ),
              fluidRow(
                box(title = "Plot Output", status = "info", solidHeader = TRUE, width = 12,
                    plotOutput("plot")
                )
              )
      ),
      
      tabItem(tabName = "univariate",
              
              fluidRow(
                
                box(title = "Univariate Analysis", status = "primary", solidHeader = TRUE, width = 12,
                    
                    selectInput("data_type_uni", "Data Type:", choices = c("Categorical", "Numerical")),
                    
                    selectInput("univariate_variable", "Select Variable:", choices = NULL),
                    
                    uiOutput("univariate_plots")  # Dynamically render plots
                    
                )
              )
        ),
      
      tabItem(tabName = "Bivariate",
              
              fluidRow(
                
                box(title = "Bivariate Analysis", status = "primary", solidHeader = TRUE, width = 12,
                    
                    selectInput("data_type", "Data Type:", choices = c("Categorical vs Categorical", "Numerical vs Numerical", "Categorical vs Numerical")),
                    
                    selectInput("bivar_var1", "Select Variable 1:", choices = NULL),
                    
                    selectInput("bivar_var2", "Select Variable 2:", choices = NULL),
                    
                    uiOutput("bivariate_plots"),  # Dynamically render plots
                    
                    
                    conditionalPanel(       # Wrap force table in conditionalPanel
                      
                      condition = "input.data_type == 'Categorical vs Categorical'",
                      
                      tableOutput("force")
                      
                    ),
                  
                    conditionalPanel(       # Wrap contingency table in conditionalPanel
                      
                      condition = "input.data_type == 'Categorical vs Categorical'",
                      
                      tableOutput("contingency")
                      
                    )
                    
                )
                
              )
      ),
      #=========================================page for  data processing ==========================================
      tabItem(tabName = "preprocessing",
              h3("Data Preprocessing"),
              
              fluidRow(
                box(title = "Imputation Options", status = "warning", solidHeader = TRUE, width = 12,
                    uiOutput("imputationUI"),
                    actionButton("impute", "Apply Imputation", class = "btn-primary")
                )
              ),
              #fluidRow(
              #  box(title = "Download Imputed Dataset", status = "success", solidHeader = TRUE, width = 12,
               #     downloadButton("downloadData", "Download Imputed Dataset")
              #  )
              #),
              
              
              fluidRow(
                # Normalization Box
                box(
                  title = "Normalization", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  selectInput("normalization_method", "Select Normalization Method",
                              choices = c(
                                "None" = "none",
                                "Min-Max Scaling" = "minmax",
                                "Z-Score Standardization" = "zscore",
                                "Robust Scaling" = "robust"
                              )
                  ),
                  selectInput("normalization_columns", "Select Columns to Normalize", 
                              choices = NULL, 
                              multiple = TRUE
                  ),
                  actionButton("apply_normalization", "Apply Normalization", class = "btn-success")
                ),
                
                # Outlier Handling Box
                box(
                  title = "Outlier Treatment", 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 12,
                  selectInput("outlier_method", "Select Outlier Treatment Method",
                              choices = c(
                                "None" = "none",
                                "Remove Outliers" = "remove",
                                "Winsorization" = "winsor",
                                "Capping" = "capping"
                              )
                  ),
                  numericInput("outlier_threshold", "Outlier Threshold (Z-score)", 
                               value = 3, min = 1, max = 5, step = 0.5
                  ),
                  selectInput("outlier_columns", "Select Columns for Outlier Treatment", 
                              choices = NULL, 
                              multiple = TRUE
                  ),
                  actionButton("apply_outlier_treatment", "Apply Outlier Treatment", class = "btn-info")
                ),
                
                # Categorical Encoding Box
                box(
                  title = "Categorical Encoding", 
                  status = "success", 
                  solidHeader = TRUE, 
                  width = 12,
                  selectInput("encoding_method", "Select Encoding Method",
                              choices = c(
                                "None" = "none",
                                "One-Hot Encoding" = "onehot",
                                "Label Encoding" = "label"
                              )
                  ),
                  selectInput("encoding_columns", "Select Columns to Encode", 
                              choices = NULL, 
                              multiple = TRUE
                  ),
                  actionButton("apply_encoding", "Apply Encoding", class = "btn-success")
                )
              ),
              
              # Preprocessed Data Preview
              fluidRow(
                box(
                  title = "Preprocessed Data Preview", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  DTOutput("preprocessed_data_preview"),
                  downloadButton("download_preprocessed_data", "Download Preprocessed Data")
                )
              )
      ),
      
      #================================= Model selection ======================================================
      # Model Selection Tab
      tabItem(tabName = "model",
              fluidRow(
                box(
                  title = "Select Model", status = "primary", solidHeader = TRUE,
                  selectInput("model_type", "Choose Model:",
                              choices = c(
                                "Random Forest" = "rf",
                                "Support Vector Machine" = "svm",
                                "Logistic Regression" = "glm",
                                "Linear Regression" = "lm"
                              )
                  )
                ),
                box(
                  title = "Model Parameters", status = "warning", solidHeader = TRUE,
                  uiOutput("model_params")
                )
              )
      ),
    
      # ========================================= Model training ========================================================
      # Model Training Tab
      tabItem(tabName = "train",
              fluidRow(
                box(
                  title = "Training Setup", status = "primary", solidHeader = TRUE,
                  selectInput("target_var", "Select Target Variable:", choices = NULL),
                  selectInput("train_vars", "Select Predictor Variables:", 
                              choices = NULL, multiple = TRUE),
                  sliderInput("train_ratio", "Training Data Ratio", 
                              min = 0.5, max = 0.9, value = 0.7, step = 0.05)
                ),
                box(
                  title = "Training Results", status = "warning", solidHeader = TRUE,
                  verbatimTextOutput("model_results")
                )
              ),
              fluidRow(textOutput("result")),
              fluidRow( # ROC curve plot
                plotOutput("roc_plot", height = "500px"),
                
                # Confusion matrix
                h4("Confusion Matrix"),
                verbatimTextOutput("conf_matrix"))
      ),
      #=============================================Model viusalization===================================================================
      # Model Visualization Tab
      tabItem(tabName = "viz",
              fluidRow(
                box(
                  title = "Model Visualization", status = "primary", solidHeader = TRUE,
                  plotOutput("model_plot")
                )
              )
      ),
      
      #=====================================================Correlation analysis=============================================================================
      # New Correlation Analysis Tab
      tabItem(tabName = "correlation",
              fluidRow(
                box(
                  title = "Correlation Heatmap", status = "primary", solidHeader = TRUE,
                  plotOutput("correlation_heatmap")
                ),
                box(
                  title = "Correlation Matrix Controls", status = "warning", solidHeader = TRUE,
                  selectInput("corr_method", "Correlation Method", 
                              choices = c("Pearson" = "pearson", 
                                          "Spearman" = "spearman", 
                                          "Kendall" = "kendall")),
                  selectInput("corr_plot_type", "Visualization Type", 
                              choices = c("Circle" = "circle", 
                                          "Pie" = "pie", 
                                          "Square" = "square", 
                                          "Heatmap" = "color", 
                                          "Mixed" = "mixed"))
                )
              ),
              fluidRow(
                box(
                  title = "Scatter Plot Matrix", status = "primary", solidHeader = TRUE,
                  plotOutput("scatter_matrix")
                )
              )
      )
      # =================================================================================================
      
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data storage
  data <- reactiveVal(NULL)
  
  # Reactive model storage
  trained_model <- reactiveVal(NULL)
  training_data <- reactiveVal(NULL)

  #================================ buttons for the navigations in the ulpoad page ========
  
  # Navigation Logic for Buttons
  observeEvent(input$btn_impute, {
    updateTabItems(session, "tabs", selected = "impute")
  })
  
  observeEvent(input$btn_dataset_show, {
    updateTabItems(session, "tabs", selected="dataset_show")
  })
  
  observeEvent(input$btn_dataset_preview, {
    updateTabItems(session, "tabs", selected="dataset_preview")
  })
#======================================= Dataset loading ====================================

  
  dataset <- reactive({
    req(input$file)
    tryCatch({
      data <- read.csv(input$file$datapath)
      
      # Force character columns to be factors if needed
      #data <- data %>%
      #mutate(across(where(is.character), as.factor))
      
      # Set default target as the last column
      default_target <- names(data)[ncol(data)] # Last column name
      
      # Update variable selection inputs
      updateSelectInput(session, "target_var", 
                        choices = names(data), 
                        selected = default_target
      ) 
      
      updateSelectInput(session, "train_vars", 
                        choices = names(data), 
                        selected=head(names(data), -1)
                        
                        #selected =NULL
      )
      
      return(data)
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        "The file could not be read. Please make sure it is a valid CSV file.",
        easyClose = TRUE
      ))
      NULL
    })
  })
  
  
  
  
  observe({
    req(get_current_dataset())
    # Get the current dataset
    current_dataset <- get_current_dataset()

    # Trigger actions or modify other variables when the dataset changes
    if (!is.null(current_dataset)) {
      # Example actions you might want to take
      default_target <- names(current_dataset)[ncol(current_dataset)] # Last column name
      
      # Update input variables based on the new dataset
      updateSelectInput(session, "target_var",
                        choices = names(current_dataset),
                        #selected = default_target
                        selected = default_target

      )

      updateSelectInput(session, "train_vars",
                        choices = names(current_dataset),
                        #selected=head(names(current_dataset), -1)
                        selected=head(names(current_dataset), -1)
                        #selected =NULL
      )


      print("Dataset has changed. Updating related variables.")
    }
  })

  
  
  output$rowsBox <- renderValueBox({
    req(dataset())
    valueBox(
      nrow(dataset()), "Number of Rows",
      icon = icon("table"), color = "blue"
    )
  })
  
  output$colsBox <- renderValueBox({
    req(dataset())
    valueBox(
      ncol(dataset()), "Number of Variables",
      icon = icon("columns"), color = "green"
    )
  })
  
  output$categoricalBox <- renderValueBox({
    req(dataset())
    num_categorical <- sum(sapply(dataset(), function(x) is.factor(x) || is.character(x)))
    valueBox(
      num_categorical, "Categorical Variables",
      icon = icon("tags"), color = "purple"
    )
  })
  
  output$numericalBox <- renderValueBox({
    req(dataset())
    num_numerical <- sum(sapply(dataset(), function(x) is.numeric(x) || is.integer(x)))
    valueBox(
      num_numerical, "Numerical Variables",
      icon = icon("chart-bar"), color = "orange"
    )
  })
  
  output$missingBox <- renderValueBox({
    req(dataset())
    missing_values_count <- sapply(dataset(), function(x) sum(is.na(x) | x == ""))
    
    # Calculate the total number of missing values
    total_missing_values <- sum(missing_values_count)
    
    #num_missing <- sum(is.na(dataset()))
    valueBox(
      total_missing_values, "Missing Values",
      icon = icon("exclamation-circle"), color = "red"
    )
  })
  
  output$outliersBox <- renderValueBox({
    req(dataset())
    data <- dataset()
    
    # Identify outliers for numerical columns
    is_outlier <- function(x) {
      if (is.numeric(x)) {
        Q1 <- quantile(x, 0.25, na.rm = TRUE)
        Q3 <- quantile(x, 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        sum(x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR), na.rm = TRUE)
      } else {
        0
      }
    }
    
    # Total outliers in all numerical columns
    num_outliers <- sum(sapply(data, is_outlier))
    
    valueBox(
      num_outliers, "Number of Outliers",
      icon = icon("exclamation-triangle"), color = "yellow"
    )
  })
  
  output$summary <- renderPrint({
    req(dataset())
    data <- dataset()
    cat("Rows:", nrow(data), "\n")
    cat("Columns:", ncol(data), "\n")
    cat("Categorical Variables:", sum(sapply(data, class) %in% c("factor", "character")), "\n")
    cat("Numerical Variables:", sum(sapply(data, class) %in% c("integer", "numeric")), "\n")
    cat("Missing Values:", sum(is.na(data)), "\n")
  })
  
  
  #=================================== Data visualization ===============================================================
  
  # Populate X and Y dropdowns based on dataset
  observe({
    req(dataset())
    #data <- dataset()
    data<-get_current_dataset()
    
    updateSelectInput(session, "var_x", choices = names(data))
    updateSelectInput(session, "var_y", choices = names(data))
  })
  
  # Generate plot based on user inputs
  output$plot <- renderPlot({
    req(dataset(), input$var_x, input$plot_type)
    data <- dataset()
    
    plot_type <- input$plot_type
    var_x <- input$var_x
    var_y <- input$var_y
    
    if (plot_type == "scatter") {
      req(input$var_y) # Y-axis is required for scatter plot
      ggplot(data, aes_string(x = var_x, y = var_y)) +
        geom_point() +
        theme_minimal()
    } else if (plot_type == "bar") {
      ggplot(data, aes_string(x = var_x)) +
        geom_bar() +
        theme_minimal()
    } else if (plot_type == "hist") {
      ggplot(data, aes_string(x = var_x)) +
        geom_histogram(binwidth = 30, fill = "blue", color = "white") +
        theme_minimal()
    }else if (plot_type == "box") {
      req(input$var_y) # Y-axis is required for box plot
      ggplot(data, aes_string(x = var_x, y = var_y)) +
        geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotates x-axis labels
        labs(title = "Box Plot", x = var_x, y = var_y) # Adds custom labels and title
    }
  })
  
  
  
  ################Univariate Analysis#######################
  
  # Update univariate variable selection
  
  # Update variable selections based on data type
  
  observeEvent(input$data_type_uni, {
    
    req(get_current_dataset())
    
    data <- get_current_dataset()
    
    if (input$data_type_uni == "Categorical") {
      
      cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
      
    } else {  # Numerical
      
      cols <- names(data)[sapply(data, is.numeric)]
      
    }
    
    updateSelectInput(session, "univariate_variable", choices = cols)
    
  })
  
  
  
  # Dynamically render plots based on data type
  
  output$univariate_plots <- renderUI({
    
    req(input$data_type_uni, get_current_dataset(), input$univariate_variable)
    
    if (input$data_type_uni == "Numerical") {
      
      tagList(
      
        plotOutput("effectifsDiag"),
      
        plotOutput("effectifsCumDiag"),
      
        plotOutput("boiteMoustaches")
      )
      
    } else {  # Categorical
      tagList(
        plotOutput("secteurs"),
        
        plotOutput("barre")
      )
      
    }
    
  })
  

  
  # Univariate Plots Numerical
  
  output$effectifsDiag <- renderPlot({
    
    req(get_current_dataset(), input$univariate_variable)
    
    selected_var <- get_current_dataset()[[input$univariate_variable]]
    
    plot(table(selected_var), col ="green4", xlab = input$univariate_variable,
         
         ylab ="Effectifs", main = paste("Distribution of", input$univariate_variable))
    
  })
  
  
  
  output$effectifsCumDiag <- renderPlot({
    
    req(get_current_dataset(), input$univariate_variable)
    
    selected_var <- get_current_dataset()[[input$univariate_variable]]
    
    plot(ecdf(as.numeric(as.character(selected_var))),
         
         col ="green4", xlab = input$univariate_variable,
         
         ylab ="Cumulative Frequencies", main = paste("Cumulative Frequencies of", input$univariate_variable))
    
  })
  
  
  
  output$boiteMoustaches <- renderPlot({
    
    req(get_current_dataset(), input$univariate_variable)
    selected_var <- get_current_dataset()[[input$univariate_variable]]
    
    boxplot(selected_var, col = grey(0.8),
            
            main = paste("Boxplot of", input$univariate_variable),
            
            ylab = input$univariate_variable, las = 1)
    
    rug(selected_var, side = 2)
    
  })
  
  
  
  # Univariate Plot Categorical
  
  # Diagramme en secteurs
  output$secteurs <- renderPlot({
    
    req(get_current_dataset(), input$univariate_variable)
    data <- get_current_dataset()
    selected_var <- data[[input$univariate_variable]]
    
    effectifs <- table(selected_var)
    
    pie(effectifs, labels = substr(names(effectifs), 1, 20), 
        main = input$univariate_variable, col=c())
  })
 
  
  output$barre <- renderPlot({
    
    req(get_current_dataset(), input$univariate_variable)
    data <- get_current_dataset()
    
    ggplot(data, aes_string(x=input$univariate_variable)) + geom_bar()
 })
  
  ####################################Bivariate########################
  
  
   # Update variable selections based on data type

  observeEvent(input$data_type, {

    req(get_current_dataset())

    data <- get_current_dataset()

    if (input$data_type == "Categorical vs Categorical") {

      cols1 <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
      cols2 <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]

    } else if (input$data_type == "Numerical vs Numerical") {  

      cols1 <- names(data)[sapply(data, is.numeric)]
      cols2 <- names(data)[sapply(data, is.numeric)]

    } else {    #Categorical vs Numerical
      
      cols1 <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
      cols2 <- names(data)[sapply(data, is.numeric)]
      
    }

    updateSelectInput(session, "bivar_var1", choices = cols1)

    updateSelectInput(session, "bivar_var2", choices = cols2)

  })
  
  # Dynamically render plots based on data type
  
  output$bivariate_plots <- renderUI({
    
    req(input$data_type, get_current_dataset(), input$bivar_var1, input$bivar_var2)
    
    if (input$data_type == "Categorical vs Categorical") {
        
        plotOutput("barplotDodgeBi")

      
    } else if (input$data_type == "Numerical vs Numerical") {  
      
      tagList(
        
        plotOutput("scatterplot"),
        
        plotOutput("boxplot_comparison")
      )
      
    } else {   # Categorical vs Numerical
      tagList(
        plotOutput("boxplotmultimodal"),
        textOutput("correlation_multimodal")
      )
    }
    
  })
  
  
  ###Categorical Plots

  output$barplotDodgeBi <- renderPlot({
    
    req(get_current_dataset(), input$bivar_var1, input$bivar_var2)
    
    data <- get_current_dataset()
    
    ggplot(data, aes_string(x = input$bivar_var1, fill = input$bivar_var2)) +
      
      geom_bar(position = "dodge")+
      
      labs(title = paste("Grouped Barplot of", input$bivar_var1, "by", input$bivar_var2),
           
           x = input$bivar_var1, fill = input$bivar_var2)
    
  })
  
  
  
  output$contingency <- renderTable({
    
    req(get_current_dataset(), input$bivar_var1, input$bivar_var2)
    
    data <- get_current_dataset()
    
    with(data, table(get(input$bivar_var2), get(input$bivar_var1))) # Use column names dynamically
    
  })
  
  
  
  output$force <- renderTable({
    
    req(get_current_dataset(), input$bivar_var1, input$bivar_var2)
    
    data <- get_current_dataset()
    
    
    
    force.df <- as.data.frame(matrix(NA, nrow = 3, ncol = 1))
    
    rownames(force.df) = c("X2", "Phi2", "Cramer")
    
    
    
    tab = with(data, table(get(input$bivar_var2), get(input$bivar_var1)))
    
    # La table de contigence s'il y a indépendence
    tab.indep = tab
    n = sum(tab)
    tab.rowSum = apply(tab, 2, sum)
    tab.colSum = apply(tab, 1, sum)
    
    for(i in c(1:length(tab.colSum))){
      for(j in c(1:length(tab.rowSum))){
        tab.indep[i,j] = tab.colSum[i]*tab.rowSum[j]/n
      }
    }
    
    # Calcul du X²
    force.df[1,1] = sum((tab-tab.indep)^2/tab.indep)
    # Calcul du Phi²
    force.df[2,1] = force.df[1,1]/n
    # Calcul du Cramer
    force.df[3,1] = sqrt(force.df[2,1]/(min(nrow(tab), ncol(tab))-1))
    
    force.df
    
  }, rownames = TRUE, colnames = FALSE)
  
  
  
  
  ###########Numerical PLots
  
  output$scatterplot <- renderPlot({
    
    req(get_current_dataset(), input$bivar_var1, input$bivar_var2)
    
    data <- get_current_dataset()
    
    ggplot(data, aes_string(x = input$bivar_var1, y = input$bivar_var2)) +
      
      geom_point() +
      
      labs(title = paste("Scatterplot of", input$bivar_var1, "vs.", input$bivar_var2),
           
           x = input$bivar_var1, y = input$bivar_var2)
    
  })
  
  output$boxplot_comparison <- renderPlot({
    
    req(get_current_dataset(), input$bivar_var1, input$bivar_var2)
    
    data <- get_current_dataset()
    
    ggplot(data, aes_string(x = input$bivar_var2, y = input$bivar_var1, fill=input$bivar_var2)) +
      
      geom_boxplot()+
      
      labs(title = paste("Boxplot comparison of", input$bivar_var1, "by", input$bivar_var2),
           
           x = input$bivar_var1, y = input$bivar_var2)
    
  })
  
  #####Categorical vs Numericals
  
  output$boxplotmultimodal <- renderPlot({
    
    req(get_current_dataset(), input$bivar_var1, input$bivar_var2)
    data <- get_current_dataset()
    
    ggplot(data, aes(x=data[[input$bivar_var1]], y=data[[input$bivar_var2]], fill=data[[input$bivar_var1]])) + 
      geom_boxplot(alpha=0.3) + xlab(input$bivar_var1) + ylab(input$bivar_var2) +
      theme(legend.position="none")
  })
  
  
  
  output$correlation_multimodal <- renderText({
    req(get_current_dataset(), input$bivar_var1, input$bivar_var2)
    data <- get_current_dataset()
    quali_var = data[[input$bivar_var1]]
    quanti_var = data[[input$bivar_var2]]
    
    # Initialisation de la somme
    somme <- 0
    # Calcul du rapport de corrélation pour var1 et var2
    for (facteur in unique(quali_var)) {
      # Trouver les indices des éléments correspondant à ce facteur
      ind <- which(quali_var == facteur)

      # Extraire les valeurs de y (var2) correspondant à ces indices
      classe <- quanti_var[ind]
      
      # Calcul de la somme de la variance entre la classe et la moyenne de y
      somme <- somme + length(classe) * (mean(classe) - mean(quanti_var))^2
    }
    # Calcul de la somme des carrés totaux pour y (var2)
    y_ecart <- sum((quanti_var - mean(quanti_var))^2)
    
    # Calcul du rapport de corrélation
    rc_var1 <- somme / y_ecart
    
    # Affichage du rapport de corrélation pour var1
    print(c("Rapport de corrélation :",round(rc_var1,4)))
    
  })
  #======================= Data show section =============================================
  
  # Data loading and preview (first 5 rows initially)
  output$dataPreview <- renderDT({
    #req(dataset())
    req(get_current_dataset())
    #head(dataset(), 5)  # Initially show first 5 rows
    head(get_current_dataset(), 5)
    
  })
  
  # Show all dataset when "Show All" button is clicked
  observeEvent(input$showAll, {
    req(dataset())
    output$dataPreview <- renderDT({
      #dataset()  # Show all rows when button is clicked
      get_current_dataset()
    })
  })
  
  

 # ===========================code of the data Summary ================================================
  
  summary_data <- reactive({
    req(dataset())
    #data <- dataset()
    data<-get_current_dataset()
    summary_df <- data.frame(
      Column = names(data),
      Type = sapply(data, class),
      Min = sapply(data, function(col) if (is.numeric(col)) min(col, na.rm = TRUE) else NA),
      Max = sapply(data, function(col) if (is.numeric(col)) max(col, na.rm = TRUE) else NA),
      Median = sapply(data, function(col) if (is.numeric(col)) median(col, na.rm = TRUE) else NA),
      StdDev = sapply(data, function(col) if (is.numeric(col)) sd(col, na.rm = TRUE) else NA),
      Variance = sapply(data, function(col) if (is.numeric(col)) var(col, na.rm = TRUE) else NA),
      UniqueValues = sapply(data, function(col) length(unique(col))),
      MissingValues = sapply(data, function(col) sum(is.na(col) | col=="")),
      # sapply(data, function(x) sum(is.na(x) | x == ""))
      stringsAsFactors = FALSE
    )
    
    return(summary_df)
  })
  output$summaryTable <- renderDT({
    req(summary_data())
    datatable(
      summary_data(),
      options = list(
        scrollX = TRUE,
        pageLength = 10
      )
    )
  })
  
  #===================================Data preprocessing =========================================================================
  
  # Populate columns for normalization, outlier treatment, and encoding
  observe({
    req(dataset())
    data <- dataset()
    
    # Populate normalization columns (only numeric columns)
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    updateSelectInput(session, "normalization_columns", 
                      choices = numeric_cols)
    
    # Populate outlier treatment columns (only numeric columns)
    updateSelectInput(session, "outlier_columns", 
                      choices = numeric_cols)
    
    # Populate encoding columns (categorical and character columns)
    categorical_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
    updateSelectInput(session, "encoding_columns", 
                      choices = categorical_cols)
  })
  
  
  # Create a reactive values object to store different preprocessing stages
  preprocessed_data <- reactiveValues(
    original = NULL,     # Original dataset
    imputed = NULL,      # After imputation
    normalized = NULL,   # After normalization
    outlier_treated = NULL,  # After outlier treatment
    encoded = NULL       # Final preprocessed dataset after encoding
  )
  
  # Function to get the current working dataset
  get_current_dataset <- reactive({
    # Prioritize the most recently preprocessed dataset
    if (!is.null(preprocessed_data$encoded)) return(preprocessed_data$encoded)
    if (!is.null(preprocessed_data$outlier_treated)) return(preprocessed_data$outlier_treated)
    if (!is.null(preprocessed_data$normalized)) return(preprocessed_data$normalized)
    if (!is.null(preprocessed_data$imputed)) return(preprocessed_data$imputed)
    return(dataset())
  })
  
  # Inside the server function, add this reactive function
  #=========================================================================================imputation ========================
  
  
  missing_summary <- reactive({
    req(get_current_dataset())
    #data <- dataset()
    data<- get_current_dataset()
    # Calculate missing values and determine variable types
    missing_data <- data.frame(
      Variable = names(data),
      MissingValues=sapply(data, function(x) sum(is.na(x) | x == "")),
      Type = sapply(data, class),
      stringsAsFactors = FALSE
    )
    
    return(missing_data)
  })
  
  imputed_dataset <- reactiveVal(NULL)
  
  
  # UI for imputation
  output$imputationUI <- renderUI({
    req(missing_summary())
    summary <- missing_summary()
    vars_with_missing <- summary %>% filter(MissingValues > 0)
    if (nrow(vars_with_missing) == 0) {
      return(h4("No missing values to impute!"))
    }
    
    impute_controls <- lapply(seq_len(nrow(vars_with_missing)), function(i) {
      var_name <- vars_with_missing$Variable[i]
      var_type <- vars_with_missing$Type[i]
      
      selectInput(
        inputId = paste0("impute_", var_name),
        label = paste("Imputation method for", var_name),
        choices = if (var_type %in% c("factor", "character")) {
          c("Custom Value", "Replace with 'Unknown'")
        } else {
          c("Mean", "Median", "Custom Value")
        }
      )
    })
    
    
    custom_value_input <- lapply(seq_len(nrow(vars_with_missing)), function(i) {
      var_name <- vars_with_missing$Variable[i]
      textInput(
        inputId = paste0("custom_", var_name),
        label = paste("Custom Value for", var_name),
        value = "",
        placeholder = "Enter a custom value"
      )
    })
    
    tagList(impute_controls, custom_value_input)
  })
  
  observeEvent(input$impute, {
    req(dataset())
    data <- get_current_dataset()
    summary <- missing_summary()
    vars_with_missing <- summary %>% filter(MissingValues > 0)
    
    for (i in seq_len(nrow(vars_with_missing))) {
      var_name <- vars_with_missing$Variable[i]
      var_type <- vars_with_missing$Type[i]
      impute_method <- input[[paste0("impute_", var_name)]]
      
      if (!is.null(impute_method)) {
        if (impute_method == "Mode") {
          mode_val <- names(which.max(table(data[[var_name]])))
          data[[var_name]][is.na(data[[var_name]]) | data[[var_name]] == ""] <- mode_val
        } else if (impute_method == "Replace with 'Unknown'") {
          data[[var_name]][is.na(data[[var_name]]) | data[[var_name]] == ""] <- "Unknown"
        } else if (impute_method == "Mean") {
          data[[var_name]][is.na(data[[var_name]]) | data[[var_name]] == ""] <- mean(data[[var_name]], na.rm = TRUE)
        } else if (impute_method == "Median") {
          data[[var_name]][is.na(data[[var_name]]) | data[[var_name]] == ""] <- median(data[[var_name]], na.rm = TRUE)
        } else if (impute_method == "Custom Value") {
          custom_val <- input[[paste0("custom_", var_name)]]
          data[[var_name]][is.na(data[[var_name]]) | data[[var_name]] == ""] <- custom_val
        }
      }
    }
    
    preprocessed_data$imputed <- data
  })
  
  #======================================================================================================
  
  # Normalization Logic
  observeEvent(input$apply_normalization, {
    # Use the current working dataset or original if no previous preprocessing
    data <- get_current_dataset()
    cols_to_normalize <- input$normalization_columns
    
    normalized_data <- data
    
    if (input$normalization_method == "minmax") {
      normalized_data[cols_to_normalize] <- lapply(data[cols_to_normalize], function(x) {
        (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
      })
    } else if (input$normalization_method == "zscore") {
      normalized_data[cols_to_normalize] <- lapply(data[cols_to_normalize], function(x) {
        (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
      })
    } else if (input$normalization_method == "robust") {
      normalized_data[cols_to_normalize] <- lapply(data[cols_to_normalize], function(x) {
        (x - median(x, na.rm = TRUE)) / IQR(x, na.rm = TRUE)
      })
    }
    
    # Store the normalized data
    preprocessed_data$normalized <- normalized_data
  })
  
  #=============================================================================================
  
  # Outlier Treatment Logic
  observeEvent(input$apply_outlier_treatment, {
    # Use the current working dataset or original if no previous preprocessing
    data <- get_current_dataset()
    cols_to_treat <- input$outlier_columns
    threshold <- input$outlier_threshold
    
    for (col in cols_to_treat) {
      x <- data[[col]]
      z_scores <- scale(x)
      
      if (input$outlier_method == "remove") {
        data <- data[abs(z_scores) < threshold, ]
      } else if (input$outlier_method == "winsor") {
        # Winsorization
        lower_bound <- quantile(x, 0.25) - (threshold * IQR(x))
        upper_bound <- quantile(x, 0.75) + (threshold * IQR(x))
        data[[col]][data[[col]] < lower_bound] <- lower_bound
        data[[col]][data[[col]] > upper_bound] <- upper_bound
      } else if (input$outlier_method == "capping") {
        # Capping
        data[[col]][x > (mean(x) + threshold * sd(x))] <- mean(x) + threshold * sd(x)
        data[[col]][x < (mean(x) - threshold * sd(x))] <- mean(x) - threshold * sd(x)
      }
    }
    
    # Store the outlier-treated data
    preprocessed_data$outlier_treated <- data
  })
  
  #============================================================================================
  # Categorical Encoding Logic
  observeEvent(input$apply_encoding, {
    # Use the current working dataset or original if no previous preprocessing
    data <- get_current_dataset()
    cols_to_encode <- input$encoding_columns
    
    if (input$encoding_method == "onehot") {
      # One-Hot Encoding
      for (col in cols_to_encode) {
        dummy_cols <- model.matrix(~. - 1, data.frame(data[[col]]))
        #dummy_cols <- as.data.frame(model.matrix(~. - 1, data.frame(data[[col]])))
        colnames(dummy_cols) <- paste0(col, "_", colnames(dummy_cols))
        data[[col]] <- NULL
        data <- cbind(data, dummy_cols)
      }
    } else if (input$encoding_method == "label") {
      # Label Encoding
      for (col in cols_to_encode) {
        data[[col]] <- as.numeric(factor(data[[col]]))
      }
    }
    
    # Store the final encoded data
    preprocessed_data$encoded <- data
  })
  
  #===================================================================================================
  # Preprocessed Data Preview
  output$preprocessed_data_preview <- renderDT({
    # Show the most recently preprocessed data
    req(get_current_dataset())
    datatable(get_current_dataset(), 
              options = list(scrollX = TRUE, pageLength = 10)
    )
  })
  
  # Download Preprocessed Data
  output$download_preprocessed_data <- downloadHandler(
    filename = function() {
      paste("preprocessed_dataset_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(get_current_dataset())
      write.csv(get_current_dataset(), file, row.names = FALSE)
    }
  )
  
  #===============================================================Model Params selection ===========================
  # Dynamic Model Parameters UI
  output$model_params <- renderUI({
    tryCatch({
      req(input$model_type)
    }, error = function(e) {
      cat("Error: ", e$message, "\n")
    })
    
    
    tryCatch({
      switch(input$model_type,
             
             "rf" = {
               tagList(
                 numericInput("rf_ntree", "Number of Trees", value = 500, min = 10),
               )
             },
             "svm" = {
               tagList(
                 selectInput("svm_kernel", "Kernel Type", 
                             choices = c("linear", "polynomial", "radial")),
                 numericInput("svm_cost", "Cost", value = 1, min = 0.01)
               )
             },
             "glm" = {
               # No additional parameters for logistic regression
               tags$p("No additional parameters required")
             },
             "lm" = {
               tags$p("No additional parameters required") # Add this for Linear Regression
             }
      )
    }, error = function(e) {
      cat("Error in the selecting model : ", e$message, "\n")
    })
    
    
  })
  
  # ==========================================Model training======================================================
  
  # Model Training
  output$model_results <- renderPrint({
    tryCatch({
      req(get_current_dataset(), input$target_var, input$train_vars)
    }, error = function(e) {
      cat("Error: ", e$message, "\n")
    })
    
    tryCatch({
      df <- get_current_dataset()
      target <- df[[input$target_var]]
      predictors <- df[, input$train_vars, drop = FALSE]
    }, error = function(e) {
      cat("Error: ", e$message, "\n")
    })
    
    # Prepare data
    
    tryCatch({
      set.seed(123)
      train_index <- createDataPartition(target, p = input$train_ratio, list = FALSE)
      x_train <- predictors[train_index, ]
      x_test <- predictors[-train_index, ]
      y_train <- target[train_index]
      y_test <- target[-train_index]
    }, error = function(e) {
      cat("Error: ", e$message, "\n")
    })
    
    
    
    tryCatch({
      
      model <- switch(input$model_type,
                      
                      "rf" = {
                        randomForest(
                          x = x_train,
                          y = y_train,
                          ntree = input$rf_ntree,
                        )
                      },
                      "svm" = {
                        data_svm <- get_current_dataset()
                        categorical_columns <- sapply(data_svm, function(col) is.character(col) || is.logical(col))
                        data_svm[categorical_columns] <- lapply(data_svm[categorical_columns], as.factor)
                        
                        selected_columns <- input$train_vars
                        X <- data_svm[, selected_columns]  # Use only the selected columns for features
                        Y <- data_svm[, input$target_var]  # Target column
                        
                        set.seed(123)  # For reproducibility
                        train_indices <- createDataPartition(Y, p = input$train_ratio, list = FALSE)
                        
                        x_train <- X[train_indices, ]
                        y_train <- Y[train_indices]
                        x_test <- X[-train_indices, ]
                        y_test <- Y[-train_indices]
                        target_col<-input$target_var
                        
                        
                        train_data <- cbind(x_train, target_col = y_train)
                        svm(target_col ~ ., data = train_data,
                                         kernel = "linear", 
                                         cost = 1
                                         )
                      },
                      
                      
                      
                      "glm" = {
                        tryCatch({
                          dtt<-data.frame(x_train, target = y_train)
                          colnames(dtt)[ncol(dtt)] <- input$target_var 
                          if(ncol(dtt)==2) colnames(dtt)[1] <- input$train_vars 
                          glm(
                            
                            formula <- as.formula(paste(input$target_var,"~", paste(input$train_vars, collapse = " + "))),
                            data<-dtt,
                            family = binomial
                          )
                        },error = function(e) {
                          cat("Error in the gml function : ", e$message, "\n")
                        })
                        
                      },
                      "lm" = {
                        dtt <- data.frame(x_train, target = y_train)
                        colnames(dtt)[ncol(dtt)] <- input$target_var
                        if (ncol(dtt) == 2) colnames(dtt)[1] <- input$train_vars
                        lm(
                          formula = as.formula(paste(input$target_var, "~", paste(input$train_vars, collapse = " + "))),
                          data = dtt
                        )
                      }
                      
                      
      )
    }, error = function(e) {
      print("====================================")
      showModal(
        modalDialog(
          title = "Alert",
          "Please select the model first",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
      print("====================================")
      cat("please select a model first ", , "\n")
    })
    # Train model
    
    tryCatch({
      # Check if model training was successful
      if (is.null(model)) {
        
        stop("Model training failed. Please check your data and model parameters.")
      }
      
      
      # Save trained model
      trained_model(model)
      predictions <- predict(model, x_test)
      
      if (input$model_type %in% c("rf", "svm", "glm")) {
        roc_obj <- roc(y_test, predictions)
        
        #====================================Roc curve==========================================
        output$roc_plot <- renderPlot({
          req(model)
          
          # roc_obj <- model_results()$roc
          roc_data <- data.frame(
            FPR = 1 - roc_obj$specificities,
            TPR = roc_obj$sensitivities
          )
          
          ggplot(roc_data, aes(x = FPR, y = TPR)) +
            geom_line(color = "#2C3E50", size = 1.2) +
            geom_abline(intercept = 0, slope = 1, 
                        linetype = "dashed", color = "gray50") +
            annotate("text", x = 0.7, y = 0.3, 
                     label = paste("AUC =", round(auc(roc_obj), 3)), 
                     size = 5, color = "#2C3E50") +
            labs(
              title = "ROC Curve",
              x = "False Positive Rate (1 - Specificity)",
              y = "True Positive Rate (Sensitivity)"
            ) +
            theme_minimal() +
            theme(
              plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              axis.title = element_text(size = 12),
              axis.text = element_text(size = 10),
              panel.grid.minor = element_blank()
            ) +
            coord_equal()
        })
        #=========================================================================
        
        
        predicted_classes <- ifelse(predictions > 0.5, 1, 0)
        confusion_matrix <- table(Predicted = predicted_classes, Actual = y_test)
        cat("Confusion Matrix: \n")
        print(confusion_matrix)
        accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
        cat("Accuracy:", accuracy, "\n")
      } else if (input$model_type == "lm") {
        # For Linear Regression, use RMSE or R-squared for evaluation
        rmse <- sqrt(mean((y_test - predictions)^2))
        r_squared <- 1 - (sum((y_test - predictions)^2) / sum((y_test - mean(y_test))^2))
        cat("RMSE:", rmse, "\n")
        cat("R-squared:", r_squared, "\n")
      }
  
      
    }, error = function(e) {
      cat("Error in the saving model : ", e$message, "\n")
    })
    
  })
  # =============================================Model visualization=================================================================
  # adding viusalizations 
  
  output$model_plot <- renderPlot({
    req(trained_model(), get_current_dataset(), input$target_var, input$train_vars)
    
    # For 2D visualization, select first two predictor variables
    if(length(input$train_vars) < 2) {
      return(NULL)
    }
    
    df <- get_current_dataset()
    x_vars <- input$train_vars[1:2]
    target_var <- input$target_var
    
    # Prepare data
    x <- df[, x_vars]
    y <- df[[target_var]]
    
    
    
    
    
    # For Linear Regression
    if (input$model_type == "lm") {
      # Extract target and predictor variables
      target_var <- input$target_var
      predictor <- input$train_vars[1]
      model <- trained_model()
      # Subset the dataset to include only the predictor and target variable
      plot_data <- df[, c(predictor, target_var)]
      colnames(plot_data) <- c("x", "y")
      
      # Create scatter plot of data
      plot(
        plot_data$x, plot_data$y,
        xlab = predictor, ylab = target_var,
        main = "Linear Regression: Scatter Plot with Regression Line",
        pch = 19, col = "blue"
      )
      
      # Add the regression line
      abline(model, col = "red", lwd = 2)
      
      legend("topright", legend = c("Data Points", "Regression Line"), 
             col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1))
    }
    
    
    
    # Create visualization based on model type
    if(input$model_type == "glm") {
      # Logistic Regression Decision Boundary
      plot(x[,1], x[,2], col = ifelse(y == 1, "blue", "red"), 
           pch = 19, xlab = x_vars[1], ylab = x_vars[2], 
           main = "Logistic Regression Decision Boundary")
      
      # Plot decision boundary
      model <- trained_model()
      
      # Create a grid of points
      x1_range <- seq(min(x[,1]), max(x[,1]), length.out = 100)
      x2_range <- seq(min(x[,2]), max(x[,2]), length.out = 100)
      grid <- expand.grid(x1_range, x2_range)
      colnames(grid) <- x_vars
      
      # Predict probabilities for the grid
      grid_pred <- predict(model, newdata = grid, type = "response")
      
      # Contour plot for decision boundary
      contour(x1_range, x2_range, matrix(grid_pred, 100, 100), 
              levels = 0.5, add = TRUE, col = "green", lwd = 2)
      
      legend("topright", legend = c("Class 0", "Class 1", "Decision Boundary"), 
             col = c("red", "blue", "green"), pch = c(19, 19, NA), 
             lty = c(NA, NA, 1))
    }
    else if(input$model_type == "svm") {
      # SVM Decision Boundary
      
      # Fit SVM with current parameters
      svm_model <- trained_model()
      
      # Create a grid of points
      x1_range <- seq(min(x[,1]), max(x[,1]), length.out = 100)
      x2_range <- seq(min(x[,2]), max(x[,2]), length.out = 100)
      grid <- expand.grid(x1_range, x2_range)
      colnames(grid) <- x_vars
      
      # Predict classes for the grid
      grid_pred <- predict(svm_model, newdata = grid)
      
      # Plot the data points
      plot(x[,1], x[,2], col = ifelse(y == 1, "blue", "red"), 
           pch = 19, xlab = x_vars[1], ylab = x_vars[2], 
           main = "SVM Decision Boundary")
      
      # Create a contour plot of the decision boundary
      contour(x1_range, x2_range, matrix(as.numeric(grid_pred), 100, 100), 
              levels = 0.5, add = TRUE, col = "green", lwd = 2)
      
      legend("topright", legend = c("Class 0", "Class 1", "Decision Boundary"), 
             col = c("red", "blue", "green"), pch = c(19, 19, NA), 
             lty = c(NA, NA, 1))
    }
  })
  
  # ======================================= Correlation visualisation =========================================================
  # Correlation Visualization
  output$correlation_heatmap <- renderPlot({
    req(get_current_dataset())
    
    # Prepare data for correlation
    dt <- get_current_dataset()
    
    # Select only numeric columns
    numeric_cols <- sapply(dt, is.numeric)
    if(sum(numeric_cols) < 2) {
      plot(0, type = 'n', axes = FALSE, xlab = "", ylab = "")
      text(0, 0, "Not enough numeric variables for correlation", col = "red")
      return()
    }
    
    # Compute correlation matrix
    cor_matrix <- cor(dt[, numeric_cols], method = input$corr_method)
    
    # Correlation plot using corrplot
    corrplot(cor_matrix, 
             method = input$corr_plot_type, 
             type = "full", 
             tl.col = "black", 
             tl.srt = 45, 
             diag = TRUE
             #title = paste("Correlation Heatmap (", 
             #             input$corr_method, " method)")
    )
  })
  
  # Scatter Plot Matrix
  output$scatter_matrix <- renderPlot({
    req(get_current_dataset())
    
    # Prepare data for scatter matrix
    dt <- get_current_dataset()
    
    # Select only numeric columns
    numeric_cols <- sapply(dt, is.numeric)
    if(sum(numeric_cols) < 2) {
      plot(0, type = 'n', axes = FALSE, xlab = "", ylab = "")
      text(0, 0, "Not enough numeric variables for scatter matrix", col = "red")
      return()
    }
    
    # Scatter plot matrix using GGally
    ggpairs(dt[, numeric_cols], 
            title = "Scatter Plot Matrix of Numeric Variables",
            lower = list(continuous = "points"),
            diag = list(continuous = "blankDiag"),
            upper = list(continuous = "cor")) +
      theme_bw()
  })
  #===========================================================================================================================
  
  
  }

# Run the application
shinyApp(ui, server)