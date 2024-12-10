# Load necessary libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2)

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
      menuItem("Impute", tabName = "impute", icon = icon("calculator")),
      menuItem("Visualisation", tabName = "visualisation", icon = icon("chart-bar")), # New Visualisation menu
      menuItem("Preprocessing", tabName = "preprocessing", icon = icon("filter"))
      
      
      
      
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
                valueBoxOutput("missingBox", width = 3)
              ),
              fluidRow(
                valueBoxOutput("searchBox", width = 12)
              ),
              fluidRow(
                valueBoxOutput("imputationBox", width = 12)
              ),
         ),
      
      tabItem(tabName = "impute",
              #h3("Impute Section"),
              
              fluidRow(
                box(title = "Imputation Options", status = "warning", solidHeader = TRUE, width = 12,
                    uiOutput("imputationUI"),
                    actionButton("impute", "Apply Imputation", class = "btn-primary")
                )
              ),
              fluidRow(
                box(title = "Download Imputed Dataset", status = "success", solidHeader = TRUE, width = 12,
                    downloadButton("downloadData", "Download Imputed Dataset")
                )
              ),
      ),
      
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
                                            "Histogram" = "hist")),
                    actionButton("generate_plot", "Generate Plot", class = "btn-success")
                )
              ),
              fluidRow(
                box(title = "Plot Output", status = "info", solidHeader = TRUE, width = 12,
                    plotOutput("plot")
                )
              )
      ),
      #=========================================page for  data processing ==========================================
      tabItem(tabName = "preprocessing",
              h3("Data Preprocessing"),
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
      )
    
    
      
      
      
      
      
      
      
      
      
      
      
    )
  )
)

# Server
server <- function(input, output, session) {

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
    num_missing <- sum(is.na(dataset()))
    valueBox(
      num_missing, "Missing Values",
      icon = icon("exclamation-circle"), color = "red"
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
    data <- dataset()
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
    }
  })
  
  
  #======================= Data show section =============================================
  
  # Data loading and preview (first 5 rows initially)
  output$dataPreview <- renderDT({
    req(dataset())
    head(dataset(), 5)  # Initially show first 5 rows
  })
  
  # Show all dataset when "Show All" button is clicked
  observeEvent(input$showAll, {
    req(dataset())
    output$dataPreview <- renderDT({
      dataset()  # Show all rows when button is clicked
    })
  })
  
  
  #=========================Data imputation section =====================================
  
  
  missing_summary <- reactive({
    req(dataset())
    data <- dataset()
    
    # Calculate missing values and determine variable types
    missing_data <- data.frame(
      Variable = names(data),
      MissingValues = sapply(data, function(col) {
        if (is.character(col) | is.factor(col)) {
          sum(is.na(col) | nchar(as.character(col)) == 0)
        } else {
          sum(is.na(col))
        }
      }),
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
    data <- dataset()
    summary <- missing_summary()
    vars_with_missing <- summary %>% filter(MissingValues > 0)
    
    for (i in seq_len(nrow(vars_with_missing))) {
      var_name <- vars_with_missing$Variable[i]
      var_type <- vars_with_missing$Type[i]
      impute_method <- input[[paste0("impute_", var_name)]]
      
      if (!is.null(impute_method)) {
        if (impute_method == "Mode") {
          mode_val <- names(which.max(table(data[[var_name]])))
          data[[var_name]][is.na(data[[var_name]])] <- mode_val
        } else if (impute_method == "Replace with 'Unknown'") {
          data[[var_name]][is.na(data[[var_name]])] <- "Unknown"
        } else if (impute_method == "Mean") {
          data[[var_name]][is.na(data[[var_name]])] <- mean(data[[var_name]], na.rm = TRUE)
        } else if (impute_method == "Median") {
          data[[var_name]][is.na(data[[var_name]])] <- median(data[[var_name]], na.rm = TRUE)
        } else if (impute_method == "Custom Value") {
          custom_val <- input[[paste0("custom_", var_name)]]
          data[[var_name]][is.na(data[[var_name]])] <- custom_val
        }
      }
    }
    
    imputed_dataset(data)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("imputed_dataset", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(imputed_dataset())
      write.csv(imputed_dataset(), file, row.names = FALSE)
    }
  )
  
  
  
 # ===========================code of the data Summary ================================================
  
  summary_data <- reactive({
    req(dataset())
    data <- dataset()
    
    summary_df <- data.frame(
      Column = names(data),
      Type = sapply(data, class),
      Min = sapply(data, function(col) if (is.numeric(col)) min(col, na.rm = TRUE) else NA),
      Max = sapply(data, function(col) if (is.numeric(col)) max(col, na.rm = TRUE) else NA),
      Median = sapply(data, function(col) if (is.numeric(col)) median(col, na.rm = TRUE) else NA),
      StdDev = sapply(data, function(col) if (is.numeric(col)) sd(col, na.rm = TRUE) else NA),
      Variance = sapply(data, function(col) if (is.numeric(col)) var(col, na.rm = TRUE) else NA),
      UniqueValues = sapply(data, function(col) length(unique(col))),
      MissingValues = sapply(data, function(col) sum(is.na(col))),
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
  observe({
    req(dataset())
    data <- dataset()
    
    # Update normalization column choices
    updateSelectInput(session, "normalization_columns", 
                      choices = names(data)[sapply(data, is.numeric)]
    )
    
    # Update outlier treatment column choices
    updateSelectInput(session, "outlier_columns", 
                      choices = names(data)[sapply(data, is.numeric)]
    )
    
    # Update encoding column choices
    updateSelectInput(session, "encoding_columns", 
                      choices = names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
    )
  })
  
  # Preprocessed data storage
  preprocessed_data <- reactiveVal(NULL)
  
  # Normalization Logic
  observeEvent(input$apply_normalization, {
    req(dataset(), input$normalization_columns)
    data <- dataset()
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
    
    preprocessed_data(normalized_data)
  })
  
  # Outlier Treatment Logic
  observeEvent(input$apply_outlier_treatment, {
    req(dataset(), input$outlier_columns)
    data <- dataset()
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
    
    preprocessed_data(data)
  })
  
  # Categorical Encoding Logic
  observeEvent(input$apply_encoding, {
    req(dataset(), input$encoding_columns)
    data <- dataset()
    cols_to_encode <- input$encoding_columns
    
    if (input$encoding_method == "onehot") {
      # One-Hot Encoding
      for (col in cols_to_encode) {
        dummy_cols <- model.matrix(~. - 1, data.frame(data[[col]]))
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
    
    preprocessed_data(data)
  })
  
  # Preprocessed Data Preview
  output$preprocessed_data_preview <- renderDT({
    req(preprocessed_data())
    datatable(preprocessed_data(), 
              options = list(scrollX = TRUE, pageLength = 10)
    )
  })
  
  # Download Preprocessed Data
  output$download_preprocessed_data <- downloadHandler(
    filename = function() {
      paste("preprocessed_dataset_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(preprocessed_data())
      write.csv(preprocessed_data(), file, row.names = FALSE)
    }
  )
  
  
  
  
  

  
  }

# Run the application
shinyApp(ui, server)