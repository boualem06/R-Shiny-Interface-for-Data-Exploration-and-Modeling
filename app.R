# Load necessary libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dataset Overview"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Dataset", tabName = "upload", icon = icon("upload"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "upload",
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
              fluidRow(
                box(title = "Dataset Preview", status = "info", solidHeader = TRUE, width = 12,
                    DTOutput("dataPreview"),
                    actionButton("showAll", "Show All", class = "btn-primary")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
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
}

# Run the application
shinyApp(ui, server)
