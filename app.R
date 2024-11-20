# Load necessary libraries
library(shiny)
library(shinydashboard)
library(dplyr)

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
                box(title = "Imputation Options", status = "warning", solidHeader = TRUE, width = 12,
                    uiOutput("imputationUI"),
                    actionButton("impute", "Apply Imputation", class = "btn-primary")
                )
              ),
              fluidRow(
                box(title = "Download Imputed Dataset", status = "success", solidHeader = TRUE, width = 12,
                    downloadButton("downloadData", "Download Imputed Dataset")
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
    read.csv(input$file$datapath)
  })
  
  missing_summary <- reactive({
    req(dataset())
    data <- dataset()
    missing_counts <- sapply(data, function(col) sum(is.na(col)))
    data.frame(
      Variable = names(missing_counts),
      MissingValues = missing_counts,
      Type = sapply(data, class),
      stringsAsFactors = FALSE
    )
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
    num_categorical <- sum(sapply(dataset(), class) %in% c("factor", "character"))
    valueBox(
      num_categorical, "Categorical Variables",
      icon = icon("tags"), color = "purple"
    )
  })
  
  output$numericalBox <- renderValueBox({
    req(dataset())
    num_numerical <- sum(sapply(dataset(), class) %in% c("integer", "numeric"))
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
  
  # UI for selecting imputation methods
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
          c("Mode", "Replace with 'Unknown'")
        } else {
          c("Mean", "Median", "Custom Value")
        }
      )
    })
    do.call(tagList, impute_controls)
  })
  
  # Apply imputation
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
        }
      }
    }
    
    imputed_dataset(data)
  })
  
  # Download imputed dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("imputed_dataset", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(imputed_dataset(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui, server)
