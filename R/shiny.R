library(shiny)
library(shinythemes)
library(readxl)
library(DT)
library(dplyr)
library(writexl)
library(diffobj)  # Use diffobj for diffing columns

# Preloaded dataset (replace with actual data)
preloaded_data <- data.frame(ID = 1:10, Value = letters[1:10], Num1 = rnorm(10, 50, 10), Num2 = rnorm(10, 100, 20))

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("CSV/XLSX Merger", windowTitle = "Data Merger"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV/XLSX File", accept = c(".csv", ".xlsx"),
                buttonLabel = "Browse...", placeholder = "No file selected"),
      uiOutput("var_select"),
      actionButton("merge_btn", "Merge Data", class = "btn btn-primary"),
      hr(),
      uiOutput("filter1"),
      selectInput("operator1", "Operator for First Filter", choices = c("<", "<=", ">", ">=")),
      numericInput("filter_value1", "Value for First Filter", value = NA, min = NA, max = NA),
      uiOutput("filter2"),
      selectInput("operator2", "Operator for Second Filter", choices = c("<", "<=", ">", ">=")),
      numericInput("filter_value2", "Value for Second Filter", value = NA, min = NA, max = NA),
      hr(),
      selectInput("diff_col1", "Select First Column for Diff", choices = NULL),
      selectInput("diff_col2", "Select Second Column for Diff", choices = NULL),
      actionButton("show_diff", "Show Differences", class = "btn btn-warning"),
      downloadButton("download", "Download Filtered Data", class = "btn btn-success")
    ),
    mainPanel(
      h3("Merged Data Table"),
      div(style = "width: 100%; overflow-x: auto; max-width: 100%;",
          DTOutput("filtered_table")
      ),
      h3("Column Differences"),
      uiOutput("diff_output")
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") {
      read.csv(input$file$datapath)
    } else if (ext == "xlsx") {
      read_excel(input$file$datapath)
    } else {
      NULL
    }
  })
  
  output$var_select <- renderUI({
    req(data())
    selectInput("merge_var", "Select Column to Merge", choices = names(data()),
                selectize = TRUE, multiple = FALSE)
  })
  
  merged_data <- eventReactive(input$merge_btn, {
    req(data(), input$merge_var)
    merge_col <- input$merge_var
    df <- lookup_address(data())
    updateSelectInput(session, "diff_col1", choices = names(df))
    updateSelectInput(session, "diff_col2", choices = names(df))
    df
  })
  
  output$filter1 <- renderUI({
    req(merged_data())
    selectInput("filter_var1", "Select First Filter Column", choices = names(merged_data()),
                selected = NULL)
  })
  
  output$filter2 <- renderUI({
    req(merged_data())
    selectInput("filter_var2", "Select Second Filter Column", choices = names(merged_data()),
                selected = NULL)
  })
  
  filtered_data <- reactive({
    req(merged_data())
    df <- merged_data()
    
    if (!is.null(input$filter_var1) && input$filter_var1 %in% names(df) && !is.na(input$filter_value1)) {
      df <- df %>% filter(eval(parse(text = paste0("`", input$filter_var1, "`", input$operator1, input$filter_value1))))
    }
    if (!is.null(input$filter_var2) && input$filter_var2 %in% names(df) && !is.na(input$filter_value2)) {
      df <- df %>% filter(eval(parse(text = paste0("`", input$filter_var2, "`", input$operator2, input$filter_value2))))
    }
    df
  })
  
  output$filtered_table <- renderDT({
    req(filtered_data())
    datatable(filtered_data(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  observeEvent(input$show_diff, {
    output$diff_output <- renderUI({
      req(filtered_data(), input$diff_col1, input$diff_col2)
      df <- filtered_data()
      
      if (input$diff_col1 %in% names(df) && input$diff_col2 %in% names(df)) {
        # Use diffobj to show differences between the selected columns
        diffs <- diffChr(as.character(df[[input$diff_col1]]),
                         as.character(df[[input$diff_col2]]))
        
        # Render the diff as HTML
        HTML(paste0("<pre style='background-color:#f8f9fa;padding:10px;border-radius:5px;'>", diffs, "</pre>"))
      } else {
        "Please select valid columns to compare."
      }
    })
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("filtered_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
