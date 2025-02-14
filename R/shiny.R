ui <- fillPage(
  page_sidebar(
    title = "🌍 geocodeR",
    sidebar = sidebar(
      accordion(
        id = "sidebar_sections",
        accordion_panel("Upload & Merge",
          fileInput("file", "Upload CSV/XLSX File", accept = c(".csv", ".xlsx"),
                    buttonLabel = "Browse...", placeholder = "No file selected"),
          uiOutput("var_select"),
          actionButton("merge_btn", "Merge Data", class = "btn btn-primary")
        ),
        accordion_panel("Filters",
          uiOutput("filter1"),
          selectInput("operator1", "Operator for First Filter", choices = c("<", "<=", ">", ">=")),
          numericInput("filter_value1", "Value for First Filter", value = NA, min = NA, max = NA),
          uiOutput("filter2"),
          selectInput("operator2", "Operator for Second Filter", choices = c("<", "<=", ">", ">=")),
          numericInput("filter_value2", "Value for Second Filter", value = NA, min = NA, max = NA)
        ),
        accordion_panel("Column Differences",
          selectInput("diff_col1", "Select First Column for Diff", choices = NULL),
          selectInput("diff_col2", "Select Second Column for Diff", choices = NULL),
          actionButton("show_diff", "Show Differences", class = "btn btn-warning")
        ),
        accordion_panel("Download",
          downloadButton("download", "Download Filtered Data", class = "btn btn-success")
        )
      )
    ),
    mainPanel(
      div(style = "width: 100%;",
        accordion(
          id = "main_sections",
          accordion_panel("Uploaded Data Preview", width = "100%",
            div(
                DTOutput("uploaded_table"))
          ),
          accordion_panel("Merged Data Table", width = "100%",
            div(
                DTOutput("filtered_table"))
          ),
          accordion_panel("Selected Locations Map", width = "100%",
            div(
                leafletOutput("map", height = "500px"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") {
      fread(input$file$datapath)
    } else if (ext == "xlsx") {
      data.table(read_excel(input$file$datapath))
    } else {
      NULL
    }
  })
  
  # **New Output for Uploaded Data Preview**
  output$uploaded_table <- renderDT({
    req(data())
    datatable(data(), 
              options = list(pageLength = 10, autoWidth = TRUE), 
              selection = "none")  
  })

  output$var_select <- renderUI({
    req(data())
    selectInput("merge_var", "Select Column to Merge", choices = names(data()),
                selectize = TRUE, multiple = FALSE)
  })
  
  merged_data <- eventReactive(input$merge_btn, {
    req(data(), input$merge_var)
    merge_col <- input$merge_var
    df <- lookup_address(as.character(toupper(data()[[merge_col]])))
    
    updateSelectInput(session, "diff_col1", choices = names(df))
    updateSelectInput(session, "diff_col2", choices = names(df))
    df
  })
  
  output$filter1 <- renderUI({
    req(merged_data())
    selectInput("filter_var1", "Select First Filter Column", choices = names(merged_data()))
  })
  
  output$filter2 <- renderUI({
    req(merged_data())
    selectInput("filter_var2", "Select Second Filter Column", choices = names(merged_data()))
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
    datatable(filtered_data(), 
              options = list(pageLength = 10, autoWidth = TRUE), 
              selection = "multiple")  
  })
  
  observeEvent(input$show_diff, {
    output$diff_output <- renderUI({
      req(filtered_data(), input$diff_col1, input$diff_col2)
      df <- filtered_data()
      
      if (input$diff_col1 %in% names(df) && input$diff_col2 %in% names(df)) {
        diffs <- diffChr(as.character(df[[input$diff_col1]]),
                         as.character(df[[input$diff_col2]]))
        
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

  # **Map Rendering**  
  observe({
    req(filtered_data())
    selected_rows <- input$filtered_table_rows_selected  
    df <- filtered_data()

    if (length(selected_rows) > 0) {
      selected_data <- df[selected_rows, ]
      
      output$map <- renderLeaflet({
        leaflet(selected_data) %>%
          addTiles() %>%
          addCircleMarkers(
            ~longitude, ~latitude, 
            radius = 5, color = "blue", fillOpacity = 0.7
            # , popup = ~paste0("ID: ", ID, "<br>Value: ", Value)
          )
      })
    } else {
      output$map <- renderLeaflet({
        leaflet() %>%
          addTiles()  
      })
    }
  })
}

shinyApp(ui, server)
