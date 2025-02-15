#' Input data
#'
#' @description
#' Output data to *.RDS objects.

#' @param path Folder path of where the data will be output.
#' 
#' @param vars_regex Regex pattern to match variable names to input.
#' 
#' @param vars_vec A vector of variable names to input.
#' 
#' @param verbose Logical argument to be verbose.

#' @import data.table, shiny, bslib, readxl, DT, writexl, diffobj, leaflet, echarts4r, dplyr

#' @export
geocodeR <- function(){

    ui <- fillPage(
        tags$head(
            # Adding custom CSS for .col-sm-8 class
            tags$style(HTML("
                @media (min-width: 576px) {
                    .col-sm-8 {
                        flex: 0 0 auto;
                        -webkit-flex: 0 0 auto;
                        width: 100%;  # Update width to 100%
                    }
                }
            "))
        ),
        page_sidebar(
            title = "🌍 geocodeR",
            sidebar = sidebar(
                accordion(
                    id = "sidebar_sections",
                    accordion_panel("\ud83d\udce9 Upload & Geocode",
                        fileInput("file", "Upload CSV/XLSX File", accept = c(".csv", ".xlsx"),
                                  buttonLabel = "Browse...", placeholder = "No file selected"),
                        uiOutput("var_select"),
                        actionButton("merge_btn", "Geocode", class = "btn btn-primary")
                    ),
                    accordion_panel("\ud83e\uddf0 Filters",
                        uiOutput("filter1"),
                        selectInput("operator1", "Operator for First Filter", choices = c("<", "<=", ">", ">=")),
                        numericInput("filter_value1", "Value for First Filter", value = NA, min = NA, max = NA),
                        uiOutput("filter2"),
                        selectInput("operator2", "Operator for Second Filter", choices = c("<", "<=", ">", ">=")),
                        numericInput("filter_value2", "Value for Second Filter", value = NA, min = NA, max = NA)
                    ),
                    accordion_panel("\u2b06\ufe0f Download",
                        downloadButton("download", "Download Filtered Data", class = "btn btn-success")
                    )
                )
            ),
            mainPanel(
                div(style = "width: 100%;",
                    accordion(
                        id = "main_sections",
                        accordion_panel("Imported Data Preview", width = "100%",
                            div(
                                DTOutput("uploaded_table"))
                        ),
                        accordion_panel("Geocoded Output", width = "100%",
                            div(
                                DTOutput("filtered_table"))
                        ),
                        accordion_panel("Selected Locations Map", width = "100%",
                            div(
                                leafletOutput("map", height = "500px"))
                        ),
                        accordion_panel("Geocode Summary Statistics", width = "100%",
                            collapsed = TRUE,
                            div(
                                echarts4rOutput("matched_summary"),
                                echarts4rOutput("match_type_summary"))
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
        
        output$uploaded_table <- renderDT({
            req(data())
            datatable(data(), options = list(pageLength = 10, autoWidth = TRUE), selection = "none")  
        })

        output$var_select <- renderUI({
            req(data())
            selectInput("merge_var", "Select Variable to Geocode", choices = names(data()),
                        selectize = TRUE, multiple = FALSE)
        })
        
        merged_data <- eventReactive(input$merge_btn, {
            req(data(), input$merge_var)
            merge_col <- input$merge_var
            df <- lookup_address(as.character(toupper(data()[[merge_col]])))
            df
        })
        
        filtered_data <- reactive({
            req(merged_data())
            df <- merged_data()
            df
        })
        
        output$filtered_table <- renderDT({
            req(filtered_data())
            datatable(filtered_data(), options = list(pageLength = 10, autoWidth = TRUE), selection = "multiple")  
        })
        
        output$download <- downloadHandler(
            filename = function() {
                paste("filtered_data", Sys.Date(), ".csv", sep = "")
            },
            content = function(file) {
                write.csv(filtered_data(), file, row.names = FALSE)
            }
        )

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
                    )
                })
            } else {
                output$map <- renderLeaflet({
                    leaflet() %>%
                    addTiles()  
                })
            }
        })
        
        output$matched_summary <- renderEcharts4r({
            req(filtered_data())
            df <- filtered_data()
            per <- round((nrow(df) / (sum(df$matched)) * 100), 2)
            e_charts() |> 
            e_gauge(per, "PERCENT") |> 
            e_title("Gecoded")
        })

        output$match_type_summary <- renderEcharts4r({
            req(filtered_data())
            df <- filtered_data()
            match_type_counts <- df %>% count(match_type) %>% mutate(percentage = n / sum(n) * 100)
            
            match_type_counts %>%
                e_charts(match_type) %>%
                e_bar(n, name = "Count") %>%
                e_tooltip(trigger = "axis", formatter = htmlwidgets::JS("
                    function(params) {
                        var count = params[0].value;
                        var percentage = params[0].data.percentage.toFixed(1);
                        return 'Match Type: ' + params[0].name + '<br>Count: ' + count + '<br>Percentage: ' + percentage + '%';
                    }
                ")) %>%
                e_title("Match Type Summary") %>%
                e_x_axis(name = "Match Type") %>%
                e_y_axis(name = "Count") %>%
                e_legend(show = FALSE)  # Remove legend for cleaner look
        })

    }

    shinyApp(ui, server)
}
