#' Input data
#'
#' @name geocodeR

#' @description Output data to *.RDS objects.

#' @param path Folder path of where the data will be output.
#' 
#' @param vars_regex Regex pattern to match variable names to input.
#' 
#' @param vars_vec A vector of variable names to input.
#' 
#' @param verbose Logical argument to be verbose.

#' @import data.table, shiny, bslib, readxl, DT, writexl, diffobj, leaflet, echarts4r, dplyr, sf, bslib, shinyjs

#' @export
options(shiny.maxRequestSize=1E10)
library(shiny)
library(shinyjs)
library(data.table)
library(readxl)
library(sf)
library(leaflet)
library(DT)
library(echarts4r)
library(bslib)

geocodeR <- function(){


    # Define UI
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

        table.dataTable {
            font-size: 10px !important;
        }
        "))
    ),
    page_sidebar(
        title = "🌍 geocodeR",
        sidebar = sidebar(
        accordion(
            id = "sidebar_sections",
            accordion_panel("\u2b06\ufe0f Upload & Geocode",
            fileInput("file", "Upload CSV/XLSX File", accept = c(".csv", ".xlsx"),
            buttonLabel = "Browse...", placeholder = "No file selected"),
            uiOutput("var_select"),
            actionButton("merge_btn", "Geocode", class = "btn btn-primary")
            ),
            accordion_panel("\ud83d\udcc4 Upload Shapefile",
            fileInput("shapefile", "Upload Shapefile", 
                accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"),
                buttonLabel = "Browse...", placeholder = "No file selected",
                multiple = TRUE),  # Allow multiple files to be uploaded
            uiOutput("polygon_column_select"),  # Dropdown to select polygon column
            uiOutput("subregion_column_select"),  # Dropdown to select subregion column
            actionButton("allocate_btn", "Allocate to Subregions", class = "btn btn-primary")
            ),
            accordion_panel("\ud83e\uddf0 Filters",
            uiOutput("filter1"),
            selectInput("operator1", "Operator for First Filter", choices = c("<", "<=", ">", ">=", "==", "%plike%")),
            textInput("filter_value1", "Value for First Filter", value = NA),
            uiOutput("filter2"),
            selectInput("operator2", "Operator for Second Filter", choices = c("<", "<=", ">", ">=", "==", "%plike%")),
            textInput("filter_value2", "Value for Second Filter", value = NA),
            ),
            accordion_panel("\ud83d\udce9 Download",
            downloadButton("download", "Download Filtered Data", class = "btn btn-success")
            ),
            accordion_panel("📃Beyond Compare",
            uiOutput("compare_var1_select"),  # Dropdown to select first variable
            uiOutput("compare_var2_select"),  # Dropdown to select second variable
            actionButton("print_btn", "Print to Console", class = "btn btn-info")
            )
        )
        ),
        mainPanel(
        div(style = "width: 100%;",
            accordion(
            id = "main_sections",
            accordion_panel("Imported Data Preview", width = "100%",
                div(DTOutput("uploaded_table"))
            ),
            accordion_panel("Geocoded Output", width = "100%",
                div(DTOutput("filtered_table"))
            ),
            accordion_panel("Selected Locations Map", width = "100%",
                div(leafletOutput("map", height = "500px"))
            ),
            accordion_panel("Subregion Allocation Map", width = "100%",
                div(leafletOutput("subregion_map", height = "500px"))
            ),
            accordion_panel("Geocode Summary Statistics", width = "100%",
                collapsed = TRUE,
                div(style = "display: flex; justify-content: space-between;",
                    div(style = "width: 48%;", echarts4rOutput("matched_summary")),
                    div(style = "width: 48%;", echarts4rOutput("match_type_summary"))
                )
            )
            )
            )
        )
        )
    )

    # Define Server
    server <- function(input, output, session) {
    # Reactive to load uploaded data
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

    # Render uploaded data table with horizontal scroll
    output$uploaded_table <- renderDT({
        req(data())
        datatable(data(), options = list(
        pageLength = 10,
        scrollX = TRUE,  # Enable horizontal scrolling
        autoWidth = FALSE), selection = "none") 
    })

    # Render variable selection dropdown
    output$var_select <- renderUI({
        req(data())
        selectInput("merge_var", "Select Variable to Geocode", choices = names(data()),
        selectize = TRUE, multiple = FALSE, selected = "address")
    })

    # Geocode data
    merged_data <- eventReactive(input$merge_btn, {
        req(data(), input$merge_var)
        merge_col <- input$merge_var
        withProgress(message = "Processing...", value = 0, {
        df <- lookup_address(as.character((data()[[merge_col]])))
        })
        df
    })

    # Reactive value to store filtered data (including subregion column)
    filtered_data <- reactiveVal(NULL)

    # Observe changes in merged_data and update filtered_data
    observe({
        req(merged_data())
        filtered_data(merged_data())
    })

    # Reactive expression for dynamically filtered data
    filtered_data_dynamic <- reactive({
        req(filtered_data())
        df <- filtered_data()
    # browser()
        # Apply first filter
        if (!is.na(input$filter_value1) && !input$filter_value1 == "") {
            operator <- input$operator1
            value <- input$filter_value1
            filter_var <- input$filter1
            if(operator %in% c("%plike%")){
                value <- paste0("'", value, "'")
            }
            filter_expr <- parse(text = paste(filter_var, operator, value))
            test <- try(df[eval(filter_expr)], silent = TRUE)
            if(class(test)[1] == "try-error"){
                df <- df
            } else {
                df <- test
            }
        }

        # Apply second filter
        if (!is.na(input$filter_value2) && !input$filter_value2 == "") {
            operator <- input$operator2
            value <- input$filter_value2
            filter_var <- input$filter2
            if(operator %in% c("%plike%")){
                value <- paste0("'", value, "'")
            }
            filter_expr <- parse(text = paste(filter_var, operator, value))
            test <- try(df[eval(filter_expr)], silent = TRUE)
            if(class(test)[1] == "try-error"){
                df <- df
            } else {
                df <- test
            }
        }

        df  # Return the filtered data
    })

    # Render filtered data table with horizontal scroll
    output$filtered_table <- renderDT({
        req(filtered_data_dynamic())
        datatable(filtered_data_dynamic(), options = list(
        pageLength = 10,
        scrollX = TRUE,  # Enable horizontal scrolling
        autoWidth = FALSE
        ), selection = "multiple")  
    })

    # Download filtered data (including subregion column)
    output$download <- downloadHandler(
        filename = function() {
        paste("filtered_data", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
        write.csv(filtered_data_dynamic(), file, row.names = FALSE)
        }
    )

    # Render map with selected locations
    observe({
        req(filtered_data_dynamic())
        selected_rows <- input$filtered_table_rows_selected  
        df <- filtered_data_dynamic()
        if (length(selected_rows) > 0) {
        selected_data <- df[selected_rows, ]
        output$map <- renderLeaflet({
            leaflet(selected_data) %>%
            addTiles() %>%
            addCircleMarkers(
                ~longitude, ~latitude, 
                radius = 5, color = "blue", fillOpacity = 0.7,
                label = ~paste(address_label)
            )
        })
        } else {
        output$map <- renderLeaflet({
            leaflet() %>%
            addTiles()  
        })
        }
    })

    # Load shapefile
    shapefile_data <- reactive({
        req(input$shapefile)
        # Ensure all required files are uploaded
        shp_files <- input$shapefile
        shp_path <- dirname(shp_files$datapath[1])
        new_shp_path <- file.path(shp_path, shp_files$name)
        file.rename(shp_files$datapath, new_shp_path)
        shp <- st_read(new_shp_path[grep("\\.shp$", new_shp_path)])
        shp
    })

    # Render dropdown to select polygon column
    output$polygon_column_select <- renderUI({
        req(shapefile_data())
        shapefile <- shapefile_data()
        selectInput("polygon_column", "Select Polygon/Geometry Variable", 
        choices = names(shapefile),  # Dynamically generate choices from shapefile columns
        selected = "geometry")  # Default selection (replace with your column name)
    })

    # Render dropdown to select subregion column
    output$subregion_column_select <- renderUI({
        req(shapefile_data())
        shapefile <- shapefile_data()
        selectInput("subregion_column", "Select Label Variable", 
        choices = names(shapefile),  # Dynamically generate choices from shapefile columns
        selected = "SA2_NAME21")  # Default selection (replace with your column name)
    })

    # Combine allocation and map rendering into a single event
    observeEvent(input$allocate_btn, {
        req(filtered_data(), shapefile_data(), input$polygon_column, input$subregion_column)
        geocoded_data <- filtered_data()  # Use the original dataset for spatial join
        shapefile <- shapefile_data()
        polygon_column <- input$polygon_column  # Get selected polygon column
        subregion_column <- input$subregion_column  # Get selected subregion column

        # Convert geocoded data to SF object
        geocoded_sf <- st_as_sf(geocoded_data, coords = c("longitude", "latitude"), crs = st_crs(shapefile), na.fail = FALSE)

        # Perform spatial join
        allocated <- st_join(geocoded_sf, shapefile, join = st_within)

        # Add subregion column to the main dataset
        allocated_df <- allocated %>%
        st_drop_geometry()  # Convert to a regular data frame

        # Count allocations per subregion
        allocation_counts <- allocated_df %>%
        count(!!sym(subregion_column))  # Use selected subregion column

        # Merge counts with shapefile
        shapefile <- merge(shapefile, allocation_counts, by = subregion_column, all.x = TRUE)
        shapefile <- shapefile[which(shapefile$n > 0), ]

        # Create a color palette
        pal <- colorNumeric("viridis", shapefile$n, na.color = "transparent")

        # Render subregion allocation map
        output$subregion_map <- renderLeaflet({
        leaflet(shapefile) %>%
            addTiles() %>%
            addPolygons(
            fillColor = ~pal(n),
            fillOpacity = 0.7,
            color = "black",
            weight = 1,
            label = ~paste("Subregion:", data.table(shapefile)[[subregion_column]], "Count:", n)  # Use selected subregion column
            )
        })
    })

    # Render geocode summary statistics
    output$matched_summary <- renderEcharts4r({
        req(filtered_data_dynamic())
        df <- filtered_data_dynamic()
        per <- round(((sum(df$matched) / nrow(df)) * 100), 2)
        e_charts() |> 
        e_gauge(per, "PERCENT") |> 
        e_title("Geocoded")
    })

    output$match_type_summary <- renderEcharts4r({
        req(filtered_data_dynamic())
        df <- filtered_data_dynamic()
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

    # Render filter dropdowns
    output$filter1 <- renderUI({
        req(filtered_data())
        selectInput("filter1", "Select First Filter Variable", choices = names(filtered_data()))
    })

    output$filter2 <- renderUI({
        req(filtered_data())
        selectInput("filter2", "Select Second Filter Variable", choices = names(filtered_data()))
    })

    # Render dropdowns for "Beyond Compare"
    output$compare_var1_select <- renderUI({
        req(filtered_data())
        selectInput("compare_var1", "Select First Variable", choices = names(filtered_data()))
    })

    output$compare_var2_select <- renderUI({
        req(filtered_data())
        selectInput("compare_var2", "Select Second Variable", choices = names(filtered_data()))
    })

    # Observe "Print to Console" button click
    observeEvent(input$print_btn, {
        req(filtered_data(), input$compare_var1, input$compare_var2)
        df <- filtered_data()
        selected_data <- df[, .SD, .SDcols = c(input$compare_var1, input$compare_var2)]
        print(selected_data)  # Print the selected data to the console
    })
    }

    if(file.exists( "C:/Program Files/Mozilla Firefox/firefox.exe")){
        options(browser = "C:/Program Files/Mozilla Firefox/firefox.exe")
        shinyApp(ui, server)
    } else if(file.exists("C:/Program Files/Internet Explorer/iexplore.exe")) {
        options(browser = "C:/Program Files/Internet Explorer/iexplore.exe")
        shinyApp(ui, server)
    } else {
         shinyApp(ui, server)
    }
   
}
