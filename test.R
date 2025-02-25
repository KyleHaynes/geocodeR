if (FALSE) {

    library(shiny)
    library(leaflet)
    library(data.table)
    library(sf)
    library(dplyr)
    library(DT)
    library(bslib)
    library(bsicons)
    library(shinyjs)

  # Load the sf object (SA2 regions)
  sf <- st_read("C:\\SA2_2021_AUST_SHP_GDA2020\\SA2_2021_AUST_GDA2020.shp")
  sf <- sf[sf$STE_NAME21 %plike% "Queen", ]  # Filter for Queensland

#   # Simplify polygons
#   sf <- st_simplify(sf, dTolerance = 6)  # Adjust the threshold as needed

#   # Extract polygon geometries
#   sf <- st_collection_extract(sf, "POLYGON")

  # Sample data.table with SA2 assignments
  d <- data.table(
    id = sample(1:1000, 40000, replace = TRUE),
    effective_from = x <- as.Date(sample(1:1000, 40000, replace = TRUE)),
    effective_to = x + 100,
    SA2_CODE21 = z <- sample(sf$SA2_CODE21, 40000, replace = TRUE),
    SA2_NAME21 = sf$SA2_NAME21[match(z, sf$SA2_CODE21)]
  )

  # Sort by id and effective_from
  setorder(d, id, effective_from)

  # Create lagged columns for SA2_CODE21 and SA2_NAME21
  d[, `:=`(
    prev_SA2_CODE21 = shift(SA2_CODE21, type = "lag"),
    prev_SA2_NAME21 = shift(SA2_NAME21, type = "lag")
  ), by = id]

  # Identify transitions where the current SA2_CODE21 differs from the previous
  d[, moved := SA2_CODE21 != prev_SA2_CODE21]

  # Filter rows where a move occurred
  result <- d[moved == TRUE, .(
    id,
    from_SA2_CODE21 = prev_SA2_CODE21,
    from_SA2_NAME21 = prev_SA2_NAME21,
    to_SA2_CODE21 = SA2_CODE21,
    to_SA2_NAME21 = SA2_NAME21,
    move_date = effective_from
  )]

  result <- result[, .(count = .N), .(from_SA2_NAME21, to_SA2_NAME21)]

  # Merge sf with result to get geometry for "from" and "to" regions
  result_sf <- result %>%
    left_join(sf, by = c("from_SA2_CODE21" = "SA2_CODE21")) %>%
    rename(from_geometry = geometry) %>%
    left_join(sf, by = c("to_SA2_CODE21" = "SA2_CODE21")) %>%
    rename(to_geometry = geometry)

  # Precompute centroids for "from" and "to" geometries
  result_sf[, `:=`(
    from_centroid = lapply(from_geometry, function(x) st_coordinates(st_centroid(st_geometry(x)))),
    to_centroid = lapply(to_geometry, function(x) st_coordinates(st_centroid(st_geometry(x))))
  )]

  # Ensure centroids are numeric matrices
  result_sf[, `:=`(
    from_centroid = lapply(from_centroid, as.numeric),
    to_centroid = lapply(to_centroid, as.numeric)
  )]
}


# Shiny App
ui <- page_sidebar(
  title = "SA2 Region Movement Visualization",
  theme = bs_theme(bootswatch = "flatly"),
  useShinyjs(),
  
  sidebar = sidebar(
    width = 300,
    
    # Add a date range filter
    dateRangeInput("date_range", "Movement Date Range",
                  start = min(result$move_date, na.rm = TRUE),
                  end = max(result$move_date, na.rm = TRUE)),
    
    # Top origin/destination SA2 regions
    selectizeInput("region_filter", "Filter by Region", 
                  choices = c("All" = "", unique(sf$SA2_NAME21)), 
                  selected = "", multiple = FALSE),
    
    # Add a summary section
    h4("Movement Summary"),
    verbatimTextOutput("summary_stats"),
    
    # Add a download button for the filtered data
    downloadButton("download_data", "Download Data"),
    
    # Reset filters
    actionButton("reset_filters", "Reset All Filters", 
                class = "btn-danger", width = "100%"),
    
    # Add a help section
    hr(),
    helpText("Click on a region in the map to see its connections, or select rows in the table.")
  ),
  
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Total Movements",
      value = textOutput("total_moves"),
      showcase = bsicons::bs_icon("arrow-left-right"),
      theme = "primary"
    ),
    value_box(
      title = "Most Active Origin",
      value = textOutput("top_origin"),
      showcase = bsicons::bs_icon("box-arrow-right"),
      theme = "success"
    ),
    value_box(
      title = "Most Active Destination",
      value = textOutput("top_destination"),
      showcase = bsicons::bs_icon("box-arrow-in-right"),
      theme = "warning"
    )
  ),
  
  navset_card_tab(
    title = "SA2 Region Movement Data",
    nav_panel(
      title = "Map View",
      icon = bsicons::bs_icon("map"),
      div(
        style = "position: relative;",
        leafletOutput("map", height = "700px"),
        absolutePanel(
          top = 10, right = 10, width = 200, draggable = TRUE,
          wellPanel(
            h4("Legend"),
            tags$div(
              tags$div(
                tags$span(style = "background-color: green; width: 20px; height: 20px; display: inline-block; margin-right: 5px;"),
                "Origin Regions"
              ),
              tags$div(
                tags$span(style = "background-color: orange; width: 20px; height: 20px; display: inline-block; margin-right: 5px;"),
                "Destination Regions"
              ),
              tags$div(
                tags$span(style = "background-color: purple; width: 20px; height: 20px; display: inline-block; margin-right: 5px;"),
                "Selected Region (No Movement)"
              )
            )
          )
        )
      )
    ),
    nav_panel(
      title = "Data Table",
      icon = bsicons::bs_icon("table"),
      div(DTOutput("result_table"))
    ),
    nav_panel(
      title = "Movement Analysis",
      icon = bsicons::bs_icon("bar-chart"),
      fluidRow(
        column(6, plotlyOutput("origin_plot", height = "400px")),
        column(6, plotlyOutput("destination_plot", height = "400px"))
      ),
      fluidRow(
        column(12, plotlyOutput("time_series_plot", height = "400px"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Create a reactive dataset filtered by user inputs
  filtered_data <- reactive({
    data <- result
    
    # Apply date range filter if provided
    if (!is.null(input$date_range)) {
      data <- data[move_date >= input$date_range[1] & move_date <= input$date_range[2]]
    }
    
    # Apply region filter if provided
    if (!is.null(input$region_filter) && input$region_filter != "") {
      data <- data[from_SA2_NAME21 == input$region_filter | to_SA2_NAME21 == input$region_filter]
    }
    
    data
  })
  
  # Create a reactive SF dataset with geometry for filtered data
  filtered_sf_data <- reactive({
    data <- filtered_data()
    if (nrow(data) > 0) {
      result_sf %>%
        semi_join(data, by = c("id", "from_SA2_CODE21", "to_SA2_CODE21", "move_date"))
    } else {
      result_sf[0, ]  # Return empty dataframe with same structure
    }
  })
  
  # Helper function to safely create centroids
  safe_centroid <- function(geom) {
    if (is.null(geom)) return(NULL)
    
    tryCatch({
      # Try to calculate the centroid
      cent <- st_centroid(geom)
      coords <- st_coordinates(cent)
      if (is.matrix(coords) && nrow(coords) > 0) {
        return(coords[1, 1:2])
      } else {
        return(NULL)
      }
    }, error = function(e) {
      return(NULL)
    })
  }
  
  # Helper function to safely create lines
  create_line_sf <- function(from_points, to_points) {
    if (length(from_points) != length(to_points) || length(from_points) == 0) {
      return(NULL)
    }
    
    valid_idx <- sapply(1:length(from_points), function(i) {
      !is.null(from_points[[i]]) && !is.null(to_points[[i]]) &&
      length(from_points[[i]]) >= 2 && length(to_points[[i]]) >= 2
    })
    
    if (!any(valid_idx)) {
      return(NULL)
    }
    
    from_points <- from_points[valid_idx]
    to_points <- to_points[valid_idx]
    
    lines_sf <- st_sf(
      id = 1:length(from_points),
      geometry = st_sfc(
        lapply(1:length(from_points), function(i) {
          st_linestring(matrix(c(
            from_points[[i]][1], from_points[[i]][2],
            to_points[[i]][1], to_points[[i]][2]
          ), ncol = 2, byrow = TRUE))
        }),
        crs = st_crs(sf)
      )
    )
    
    return(lines_sf)
  }
  
  # Reset all filters
  observeEvent(input$reset_filters, {
    updateDateRangeInput(session, "date_range",
                        start = min(result$move_date, na.rm = TRUE),
                        end = max(result$move_date, na.rm = TRUE))
    updateSelectizeInput(session, "region_filter", selected = "")
    
    # Clear map selections
    leafletProxy("map") %>%
      clearGroup("selected_polygons") %>%
      clearGroup("movement_lines")
  })
  
  # Summary statistics
  output$summary_stats <- renderText({
    data <- filtered_data()
    
    # Count unique IDs, origins, and destinations
    unique_ids <- length(unique(data$id))
    unique_origins <- length(unique(data$from_SA2_NAME21))
    unique_destinations <- length(unique(data$to_SA2_NAME21))
    
    paste(
      "Unique Movers:", unique_ids, "\n",
      "Unique Origins:", unique_origins, "\n",
      "Unique Destinations:", unique_destinations
    )
  })
  
  # Value box outputs
  output$total_moves <- renderText({
    nrow(filtered_data())
  })
  
  output$top_origin <- renderText({
    data <- filtered_data()
    if (nrow(data) > 0) {
      top <- data[, .N, by = from_SA2_NAME21][order(-N)][1, from_SA2_NAME21]
      return(top)
    } else {
      return("None")
    }
  })
  
  output$top_destination <- renderText({
    data <- filtered_data()
    if (nrow(data) > 0) {
      top <- data[, .N, by = to_SA2_NAME21][order(-N)][1, to_SA2_NAME21]
      return(top)
    } else {
      return("None")
    }
  })
  
  # Render the DT datatable with filters
  output$result_table <- renderDT({
    datatable(
      filtered_data(),
      filter = "top",  # Add filters for each column
      selection = "multiple",  # Allow multiple row selection
      options = list(
        pageLength = 10,  # Show 10 rows per page
        autoWidth = TRUE,
        scrollX = TRUE
      )
    )
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("sa2_movements_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Base map
  output$map <- renderLeaflet({
    leaflet(sf) %>%
      addTiles() %>%
      addPolygons(
        layerId = ~SA2_CODE21,
        fillColor = "blue",
        fillOpacity = 0.1,  # Light blue fill for better visibility
        color = "black",
        weight = 1,
        label = ~paste("Name:", SA2_NAME21, "Code:", SA2_CODE21),
        highlightOptions = highlightOptions(
          color = "red",
          weight = 2,
          bringToFront = TRUE
        )
      ) %>%
      addScaleBar() %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      hideGroup("Satellite")
  })
  
  # Observe row selection in the DT datatable
  observeEvent(input$result_table_rows_selected, {
    selected_rows <- input$result_table_rows_selected
    
    # Clear previous layers
    leafletProxy("map") %>%
      clearGroup("selected_polygons") %>%
      clearGroup("movement_lines")
    
    if (!is.null(selected_rows) && length(selected_rows) > 0) {
      # Get the selected data
      selected_data <- filtered_data()[selected_rows, ]
      
      # Find all from regions
      from_sf <- sf %>%
        filter(SA2_CODE21 %in% selected_data$from_SA2_CODE21)
      
      # Find all to regions
      to_sf <- sf %>%
        filter(SA2_CODE21 %in% selected_data$to_SA2_CODE21)
      
      # Add from regions to map
      if (nrow(from_sf) > 0) {
        leafletProxy("map") %>%
          addPolygons(
            data = from_sf,
            group = "selected_polygons",
            fillColor = "green",
            fillOpacity = 0.5,
            color = "black",
            weight = 2,
            label = ~paste("From:", SA2_NAME21)
          )
      }
      
      # Add to regions to map
      if (nrow(to_sf) > 0) {
        leafletProxy("map") %>%
          addPolygons(
            data = to_sf,
            group = "selected_polygons",
            fillColor = "orange",
            fillOpacity = 0.5,
            color = "black",
            weight = 2,
            label = ~paste("To:", SA2_NAME21)
          )
      }
      
      # Create lines between centroids
      from_centroids <- lapply(st_geometry(from_sf), safe_centroid)
      to_centroids <- lapply(st_geometry(to_sf), safe_centroid)
      
      # Create lines between from and to centroids
      if (length(from_centroids) > 0 && length(to_centroids) > 0) {
        # Match centroids to the movement data
        movements_with_coords <- list()
        for (i in 1:nrow(selected_data)) {
          from_code <- selected_data$from_SA2_CODE21[i]
          to_code <- selected_data$to_SA2_CODE21[i]
          
          from_idx <- which(from_sf$SA2_CODE21 == from_code)
          to_idx <- which(to_sf$SA2_CODE21 == to_code)
          
          if (length(from_idx) > 0 && length(to_idx) > 0 && 
              !is.null(from_centroids[[from_idx[1]]]) && 
              !is.null(to_centroids[[to_idx[1]]])) {
            movements_with_coords[[length(movements_with_coords) + 1]] <- list(
              from = from_centroids[[from_idx[1]]],
              to = to_centroids[[to_idx[1]]]
            )
          }
        }
        
        if (length(movements_with_coords) > 0) {
          # Create line geometries
          lines_sf <- st_sf(
            id = 1:length(movements_with_coords),
            geometry = st_sfc(
              lapply(movements_with_coords, function(move) {
                st_linestring(matrix(c(
                  move$from[1], move$from[2],
                  move$to[1], move$to[2]
                ), ncol = 2, byrow = TRUE))
              }),
              crs = st_crs(sf)
            )
          )
          
          # Add lines to map
          leafletProxy("map") %>%
            addPolylines(
              data = lines_sf,
              group = "movement_lines",
              color = "blue",
              weight = 2,
              opacity = 0.8,
              dashArray = "5, 5"
            )
          
          # Add points at the start and end of each line
          from_points_sf <- st_sf(
            id = 1:length(movements_with_coords),
            geometry = st_sfc(
              lapply(movements_with_coords, function(move) {
                st_point(c(move$from[1], move$from[2]))
              }),
              crs = st_crs(sf)
            )
          )
          
          to_points_sf <- st_sf(
            id = 1:length(movements_with_coords),
            geometry = st_sfc(
              lapply(movements_with_coords, function(move) {
                st_point(c(move$to[1], move$to[2]))
              }),
              crs = st_crs(sf)
            )
          )
          
          leafletProxy("map") %>%
            addCircleMarkers(
              data = from_points_sf,
              radius = 4,
              color = "green",
              fillOpacity = 1,
              group = "movement_lines"
            ) %>%
            addCircleMarkers(
              data = to_points_sf,
              radius = 4,
              color = "orange",
              fillOpacity = 1,
              group = "movement_lines"
            )
        }
      }
    }
  })
  
  # Observe click events on the map
  observeEvent(input$map_shape_click, {
    clicked_sa2 <- input$map_shape_click$id
    
    if (!is.null(clicked_sa2)) {
      # Clear previous layers
      leafletProxy("map") %>%
        clearGroup("selected_polygons") %>%
        clearGroup("movement_lines")
      
      # Get clicked region name
      clicked_name <- sf$SA2_NAME21[sf$SA2_CODE21 == clicked_sa2][1]
      
      # Filter movements involving the clicked SA2 region
      clicked_movements <- result %>%
        filter(from_SA2_CODE21 == clicked_sa2 | to_SA2_CODE21 == clicked_sa2)
      
      # If there are movements, display them
      if (nrow(clicked_movements) > 0) {
        # Update the region filter to the clicked region
        updateSelectizeInput(
          session, 
          "region_filter", 
          selected = clicked_name
        )
        
        # Find all from regions
        from_sf <- sf %>%
          filter(SA2_CODE21 %in% clicked_movements$from_SA2_CODE21)
        
        # Find all to regions
        to_sf <- sf %>%
          filter(SA2_CODE21 %in% clicked_movements$to_SA2_CODE21)
        
        # Add regions to map
        leafletProxy("map") %>%
          addPolygons(
            data = from_sf,
            group = "selected_polygons",
            fillColor = "green",
            fillOpacity = 0.5,
            color = "black",
            weight = 2,
            label = ~paste("From:", SA2_NAME21)
          ) %>%
          addPolygons(
            data = to_sf,
            group = "selected_polygons",
            fillColor = "orange",
            fillOpacity = 0.5,
            color = "black",
            weight = 2,
            label = ~paste("To:", SA2_NAME21)
          )
        
        # Create lines between centroids
        from_centroids <- lapply(st_geometry(from_sf), safe_centroid)
        to_centroids <- lapply(st_geometry(to_sf), safe_centroid)
        
        # Create lines for valid movements
        movements_with_coords <- list()
        for (i in 1:nrow(clicked_movements)) {
          from_code <- clicked_movements$from_SA2_CODE21[i]
          to_code <- clicked_movements$to_SA2_CODE21[i]
          
          from_idx <- which(from_sf$SA2_CODE21 == from_code)
          to_idx <- which(to_sf$SA2_CODE21 == to_code)
          
          if (length(from_idx) > 0 && length(to_idx) > 0 && 
              !is.null(from_centroids[[from_idx[1]]]) && 
              !is.null(to_centroids[[to_idx[1]]])) {
            movements_with_coords[[length(movements_with_coords) + 1]] <- list(
              from = from_centroids[[from_idx[1]]],
              to = to_centroids[[to_idx[1]]]
            )
          }
        }
        
        if (length(movements_with_coords) > 0) {
          # Create line geometries
          lines_sf <- st_sf(
            id = 1:length(movements_with_coords),
            geometry = st_sfc(
              lapply(movements_with_coords, function(move) {
                st_linestring(matrix(c(
                  move$from[1], move$from[2],
                  move$to[1], move$to[2]
                ), ncol = 2, byrow = TRUE))
              }),
              crs = st_crs(sf)
            )
          )
          
          # Add lines to map
          leafletProxy("map") %>%
            addPolylines(
              data = lines_sf,
              group = "movement_lines",
              color = "blue",
              weight = 2,
              opacity = 0.8,
              dashArray = "5, 5"
            )
          
          # Add points at the start and end of each line
          from_points_sf <- st_sf(
            id = 1:length(movements_with_coords),
            geometry = st_sfc(
              lapply(movements_with_coords, function(move) {
                st_point(c(move$from[1], move$from[2]))
              }),
              crs = st_crs(sf)
            )
          )
          
          to_points_sf <- st_sf(
            id = 1:length(movements_with_coords),
            geometry = st_sfc(
              lapply(movements_with_coords, function(move) {
                st_point(c(move$to[1], move$to[2]))
              }),
              crs = st_crs(sf)
            )
          )
          
          leafletProxy("map") %>%
            addCircleMarkers(
              data = from_points_sf,
              radius = 4,
              color = "green",
              fillOpacity = 1,
              group = "movement_lines"
            ) %>%
            addCircleMarkers(
              data = to_points_sf,
              radius = 4,
              color = "orange",
              fillOpacity = 1,
              group = "movement_lines"
            )
        }
      } else {
        # Display a notification when no movements are found
        showNotification(
          paste("No movements found for region:", clicked_name),
          type = "warning",
          duration = 5
        )
        
        # Highlight the selected region with a different color
        clicked_sf <- sf[sf$SA2_CODE21 == clicked_sa2, ]
        
        leafletProxy("map") %>%
          addPolygons(
            data = clicked_sf,
            group = "selected_polygons",
            fillColor = "purple",
            fillOpacity = 0.5,
            color = "black",
            weight = 2,
            label = ~paste("Selected:", SA2_NAME21)
          )
      }
    }
  })
  
  # Create the origin plot
  output$origin_plot <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) > 0) {
      origin_counts <- data[, .N, by = from_SA2_NAME21][order(-N)][1:10]
      
      p <- plot_ly(
        origin_counts,
        x = ~reorder(from_SA2_NAME21, -N),
        y = ~N,
        type = "bar",
        marker = list(color = "green")
      ) %>%
        layout(
          title = "Top 10 Origin Regions",
          xaxis = list(title = "Region", tickangle = 45),
          yaxis = list(title = "Number of Movements")
        )
      
      return(p)
    } else {
      return(plotly::plot_ly() %>% 
               plotly::add_annotations(
                 text = "No data available",
                 showarrow = FALSE,
                 font = list(size = 20)
               ))
    }
  })
  
  # Create the destination plot
  output$destination_plot <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) > 0) {
      dest_counts <- data[, .N, by = to_SA2_NAME21][order(-N)][1:10]
      
      p <- plot_ly(
        dest_counts,
        x = ~reorder(to_SA2_NAME21, -N),
        y = ~N,
        type = "bar",
        marker = list(color = "orange")
      ) %>%
        layout(
          title = "Top 10 Destination Regions",
          xaxis = list(title = "Region", tickangle = 45),
          yaxis = list(title = "Number of Movements")
        )
      
      return(p)
    } else {
      return(plotly::plot_ly() %>% 
               plotly::add_annotations(
                 text = "No data available",
                 showarrow = FALSE,
                 font = list(size = 20)
               ))
    }
  })
  
  # Create the time series plot
  output$time_series_plot <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) > 0) {
      # Aggregate movements by month
      data[, month := format(move_date, "%Y-%m")]
      monthly_counts <- data[, .N, by = month][order(month)]
      
      p <- plot_ly(
        monthly_counts,
        x = ~month,
        y = ~N,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "blue", width = 2),
        marker = list(color = "blue", size = 8)
      ) %>%
        layout(
          title = "Movement Trends Over Time",
          xaxis = list(title = "Month"),
          yaxis = list(title = "Number of Movements")
        )
      
      return(p)
    } else {
      return(plotly::plot_ly() %>% 
               plotly::add_annotations(
                 text = "No data available",
                 showarrow = FALSE,
                 font = list(size = 20)
               ))
    }
  })
}

shinyApp(ui, server)