library(shiny)
library(leaflet)
library(data.table)
library(sf)
library(dplyr)
library(DT)
library(bslib)

if (FALSE) {
  # Load the sf object (SA2 regions)
  sf <- st_read("C:\\SA2_2021_AUST_SHP_GDA2020\\SA2_2021_AUST_GDA2020.shp")
  sf <- sf[sf$STE_NAME21 %plike% "Queen", ]  # Filter for Queensland

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
  sidebar = sidebar(
    width = 300,
    DTOutput("result_table")  # DT datatable with filters
  ),
  leafletOutput("map", height = "800px")  # Leaflet map
)

server <- function(input, output, session) {
  # Render the DT datatable with filters
  output$result_table <- renderDT({
    datatable(
      result,
      filter = "top",  # Add filters for each column
      selection = "multiple",  # Allow multiple row selection
      options = list(
        pageLength = 10,  # Show 10 rows per page
        autoWidth = TRUE
      )
    )
  })

  # Base map
  output$map <- renderLeaflet({
    leaflet(sf) %>%
      addTiles() %>%
      addPolygons(
        layerId = ~SA2_CODE21,
        fillColor = "blue",
        fillOpacity = 0,
        color = "black",
        weight = 1,
        label = ~SA2_NAME21,
        highlightOptions = highlightOptions(
          color = "red",
          weight = 2,
          bringToFront = TRUE
        )
      )
  })

  # Observe row selection in the DT datatable
  observeEvent(input$result_table_rows_selected, {
    selected_rows <- input$result_table_rows_selected

    if (!is.null(selected_rows)) {
      # Clear previous layers
      leafletProxy("map") %>%
        clearGroup("selected_polygons") %>%
        clearGroup("movement_lines")

      # Get the selected data
      selected_data <- result_sf[selected_rows, ]

      # Draw all selected polygons and lines
      for (i in seq_len(nrow(selected_data))) {
        # Draw the from and to polygons
        leafletProxy("map") %>%
          addPolygons(
            data = selected_data$from_geometry[i],
            group = "selected_polygons",
            fillColor = "green",
            fillOpacity = 0.5,
            color = "black",
            weight = 2
          ) %>%
          addPolygons(
            data = selected_data$to_geometry[i],
            group = "selected_polygons",
            fillColor = "orange",
            fillOpacity = 0.5,
            color = "black",
            weight = 2
          )

        # Draw a line between the from and to regions
        leafletProxy("map") %>%
          addPolylines(
            data = st_linestring(matrix(c(
              selected_data$from_centroid[[i]],
              selected_data$to_centroid[[i]]
            ), ncol = 2, byrow = TRUE)),
            group = "movement_lines",
            color = "red",
            weight = 3,
            opacity = 1
          )
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

      # Filter movements involving the clicked SA2 region
      movements <- result_sf[from_SA2_CODE21 == clicked_sa2 | to_SA2_CODE21 == clicked_sa2, ]

      if (nrow(movements) > 0) {
        # Draw all connections involving the clicked region
        for (i in seq_len(nrow(movements))) {
          # Draw the from and to polygons
          leafletProxy("map") %>%
            addPolygons(
              data = movements$from_geometry[i],
              group = "selected_polygons",
              fillColor = "green",
              fillOpacity = 0.5,
              color = "black",
              weight = 2
            ) %>%
            addPolygons(
              data = movements$to_geometry[i],
              group = "selected_polygons",
              fillColor = "orange",
              fillOpacity = 0.5,
              color = "black",
              weight = 2
            )

          # Draw a line between the from and to regions
          leafletProxy("map") %>%
            addPolylines(
              data = st_linestring(matrix(c(
                movements$from_centroid[[i]],
                movements$to_centroid[[i]]
              ), ncol = 2, byrow = TRUE)),
              group = "movement_lines",
              color = "red",
              weight = 3,
              opacity = 1
            )
        }
      }
    }
  })
}

shinyApp(ui, server)