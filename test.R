library(shiny)
library(leaflet)
library(data.table)
library(sf)
library(dplyr)

# Sample data.table with SA2 assignments
d <- data.table(
  id = c(1, 2, 2, 2, 3, 3, 4, 4, 5, 5),
  effective_from = as.Date(c("2023-02-01", "2001-03-23", "2002-06-02", "2000-03-23", 
                             "2023-02-01", "2001-03-23", "2023-02-01", "2001-03-23", "2023-02-01", "2001-03-23")),
  effective_to = as.Date(c("2024-02-01", "2002-06-02", "2024-02-01", "2001-03-23", 
                           "2024-02-01", "2002-06-02", "2024-02-01", "2001-03-23", "2024-02-01", "2005-06-02")),
  SA2_CODE21 = c("305011105", "305011105", "305011105", "305011105", "305011105", 
                 "305021115", "305011110", "301031014", "305011110", "306021145"),
  SA2_NAME21 = c("Townsville North", "Townsville South", "Cairns North", "Cairns South",
                 "Brisbane North", "Brisbane South", "Gold Coast North", "Gold Coast South",
                 "Sunshine Coast", "Toowoomba")
)

d <- data.table(
    id = sample(1:1000, 40000, replace = T),
    effective_from = x <- as.Date(sample(1:1000, 40000, replace = T)),
    effective_to = x + 100,
    SA2_CODE21 = z <- sample(sf$SA2_CODE21, 40000, replace = T),
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

# Load the sf object (SA2 regions)
sf <- st_read("C:\\SA2_2021_AUST_SHP_GDA2020\\SA2_2021_AUST_GDA2020.shp")
sf <- sf[sf$STE_NAME21 %plike% "Queen", ]  # Filter for Queensland

# Merge sf with result to get geometry for "from" and "to" regions
result_sf <- result %>%
  left_join(sf, by = c("from_SA2_CODE21" = "SA2_CODE21")) %>%
  rename(from_geometry = geometry) %>%
  left_join(sf, by = c("to_SA2_CODE21" = "SA2_CODE21")) %>%
  rename(to_geometry = geometry)

library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# Shiny App
ui <- fluidPage(
  titlePanel("SA2 Region Movement Visualization"),
  leafletOutput("map", height = "800px")
)

server <- function(input, output, session) {
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

  # Reactive value to store the currently clicked SA2 region
  current_region <- reactiveVal(NULL)

  # Observe click events
  observeEvent(input$map_shape_click, {
    clicked_sa2 <- input$map_shape_click$id

    # Clear previous lines if a new region is clicked
    if (!is.null(current_region())) {
      leafletProxy("map") %>%
        clearGroup("movement_lines")
    }
# browser()
    # Filter movements involving the clicked SA2 region
    movements <- result_sf[from_SA2_CODE21 == clicked_sa2 | to_SA2_CODE21 == clicked_sa2]

    # Draw lines for movements
    if (nrow(movements) > 0) {
      leafletProxy("map") %>%
        addPolylines(
          data = movements,
          group = "movement_lines",
          lng = ~st_coordinates(st_centroid(from_geometry))[, 1],
          lat = ~st_coordinates(st_centroid(from_geometry))[, 2],
          layerId = ~paste0("line_", from_SA2_CODE21, "_", to_SA2_CODE21),
          color = "red",
          weight = 3,
          opacity = 1
        )
    }

    # Update the currently clicked region
    current_region(clicked_sa2)
  })
}

shinyApp(ui, server)