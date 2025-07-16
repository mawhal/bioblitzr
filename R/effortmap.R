#Interactivity for Desity per Species Effort
####
library(shiny)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)

# Assuming these are already loaded:
# - grid_sf: your spatial grid
# - dsf: species observation points (with `morphospecies`)
# - bcraw: basemap for context

# Create padded bounding box once
bbox <- st_bbox(grid_sf)
pad <- 0.02

# Shiny UI
ui <- fluidPage(
  titlePanel("Species-Specific Search Effort Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_species", "Choose a Species:",
                  choices = unique(dsf$morphospecies),
                  selected = unique(dsf$morphospecies)[1])
    ),
    mainPanel(
      plotOutput("effortMap", height = "600px")
    )
  )
)

# Shiny server
server <- function(input, output, session) {
  output$effortMap <- renderPlot({
    req(input$selected_species)
    
    # 1. Filter for the selected species
    filtered_points <- dsf %>%
      filter(morphospecies == input$selected_species)
    
    # 2. Join points to grid (only keep grid cells with matching species points)
    joined_species <- st_join(grid_sf, filtered_points, left = FALSE)
    
    # 3. Summarize effort
    species_effort <- joined_species %>%
      group_by(ID) %>%
      summarize(
        point_count = n(),
        species_effort = n_distinct(morphospecies),
        geometry = st_geometry(first(geometry)),
        .groups = "drop"
      ) %>%
      mutate(efficiency = point_count / species_effort)
    
    # 4. Plot
    ggplot() +
      geom_sf(data = bcraw, fill = "gray90", color = "white") +  # basemap
      geom_sf(data = species_effort, aes(fill = efficiency), color = "black", alpha = 0.8) +
      scale_fill_viridis_c(na.value = "transparent") +
      coord_sf(
        xlim = c(bbox["xmin"] - pad, bbox["xmax"] + pad),
        ylim = c(bbox["ymin"] - pad, bbox["ymax"] + pad)
      ) +
      theme_minimal() +
      labs(fill = "Efficiency")
  })
}

# Run app
shinyApp(ui, server)
