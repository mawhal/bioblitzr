library(shiny)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)

# Precalculated objects assumed loaded: dsf, grid_sf, bcraw

bbox <- st_bbox(grid_sf)
pad <- 0.02

ui <- fluidPage(
  titlePanel("Search Effort by Species, Genus, and Phylum"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_phylum", "Choose a Phylum:",
                  choices = c("All", sort(unique(dsf$Phylum)))),
      
      selectInput("selected_genus", "Choose a Genus:",
                  choices = c("All", sort(unique(dsf$Genus)))),
      
      selectInput("selected_species", "Choose a Species:",
                  choices = c("All", sort(unique(dsf$morphospecies))))
    ),
    mainPanel(
      plotOutput("effortMap", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  output$effortMap <- renderPlot({
    filtered_points <- dsf
    
    # Apply each filter if not "All"
    if (input$selected_phylum != "All") {
      filtered_points <- filtered_points %>% filter(Phylum == input$selected_phylum)
    }
    if (input$selected_genus != "All") {
      filtered_points <- filtered_points %>% filter(Genus == input$selected_genus)
    }
    if (input$selected_species != "All") {
      filtered_points <- filtered_points %>% filter(morphospecies == input$selected_species)
    }
    
    # Join to grid and calculate efficiency
    joined_species <- st_join(grid_sf, filtered_points, left = FALSE)
    
    species_effort <- joined_species %>%
      group_by(ID) %>%
      summarize(
        point_count = n(),
        species_effort = n_distinct(morphospecies),
        geometry = st_geometry(first(geometry)),
        .groups = "drop"
      ) %>%
      mutate(efficiency = point_count / species_effort) %>%
      filter(!is.na(efficiency) & efficiency > 0)
    
    ggplot() +
      geom_sf(data = bcraw, fill = "gray90", color = "white") +
      geom_sf(data = species_effort, aes(fill = efficiency), color = "black", alpha = 0.8) +
      scale_fill_viridis_c(na.value = "transparent") +
      coord_sf(
        xlim = c(bbox["xmin"] - pad, bbox["xmax"] + pad),
        ylim = c(bbox["ymin"] - pad, bbox["ymax"] + pad)
      ) +
      theme_minimal() +
      labs(fill = "Effort Efficiency")
  })
}

shinyApp(ui, server)

