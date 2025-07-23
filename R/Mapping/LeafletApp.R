ui <- fluidPage(
  titlePanel("Species-Specific Search Effort Map (Leaflet)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_phylum", "Choose a Phylum:",
                  choices = c("All", sort(unique(dsf_leaflet$Phylum)))),
      selectInput("selected_genus", "Choose a Genus:",
                  choices = c("All", sort(unique(dsf_leaflet$Genus)))),
      selectInput("selected_species", "Choose a Species:",
                  choices = c("All", sort(unique(dsf_leaflet$morphospecies))))
    ),
    mainPanel(
      leafletOutput("effortMap", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- dsf_leaflet
    if (input$selected_phylum != "All") {
      data <- filter(data, Phylum == input$selected_phylum)
    }
    if (input$selected_genus != "All") {
      data <- filter(data, Genus == input$selected_genus)
    }
    if (input$selected_species != "All") {
      data <- filter(data, morphospecies == input$selected_species)
    }
    data
  })
  
  effort_data <- reactive({
    points <- filtered_data()
    if (nrow(points) == 0) return(NULL)
    
    joined <- st_join(grid_leaflet, points, left = FALSE)
    
    joined %>%
      group_by(ID) %>%
      summarize(
        point_count = n(),
        species_effort = n_distinct(morphospecies),
        geometry = st_geometry(first(geometry)),
        .groups = "drop"
      ) %>%
      mutate(efficiency = point_count / species_effort)
  })
  
  output$effortMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -125, lat = 51, zoom = 6)  # Approx center on BC
  })
  
  observe({
    data <- effort_data()
    if (is.null(data)) {
      leafletProxy("effortMap") %>% clearShapes()
      return()
    }
    
    pal <- colorNumeric("viridis", domain = data$efficiency, na.color = "transparent")
    
    leafletProxy("effortMap") %>%
      clearShapes() %>%
      addPolygons(
        data = data,
        fillColor = ~pal(efficiency),
        fillOpacity = 0.7,
        weight = 0.5,
        color = "black",
        popup = ~paste("Efficiency:", round(efficiency, 2))
      ) %>%
      addLegend("bottomright", pal = pal, values = data$efficiency,
                title = "Efficiency",
                opacity = 0.7)
  })
}

# Run the app
shinyApp(ui, server)
