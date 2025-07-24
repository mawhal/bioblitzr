# === SHINY APP ===

ui <- fluidPage(
  titlePanel("Species-Specific Search Effort Map (Leaflet + Plotly)"),
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
      leafletOutput("effortMap", height = "600px"),
      fluidRow(
        column(6, plotlyOutput("barChart")),
        column(6, plotlyOutput("pieChart"))
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    data <- dsf
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
    
    joined <- st_join(grid_sf, points, left = FALSE)
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
      setView(lng = -125, lat = 51, zoom = 6)
  })
  
  observe({
    data <- effort_data()
    points <- filtered_data()
    
    if (is.null(data) || nrow(points) == 0) {
      leafletProxy("effortMap") %>% clearShapes()
      return()
    }
    
    pal <- colorNumeric("viridis", domain = data$efficiency, na.color = "transparent")
    
    leafletProxy("effortMap") %>%
      clearShapes() %>%
      clearMarkers() %>%
      addPolygons(
        data = st_transform(data, crs = 4326),
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
  
  output$barChart <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(NULL)
    
    bar_data <- data %>%
      st_drop_geometry() %>%
      count(morphospecies, sort = TRUE) %>%
      top_n(10, wt = n)
    
    plot_ly(
      data = bar_data,
      x = ~reorder(morphospecies, n),
      y = ~n,
      type = "bar"
    ) %>%
      layout(
        title = "Top Morphospecies Counts",
        xaxis = list(title = "Morphospecies"),
        yaxis = list(title = "Count")
      )
  })
  
  output$pieChart <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(NULL)
    
    pie_data <- data %>%
      st_drop_geometry() %>%
      count(morphospecies, sort = TRUE) %>%
      top_n(10, wt = n)
    
    plot_ly(
      data = pie_data,
      labels = ~morphospecies,
      values = ~n,
      type = "pie"
    ) %>%
      layout(title = "Top 10 Morphospecies (Pie)")
  })
}

# Launch app
shinyApp(ui, server)
