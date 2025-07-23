# Load required libraries
library(sf)
library(dplyr)
library(readr)
library(bcmaps)
library(shiny)
library(leaflet)

# --- Data Preparation ---

# Read CSVs
d <- read_csv("test_data/MarineGEOBC_bioblitz_specimen_20180403.csv")
m <- read_csv("test_data/MarineGEOBC_bioblitz_station_20180403.csv")

# Select and clean
mselect <- m %>%
  select(eventID, decimalLatitude, decimalLongitude)

dselect <- d %>%
  select(
    eventID = `eventID (station #)`,
    morphospecies = `scientificName (morphospecies)`,
    Phylum = Phylum,
    Genus = Genus
  )

# Normalize eventID
mselect$eventID <- substr(mselect$eventID, 1, 6)
dselect$eventID <- substr(dselect$eventID, 1, 6)
mselect <- distinct(mselect)

# Merge species + coordinates
dmerge <- left_join(dselect, mselect, by = "eventID")
dmerge <- dmerge[!is.na(dmerge$decimalLatitude), ]

# Convert to sf and transform to both projections
dsf <- st_as_sf(dmerge, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
dsf_bc <- st_transform(dsf, crs = 3005)       # For analysis
dsf_leaflet <- dsf                            # For leaflet (keep in WGS84)

# Create 1km grid (BC Albers)
grid <- st_make_grid(dsf_bc, cellsize = 1000)
grid_sf <- st_as_sf(data.frame(ID = seq_along(grid), geometry = grid))

# Transform grid to Leaflet CRS
grid_leaflet <- st_transform(grid_sf, 4326)

# Optional BC boundary for reference (not used in Leaflet by default)
bcraw <- st_geometry(bc_bound())
bcraw <- st_transform(bcraw, 3005)


# --- Shiny App ---

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
