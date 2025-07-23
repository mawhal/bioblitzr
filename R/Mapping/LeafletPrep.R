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
