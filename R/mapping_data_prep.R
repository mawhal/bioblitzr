
# Minimal Script to Prepare Data for Interactive Shiny App

# Load required libraries
library(sf)
library(dplyr)
library(readr)

# Load data
d <- read_csv("test_data/MarineGEOBC_bioblitz_specimen_20180403.csv")
m <- read_csv("test_data/MarineGEOBC_bioblitz_station_20180403.csv")

# Select and clean station + species data
mselect <- m %>% dplyr::select(eventID, decimalLatitude, decimalLongitude)
dselect <- d %>% dplyr::select(eventID = `eventID (station #)`, morphospecies = `scientificName (morphospecies)`)

# Remove suffixes and duplicate station IDs
mselect$eventID <- substr(mselect$eventID,1,6)
dselect$eventID <- substr(dselect$eventID,1,6)
mselect <- distinct(mselect)

# Merge station coordinates with species data
dmerge <- left_join(dselect, mselect)
dmerge <- dmerge[!is.na(dmerge$decimalLatitude),]

# Convert to sf object with lat/lon and reproject to BC Albers (EPSG:3005)
dsf <- st_as_sf(dmerge, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
dsf <- st_transform(dsf, crs = 3005)

# Create grid around points
grid <- st_make_grid(dsf, cellsize = 1000)  # adjust resolution as needed
grid_sf <- st_as_sf(data.frame(ID = seq_along(grid), geometry = grid))

# Optional: Load BC boundary map and reproject
library(bcmaps)
bcraw <- st_geometry(bc_bound())
bcraw <- st_transform(bcraw, crs = 3005)