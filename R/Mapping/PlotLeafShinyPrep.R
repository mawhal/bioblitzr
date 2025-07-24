# Load libraries
library(shiny)
library(sf)
library(dplyr)
library(readr)
library(leaflet)
library(plotly)
library(bcmaps)

# === Load & preprocess data ===

# Read input CSVs
d <- read_csv("test_data/MarineGEOBC_bioblitz_specimen_20180403.csv")
m <- read_csv("test_data/MarineGEOBC_bioblitz_station_20180403.csv")

# Select and clean columns
mselect <- m %>%
  select(eventID, decimalLatitude, decimalLongitude)

dselect <- d %>%
  select(
    eventID = `eventID (station #)`,
    morphospecies = `scientificName (morphospecies)`,
    Phylum = Phylum,
    Genus = Genus
  )

# Format eventIDs and merge
mselect$eventID <- substr(mselect$eventID, 1, 6)
dselect$eventID <- substr(dselect$eventID, 1, 6)
mselect <- distinct(mselect)

dmerge <- left_join(dselect, mselect, by = "eventID")
dmerge <- dmerge[!is.na(dmerge$decimalLatitude), ]

# Convert to spatial data
dsf <- st_as_sf(dmerge, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
dsf <- st_transform(dsf, crs = 3005)

# Create 1km grid
grid <- st_make_grid(dsf, cellsize = 1000)
grid_sf <- st_as_sf(data.frame(ID = seq_along(grid), geometry = grid))


