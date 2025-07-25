### code to prepare user-supplied datasets to be used in other scripts





# single file approach - simplest approach
# taxon, taxonomy columns (uppercase to avoid confusion with function like class() and order()), latitude, longitude (decimal degrees)

# comparison data should have the same format as the main bioblitz data if possible





# other options and requirements to consider
# Things that must be included
# 1. taxon list
# 2. geolocations
# 3. comparative dataset with names (optional)
# ideally, numbers 1+2 above will be in the same datafile, but not always

# guide users to provide taxon list with taxonomy and geolocations

# Another option
# 1. taxon list only - can only make a map a whatever resolution is given. 
# 2. taxon list with taxonomy - can make maps at different levels, can add levels to comparison script
# 3. geolocation file - should have an ID that links observations with location (individual ID per observation or ID per area/sampling event)
# 

# Files
# 1. taxon list
#   requires column name "taxon"
#   should have a location ID column (if  geolocations already in taxon list table, then provide "taxon" as the ID)

# 2. taxonomy table
#   requires column name "taxon"
#   should only include levels that the user wants to investigate

# 3. geolocation data
#   currently operational only for lat/long tabular data 
#   must have an ID column that links observations and locations




# Option for user to pick the options for file in a directory to load



# Comparison data loading
#   requires column name "taxon" to link with 
#   should include taxonomy



# The remainder of this script is used to prepare datasets to use in testing package functions
library(tidyverse)

### Calvert Island BioBlitz Data
# files
# bioblitz specimens (occurrences/observations/samples)
d <- read_csv("test_data_raw/MarineGEOBC_bioblitz_specimen_20180403.csv")
# geolocations
m <- read_csv("test_data_raw/MarineGEOBC_bioblitz_station_20180403.csv")
# dataset for comparison (digitized species list for NE Pacific coast invertebrates)
k <- read_csv("test_data_raw/Koz_list.csv") 
# select and name columns
d <- d %>% 
  dplyr::select( taxon = `scientificName (morphospecies)`, id = `eventID (station #)`,
          Phylum, Genus) %>% 
  filter( !is.na(taxon) ) %>% # remove specimens without a name
  filter( taxon != "LOST LABELS" ) # remove specimens without metadata
d$firstname <-  gsub("([A-Za-z]+).*", "\\1", d$taxon )

m <- m %>% 
  dplyr::select( id = eventID, latitude = decimalLatitude, longitude = decimalLongitude)
k <- k %>% 
  dplyr::select( taxon = ScientificName_accepted, Phylum, Class, Order, Family, Genus, Species )
k$firstname <-  gsub("([A-Za-z]+).*", "\\1", k$taxon )

# Remove suffixes and duplicate station IDs
m$id <- substr(m$id,1,6)
d$id <- substr(d$id,1,6)
m <- distinct(m)

# Merge station coordinates with species data
# for this dataset, resolution of geolocations of sub ids (e.g., IHAK42a) makes merging difficult
m <- m %>% 
  group_by(id) %>% 
  summarize( latitude = mean(latitude), longitude = mean(longitude) )
dmerge <- left_join(d, m)
# dmerge <- dmerge[!is.na(dmerge$latitude),] # remove a few occurrences without geolocations




## use taxadb to grab taxonomic ranks 
# allnames <- unique(c(d$firstname, k$firstname))
# allnames <- allnames[ !is.na(allnames)]
# allnames <- data.frame( taxon = allnames )
# source("R/higher_rank.R")
# ranks <- higher_rank(allnames, taxon)
# write_csv(ranks, "test_data_clean/ranks.csv")

# read taxonomic ranks file
ranks <- read_csv("test_data_clean/ranks.csv")
dmerge <- left_join(dmerge, ranks)

# write test data to file
write_csv(dmerge, "test_data_clean/hakai17.csv")
write_csv(k, "test_data_clean/kozclean.csv")

  
####