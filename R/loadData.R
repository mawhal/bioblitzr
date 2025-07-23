### code to prepare user-supplied datasets to be used in other scripts





# single file approach - simplest approach
# taxon, taxonomy columns (lowercase), latitude, longitude (decimal degrees)

# comparison data should have the same format as the main bioblitz data if possible



# start of function to replace stacked_barplot (function will do more than stacked bar plots)
compareBlitz() <-  function( data = data, latitude = "latitude", longitude = "longitude", 
                             taxonomy_cols = c("kingdom", "phylum", "class", "order", "family", "genus", "species"),
                             data_compare = data_compare){
  
}

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
