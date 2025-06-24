### test script
# started on 13 June 2025

test_and_compare <- function(blitz_data, historical_data){
  
}

test_and_compare(dnames, knames)


# load packages
library(tidyverse)
# library(lubridate)
library(VennDiagram)
#

# practice reading files
d = read_csv("test_data/MarineGEOBC_bioblitz_specimen_20180403.csv")
m = read_csv("test_data/MarineGEOBC_bioblitz_station_20180403.csv")
k = read_csv("test_data/Koz_list.csv")

# goals for week 16 June 2025
# 1) comparing BioBlitz data to historical baselines (VENN)
# 2) explore possibilities for visualizations
# 3) possible with partner - updates on goals




# Visualization ideas
# 1) heatmap
# 2) Venn diagram



## Tasks for 16 June 2025

# compare morphospecies in specimen bioblitz data  and Scientificname accepted in kozloff
d
class(d)
# access columns of data.frame using the dollar sign
dnames = d$`scientificName (morphospecies)`
knames = k$ScientificName_accepted

# duplicates?
dnames = unique(dnames) # lots of duplicates
unique(knames) # all unique

# are the unique species names in d also in k?
dnames %in% knames
# TRUE
# FALSE
# T
# F
T + T
T + F
F + F
sum(dnames %in% knames)



# how many species from the Kozloff list (k) are in the Bioblitz dataset (d)
paste( "percent found in Kozloff =", 
       round( sum(knames %in% dnames) / 
                length(knames) * 100, 1),"%" )

# how to make to a Venn diagram in R?

venn.plot <- draw.pairwise.venn(length(knames), length(dnames), sum(dnames %in% knames), 
                                c("Kozloff", "2017 BioBlitz"),
                                fill = "orange")
grid.draw(venn.plot) 
grid.newpage()



## 17 June 2025
# goals:
# - learn how to deal with merge conflicts (always pull before you start working)
# - back to data - dealing with taxonomic resolution
# - if time: learn to branch repo
# homework: we all read Andrew's recent paper on detecting extirpations
#   view PDF version for color images


# taxonomic resolution
dnames
knames
# 

na.omit(dnames)
is.na(dnames)
dnames == "LOST LABELS"
dnames != "LOST LABELS"

# subsetting
dnames[1:10]
dnames = dnames[dnames != "LOST LABELS"]
dnames = dnames[!is.na(dnames)]



# use regular expressions
gsub("(\\w+)", "\\1", knames) #unable to translate 'Lophaster furcilliger<a0>Fisher, 1905' to a wide string
gsub("([A-Za-z]+).*", "\\1", dnames)
gsub("([A-Za-z]+).*", "\\1", knames)



### Notes
# add taxonomy to species or genus lists for later breakdown



### Practice merging - 
# one need is to include geotags for each specimen
View(m)
mselect <- m %>% select(eventID, decimalLatitude, decimalLongitude) 
dselect <- d %>% select(eventID = `eventID (station #)`, morphospecies = `scientificName (morphospecies)`)

dmerge <- left_join( dselect, mselect )

with(dmerge, plot(y = decimalLatitude, x = decimalLongitude) )
