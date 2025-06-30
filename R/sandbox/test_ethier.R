# Test script on 6/13/25

# load packages
library(tidyverse)

# library(lubridate)


#practicing reading files
read.csv("test_data/MarineGEOBC_bioblitz_specimen_20180403.csv")
read_csv("test_data/MarineGEOBC_bioblitz_specimen_20180403.csv")

d<-read_csv("test_data/MarineGEOBC_bioblitz_specimen_20180403.csv")
k = read_csv("test_data/koz_list.csv")

view(d)
view(k)
m = read_csv("test_data/MarineGEOBC_bioblitz_station_20180403.csv")

class(d)

#access colums of data.frame using dollar sign
dnames = d$`scientificName (morphospecies)`
knames= k$ScientificName_accepted

# duplicates? (how to remove) look for the uniqe values
dnames = unique(dnames) #lots of duplicates
#(alt/shift/down) to copy
#command/shift/p- to search key commands
unique(knames) #all unique

#are the unique species names in d also in k?
dnames %in% knames

#how many species in Kozloff (k) list are in the BioBlitz data (d) set
knames %in% dnames

T + T
T + F
F + F
sum(dnames %in% knames)
sum(knames %in% dnames)

sum(knames %in% dnames) / length(knames)
paste( "percent found in Kozloff =", round( sum(knames %in% dnames) / length(knames) * 100,1), "%")

#how to make a venn diagram in R
install.packages("VennDiagram")
library("VennDiagram")

venn.plot <- draw.pairwise.venn(100, 70, 30, c ("First", "Second"))
grid.draw(venn.plot)
grid.newpage()

#getting it customized
venn.plot <- draw.pairwise.venn(length(knames), length(dnames), sum(dnames %in% knames), c ("Kozloff", "2017 BioBlitz"))
grid.draw(venn.plot)
grid.newpage()

#reading
dnames

#cleaning up data to account for missing data like LOST LABLE and NA

#how to omit NA
na.omit(dnames)
#or
is.na(dnames)
dnames=="LOST LABLES"
dnames=="LOST LABELS"
#^asking to identify any strings in the data as "LOST LABELS"
dnames!="LOST LABELS"
#^reverses the above identification
dnames[1]
#feeds back the [1] data in the data
dnames[1:10]
dnames[dnames!="LOST LABELS"]
dnames[!is.na(dnames)]
dnames=dnames[dnames!="LOST LABELS"]
dnames=dnames[!is.na(dnames)]
dnames


# use regular expressions
gsub("(\\w+)", "\\1", knames) #unable to translate 'Lophaster furcilliger<a0>Fisher, 1905' to a wide string
#^ wont remove first names, see code below for knames

gsub("([A-Za-z]+).*", "\\1", dnames)
gsub("([A-Za-z]+).*", "\\1", knames)


### Practice merging - 
# one need is to include geotags for each specimen
View(m)
mselect <- m %>% dplyr::select(eventID, decimalLatitude, decimalLongitude) 
dselect <- d %>% dplyr::select(eventID = `eventID (station #)`, morphospecies = `scientificName (morphospecies)`)

?selectdmerge <- left_join( dselect, mselect )

with(dmerge, plot(y = decimalLatitude, x = decimalLongitude) )



#look deeper into SF package to use the decimal corr. 

library(sf)
library(raster)
library(sp)

#code from Whalen
# transform CRS
dmerge2 <- st_as_sf(dmerge, coords = c("decimalLongitude","decimalLatitude"))
# simple_df2 <- st_transform( simple_df, crs = 4326)

#define raster resolution
bbox <- st_bbox(dmerge2)
res <- 30

#raster template
raster_template <- raster(extent(bbox), res = res)


#trying to clean dmerge missing data
dmerge= na.omit(dmerge)

dmerge2 <- st_as_sf(dmerge, coords = c("decimalLongitude","decimalLatitude") )

my_raster<- raster("dmerge2")


#wild shot inspired from Dr. Faisons code, 
dgraph <-ggplot(dmerge, aes(x = decimalLongitude, y = decimalLatitude))
geom_density()
dgraph
