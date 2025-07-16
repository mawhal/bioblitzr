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


length(knames)
length(dnames)
sum(dnames %in% knames)


unique(k$Phylum)
unique(d$Phylum)
unique(d$Phylum) %in% unique(k$Phylum)
unique(d$Phylum)[!(unique(d$Phylum) %in% unique(k$Phylum))]

dcompare <- d %>% dplyr::select( species = `scientificName (morphospecies)`,
                          Phylum, Genus) %>% distinct() 
kcompare <- k %>% dplyr::select( species = ScientificName_accepted,
                          Phylum, Genus) %>% distinct() 



###----------------------------------
# new code on 15 July 2025
# goals
# use galiano data with stacked barplot code
originalSummary <- read.csv("test_data/Galiano_Tracheophyta_review_summary_reviewed_2024-10-07-assigned_revised.csv")
originalSummary$status.short <- originalSummary$Reporting.Status
originalSummary$status.short <- gsub("new.*", "new", originalSummary$status.short)
d <- originalSummary %>% 
  filter( status.short %in% c("new", "confirmed"))
k <- originalSummary %>% 
  filter( status.short %in% c("reported", "confirmed"))

# add flexibility for taxonomic level to compare
level = "Family"
d$level <- d[, level]
k$level <- k[, level]

dcompare <- d %>% dplyr::select( species = Taxon,
                                 level, Genus) %>% distinct() 
kcompare <- k %>% dplyr::select( species = Taxon,
                                 level, Genus) %>% distinct() 


###---------------------------------





# code to repeat calculations for each Phylum
# use a "for loop" to calculate the same quantities over each Phylum
confirmed  <- c()
new_report <- c()
undetected <- c()

levels_all <- c(k$level,d$level)
levels_sort <- sort(unique(unique(levels_all)))

for( i in 1:length(level_sort) ){
  confirmed[i]  = sum(dcompare$species[dcompare$level == levels_sort[i]] %in% kcompare$species[kcompare$level == levels_sort[i]])
  new_report[i] = sum(!(dcompare$species[dcompare$level == levels_sort[i]] %in% kcompare$species[kcompare$level == levels_sort[i]]))
  undetected[i] = sum(!(kcompare$species[kcompare$level == levels_sort[i]] %in% dcompare$species[dcompare$level == levels_sort[i]]))
}

phyla_compare <- data.frame( level = levels_sort, confirmed, new_report, undetected )

data_long <- phyla_compare %>%
  pivot_longer( !level, names_to = "category", values_to = "count")

#
# Vector of plant families (All families you want to include)
plant_families <- c("Pinaceae", "Sapindaceae", "Berberidaceae", "Fabaceae", "Asteraceae", 
                    "Poaceae", "Lamiaceae", "Rosaceae", "Amaryllidaceae", "Betulaceae", 
                    "Apiaceae", "Orobanchaceae", "Ericaceae", "Aspleniaceae", "Athyriaceae", 
                    "Amaranthaceae", "Asparagaceae", "Brassicaceae", "Orchidaceae", "Cyperaceae", 
                    "Caryophyllaceae", "Montiaceae", "Thymelaeaceae", "Ranunculaceae", 
                    "Dryopteridaceae", "Onagraceae", "Equisetaceae", "Geraniaceae", "Phrymaceae", 
                    "Liliaceae", "Rubiaceae", "Saxifragaceae", "Aquifoliaceae", "Caprifoliaceae", 
                    "Juncaceae", "Araceae", "Primulaceae", "Malvaceae", "Boraginaceae", 
                    "Celastraceae", "Pteridaceae", "Plantaginaceae", "Polypodiaceae", 
                    "Dennstaedtiaceae", "Fagaceae", "Grossulariaceae", "Polygonaceae", 
                    "Salicaceae", "Viburnaceae", "Crassulaceae", "Selaginellaceae", 
                    "Blechnaceae", "Taxaceae", "Cupressaceae", "Melanthiaceae", "Violaceae", "Zosteraceae")

# filter by family
data_long <- data_long %>% filter( level %in% plant_families )

# stacked barplot
ggplot( d = data_long, aes(y = count, x = fct_rev(level), fill = category) ) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip()


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
mselect <- m %>% dplyr::select(eventID, decimalLatitude, decimalLongitude) 
dselect <- d %>% dplyr::select(eventID = `eventID (station #)`, morphospecies = `scientificName (morphospecies)`)

dmerge <- left_join( dselect, mselect )

with(dmerge, plot(y = decimalLatitude, x = decimalLongitude) )

# redo the merge without the suffix letters (e.g., IHAK23c vs IHAK23)
mselect$eventID <- substr(mselect$eventID,1,6)
mselect <- distinct(mselect)
dselect$eventID <- substr(dselect$eventID,1,6)

dmerge <- left_join( dselect, mselect )
# remove NA values - some of which need to be removed anyway
dmerge <- dmerge[ !is.na(dmerge$decimalLatitude),]
with(dmerge, plot(y = decimalLatitude, x = decimalLongitude) )


# practice with making grid and working with geolocation to assess total counts, effort, etc

# load libraries
# library(USAboundaries)
library(bcmaps)
library(sf)
library(raster)


# access baselayer map - code adapted from BIOL490 urban ecology course taught in Fall 2024
bcbase <- bc_bound()
bcraw <- st_geometry( bcbase )
# bc <- bcraw
plot(bcraw)

# convert geolocation data
dmerge[is.na(dmerge$decimalLatitude),]
dsf <- st_as_sf( dmerge, coords = c("decimalLongitude","decimalLatitude"),
                                           crs = 3005 ) 
# convert to meters
utm_zone <- 9
# Transform to UTM (meters)
dsftr <- st_transform(dsf, crs = paste0("+proj=utm +zone=", utm_zone, " +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

dsftr <- st_transform( dsftr, crs = 3005 )
bctr <- st_transform( bcraw, crs = 3005 )
plot(bctr)
points(dsftr)

projection(dsf) <- projection(bcraw)
st_crs(dsf) <- st_crs(bcraw)
st_crs(dsf)


# crop map to bounding box of data
bbox <- st_bbox(dsf)
res <- 0.3

bccrop <- st_crop( bcraw, bbox )
plot(bccrop)
#
# Potentially helpful links
# <https://stackoverflow.com/questions/48437569/how-to-crop-a-polygon-from-an-underlying-grid-with-the-r-sf-package>
#
#

# grid
grid <- st_make_grid(dsf, cellsize = .03)
plot(grid)
points(dsf)

area <- st_area(grid)
grid <- st_as_sf(data.frame(ID=c(1:length(area)), area=area, geometry=grid))
plot(grid)
points(dsf)


tmp <- st_intersection(grid, dsf)
tmp$area <- st_area(tmp)
tmp$frac <- tmp$area/unique(round(area,6))
summary(tmp$frac)
plot(tmp['frac'])


### number of points per grid cell
# see <https://www.google.com/search?q=r+sf+point+density+in+grid&rlz=1C1JZAP_enCA978CA978&oq=r+sf+point+density+in+grid&gs_lcrp=EgZjaHJvbWUyBggAEEUYOTIHCAEQIRigATIHCAIQIRigATIHCAMQIRigATIHCAQQIRigATIHCAUQIRigATIHCAYQIRirAjIHCAcQIRirAjIHCAgQIRifBTIHCAkQIRifBdIBCDg5NzZqMGo0qAIAsAIB&sourceid=chrome&ie=UTF-8>
# see also <https://dieghernan.github.io/202312_bertin_dots/#:~:text=Beautiful%20Maps%20with%20R%20(V):%20Point%20densities.&text=The%20issue%20here%20is%20that%20terra%20does,than%20each%20cell%20on%20the%20aggregated%20raster.>
# Perform spatial join
joined_data <- st_join(grid, dsf)

# Count points per cell
density_data <- joined_data %>%
  group_by(geometry) %>%
  summarize(point_count = n()) %>% 
  filter( point_count > 1)

summary(density_data$point_count)
unique(density_data$point_count)

# Visualize using ggplot2
ggplot() +
  geom_sf(data = density_data, aes(fill = point_count), color = NA) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(fill = "Point Density")
plot.new()
plot(dsf)
