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

# code to repeat calculations for each Phylum
# use a "for loop" to calculate the same quantities over each Phylum
confirmed  <- c()
new_report <- c()
undetected <- c()

for( i in 1:length(unique(k$Phylum)) ){
  confirmed[i]  = sum(dcompare$species[dcompare$Phylum == unique(k$Phylum)[i]] %in% kcompare$species[kcompare$Phylum == unique(k$Phylum)[i]])
  new_report[i] = sum(!(dcompare$species[dcompare$Phylum == unique(k$Phylum)[i]] %in% kcompare$species[kcompare$Phylum == unique(k$Phylum)[i]]))
  undetected[i] = sum(!(kcompare$species[kcompare$Phylum == unique(k$Phylum)[i]] %in% dcompare$species[dcompare$Phylum == unique(k$Phylum)[i]]))
}

phyla_compare <- data.frame( Phylum = unique(k$Phylum), confirmed, new_report, undetected )

data_long <- phyla_compare %>%
  pivot_longer( !Phylum, names_to = "category", values_to = "count")


# stacked barplot
ggplot( d = data_long, aes(y = count, x = Phylum, fill = category) ) +
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


# convert to meters
utm_zone <- 9
# Transform to UTM (meters)
dsftr <- st_transform(dsf, crs = paste0("+proj=utm +zone=", utm_zone, " +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

dsftr <- st_transform( dsftr, crs = 3005 )
bctr <- st_transform( bcraw, crs = 3005 )
plot(bctr)
points(dsftr)


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
  group_by(ID) %>%
  summarize(point_count = n(), geometry = st_union(geometry)) %>%
  filter(point_count > 1)

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

###
#merging map and plot together- attempting
ggplot() +
  geom_sf(data = bcraw, fill = "gray90", color = "white") +  # base map
  geom_sf(data = density_data, aes(fill = point_count), color = NA, alpha = 0.8) +  # your density layer
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(fill = "Point Density")

#looking into getting density points pon map
st_crs(density_data)
st_crs(bcraw)

library(sf)
library(dplyr)

# Check how many points are inside the bcraw geometry
inside_bc <- st_within(density_data, bcraw, sparse = FALSE)

# How many are TRUE (inside)?
sum(inside_bc)  # this should be > 0

summary(st_coordinates(density_data))

# Fix: reproject to BC Albers
density_data <- st_transform(density_data, crs = 3005)
ggplot() +
  geom_sf(data = bcraw, fill = "gray90") +
  geom_sf(data = density_data, aes(fill = point_count), color = NA, alpha = 0.8) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(fill = "Point Density")

#TEST TO PLOT ON MAP/GRID

# Spatial join points to grid cells
joined <- st_join(grid, dsf)

# Count points per grid cell using ID, and preserve original grid shape
density_data <- joined %>%
  group_by(ID) %>%
  summarize(point_count = n(), geometry = st_union(geometry)) %>%
  filter(point_count > 1)

# OPTIONAL: crop to BC boundary (visually cleaner, but optional)
density_data <- st_intersection(density_data, bcraw)

ggplot() +
  geom_sf(data = bcraw, fill = "gray90", color = "white") +  # Base map
  geom_sf(data = density_data, aes(fill = point_count), color = NA, alpha = 0.8) +  # Density overlay
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(fill = "Point Density")

# Join points to grid cells
joined <- st_join(grid_sf, dsf)

# Count how many points per grid cell
density_data <- joined %>%
  group_by(ID) %>%
  summarize(point_count = n(), geometry = st_geometry(first(geometry))) %>%
  filter(point_count > 1)

ggplot() +
  geom_sf(data = bcraw, fill = "gray90", color = "white") +
  geom_sf(data = density_data, aes(fill = point_count), color = NA, alpha = 0.8) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(fill = "Point Density")

plot(st_geometry(density_data), col = "red")
str(density_data)
summary(density_data$point_count)

ggplot() +
  geom_sf(data = bcraw, fill = "gray90", color = "white") +
  geom_sf(data = density_data, aes(fill = point_count), 
          color = "black", lwd = 0.1, alpha = 1) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(fill = "Point Density")

coord_sf(xlim = c(st_bbox(density_data)$xmin, st_bbox(density_data)$xmax),
         ylim = c(st_bbox(density_data)$ymin, st_bbox(density_data)$ymax))

ggplot() +
  geom_sf(data = density_data, aes(fill = point_count), 
          color = "black", alpha = 1) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(fill = "Point Density")

plot(st_geometry(bcraw))
plot(st_geometry(density_data), col = "red", add = TRUE)


ggplot() +
  geom_sf(data = bcraw, fill = "gray90", color = "white") +
  geom_sf(data = density_data, aes(fill = point_count), 
          color = "black", alpha = 0.9) +
  scale_fill_viridis_c() +
  coord_sf(
    xlim = c(st_bbox(density_data)$xmin, st_bbox(density_data)$xmax),
    ylim = c(st_bbox(density_data)$ymin, st_bbox(density_data)$ymax)
  ) +
  theme_minimal() +
  labs(fill = "Point Density")

# Combine bounding boxes
combo_bbox <- st_bbox(st_union(bcraw, density_data))

# Optional padding
pad <- 10000  # meters

# Plot
ggplot() +
  geom_sf(data = bcraw, fill = "gray90", color = "white") +
  geom_sf(data = density_data, aes(fill = point_count), 
          color = "black", alpha = 0.9) +
  scale_fill_viridis_c() +
  coord_sf(
    xlim = c(combo_bbox["xmin"] - pad, combo_bbox["xmax"] + pad),
    ylim = c(combo_bbox["ymin"] - pad, combo_bbox["ymax"] + pad)
  ) +
  theme_minimal() +
  labs(fill = "Point Density")

bb1 <- st_bbox(bcraw)
bb2 <- st_bbox(density_data)

# Combine manually
combo_bbox <- c(
  xmin = min(bb1["xmin"], bb2["xmin"]),
  xmax = max(bb1["xmax"], bb2["xmax"]),
  ymin = min(bb1["ymin"], bb2["ymin"]),
  ymax = max(bb1["ymax"], bb2["ymax"])
)

pad <- 10000  # meters

ggplot() +
  geom_sf(data = bcraw, fill = "gray90", color = "white") +
  geom_sf(data = density_data, aes(fill = point_count), 
          color = "black", alpha = 0.9) +
  scale_fill_viridis_c() +
  coord_sf(
    xlim = c(combo_bbox["xmin"] - pad, combo_bbox["xmax"] + pad),
    ylim = c(combo_bbox["ymin"] - pad, combo_bbox["ymax"] + pad)
  ) +
  theme_minimal() +
  labs(fill = "Point Density")

# Clip BC map to density region
cropped_bc <- st_crop(bcraw, st_bbox(density_data))

ggplot() +
  geom_sf(data = cropped_bc, fill = "gray90", color = "white") +
  geom_sf(data = density_data, aes(fill = point_count), 
          color = "black", alpha = 0.9) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(fill = "Point Density")

#Create seperate maps of both data
par(mfrow = c(1, 2))

plot(st_geometry(bcraw), main = "BC Map", col = "lightgray", border = "black")
box()  # draw bounding box

plot(st_geometry(density_data), main = "Density Grid", col = "red")
box()

#checking points on both data sets.
st_bbox(bcraw)
st_bbox(density_data)

#new attempt?
dsf <- st_as_sf(dmerge, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)  # lat/lon
dsf <- st_transform(dsf, crs = 3005)  # reproject to match bcraw, by converting to meeter


joined <- st_join(grid_sf, dsf)

density_data <- joined %>%
  group_by(ID) %>%
  summarize(point_count = n(), geometry = st_geometry(first(geometry))) %>%
  filter(point_count > 0)

ggplot() +
  geom_sf(data = bcraw, fill = "gray90", color = "white") +
  geom_sf(data = density_data, aes(fill = point_count), 
          color = "black", alpha = 0.9) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(fill = "Point Density")

#This is the code that reconfigured to allow grid and data to align on map corrd.
dsf <- st_as_sf(dmerge, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)  # lat/lon
dsf <- st_transform(dsf, crs = 3005)  # reproject to match bcraw, by converting to meeter

grid <- st_make_grid(dsf, cellsize =)  # meters; tweak as needed
grid_sf <- st_as_sf(data.frame(ID = seq_along(grid), geometry = grid))
joined <- st_join(grid_sf, dsf)

density_data <- joined %>%
  group_by(ID) %>%
  summarize(point_count = n(), geometry = st_geometry(first(geometry))) %>%
  filter(point_count > 1)

#Creates a map that shows a WIDE view of the area
ggplot() +
  geom_sf(data = bcraw, fill = "gray90", color = "white") +
  geom_sf(data = density_data, aes(fill = point_count), color = "black", alpha = 0.9) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(fill = "Point Density")


#This creates a more zoomed in view of the area being studied. 
bbox <- st_bbox(density_data)
pad <- 1000

ggplot() +
  geom_sf(data = bcraw, fill = "gray90", color = "white") +
  geom_sf(data = density_data, aes(fill = point_count), color = "black", alpha = 0.9) +
  scale_fill_viridis_c() +
  coord_sf(
    xlim = c(bbox["xmin"] - pad, bbox["xmax"] + pad),
    ylim = c(bbox["ymin"] - pad, bbox["ymax"] + pad)
  ) +
  theme_minimal() +
  labs(fill = "Point Density")

#new work July 16th (morning run)
head(dsf$morphospecies)
joined <- st_join(grid_sf, dsf)
#calculate species richness
effort_species <- joined %>%
  group_by(ID) %>%
  summarize(species_effort = n_distinct(morphospecies),
            geometry = st_geometry(first(geometry)))
#plot results
ggplot() +
  geom_sf(data = effort_species, aes(fill = species_effort), color = "black", alpha = 0.9) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(fill = "Species Count (Effort)")

#
effort_combined <- left_join(density_data, st_drop_geometry(effort_species), by = "ID") %>%
  mutate(efficiency = point_count / species_effort)

bbox <- st_bbox(density_data)  # or use effort_combined for tighter bounds
pad <- 1000

ggplot() +
  geom_sf(data = bcraw, fill = "gray90", color = "white") +
  geom_sf(data = effort_species, aes(fill = species_effort), color = "black", alpha = 0.2) +
  scale_fill_viridis_c() +
  coord_sf(
    xlim = c(bbox["xmin"] - pad, bbox["xmax"] + pad),
    ylim = c(bbox["ymin"] - pad, bbox["ymax"] + pad)
  ) +
  theme_minimal() +
  labs(fill = "Species Count (Effort)")


ggplot() +
  geom_sf(data = bcraw, fill = "gray90", color = "white") +
  geom_sf(data = effort_combined, aes(fill = efficiency), color = "black", alpha = 0.3) +
  scale_fill_viridis_c() +
  coord_sf(
    xlim = c(bbox["xmin"] - pad, bbox["xmax"] + pad),
    ylim = c(bbox["ymin"] - pad, bbox["ymax"] + pad)
  ) +
  theme_minimal() +
  labs(fill = "Density per Species Effort")

