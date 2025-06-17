### test script
# started on 13 June 2025

# load packages
library(tidyverse)
# library(lubridate)
library(VennDiagram)
#

# practice reading files
d = read_csv("test_data/MarineGEOBC_bioblitz_specimen_20180403.csv")
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
# - if time: 
# homework: we all read Andrew's recent paper on detecting extirpations
#   view PDF version for color images

