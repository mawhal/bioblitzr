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
