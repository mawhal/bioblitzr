### test script
# started on 13 June 2025

#run everything evryday 

# load packages(tidyverse)
library(lubridate)
library(tidyverse)
library(VennDiagram)

# practice reading files

d = read_csv("test_data/MarineGEOBC_bioblitz_specimen_20180403.csv")
k = read_csv("test_data/Koz_list.csv")




# goals for next week


#access to columns 
d
class(d) 
dnames = d$`scientificName (morphospecies)`
knames = k$ScientificName_accepted

dnames
# duplicates? 
dnames = unique(dnames)
unique(knames)

dnames %in% knames 
#TRUE 
#FALSE
#T
#F 

T + F 
T + T
F + F

#removing NA 
clean_data <- na.omit(dnames)
is.na(dnames)
dnames=="LOST LABELS"
dnames !! "LOST LABELS"
dnames[1]
dnames[1:10]
dnames= dnames[dnames!="LOST LABELS"]
dnames= dnames[!is.na(dnames)]


#venn.diagram()
venn.plot <- draw.pairwise.venn(100, 70, 30, c("First", "Second"))
grid.draw(venn.plot) 
grid.newpage()

venn.plot <- draw.pairwise.venn(T + T , F + T , F + F, c("First", "Second"))


