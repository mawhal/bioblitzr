### test script
# started on 13 June 2025

#run everything evryday 

# load packages(tidyverse)
library(lubridate)
library(tidyverse)
library(VennDiagram)
library(ggplot2)
library(taxize)

# practice reading files

d = read_csv("test_data/MarineGEOBC_bioblitz_specimen_20180403.csv")
k = read_csv("test_data/Koz_list.csv")




# goals for next week


#access to columns 
d
d$firstname=gsub("([A-Za-z]+).*", "\\1", d$`scientificName (morphospecies)`)
class(d) 
dnames = d$`scientificName (morphospecies)`
knames = k$ScientificName_accepted
d$firstname=gsub("([A-Za-z]+).*", "\\1", d$`scientificName (morphospecies)`)



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
dnames!="LOST LABELS"
dnames[1]
dnames[1:10]
dnames= dnames[dnames!="LOST LABELS"]
dnames= dnames[!is.na(dnames)]
dnames=unique(dnames)



# use regular expressions
gsub("(\\w+)", "\\1", knames) #unable to translate 'Lophaster furcilliger<a0>Fisher, 1905' to a wide string
dnamesfirst=unique(gsub("([A-Za-z]+).*", "\\1", dnames)) 
knamesfirst=gsub("([A-Za-z]+).*", "\\1", knames)

#use taxize to obtain 
dtax=taxize::classification(dnamesfirst,db="worms")


#venn.diagram()

venn.plot <- draw.pairwise.venn(length(knames), length(dnames), sum(dnames %in% knames), 
                                c("Kozloff", "2017 BioBlitz"),
                                fill = "orange")
grid.draw(venn.plot) 
grid.newpage()


#stacked bar plot 
data <- data.frame(
category = c("dnames", "knames", "na"),
  sub_category = c("X", "X", "X", "Y", "Y", "Y"),
  value = c(10, 15, 7, 12, 8, 11)
)

data_long <- data %>%
  pivot_longer(cols = c(value), names_to = "variable", values_to = "count")

ggplot(data_long, aes(x = category, y = count, fill = sub_category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Stacked Bar Chart",
       x = "Category",
       y = "Count",
       fill = "Sub-category") +
  theme_bw()

ggplot(data_long, aes(x = category, y = count, fill = sub_category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Stacked Bar Chart",
       x = "Category",
       y = "Count",
       fill = "Sub-category") +
  theme_bw()
