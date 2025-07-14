### test script
# started on 13 June 2025

#run everything evryday 

# load packages(tidyverse)
library(lubridate)

# practice reading files
library(tidyverse)
library(VennDiagram)
library(ggplot2)
library(taxize)


d = read_csv("test_data/MarineGEOBC_bioblitz_specimen_20180403.csv")
k = read_csv("test_data/Koz_list.csv")





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

#use taxize to obtain # Only take a small sample first to avoid long waits or errors
dnamesfirst_clean <- dnamesfirst[1:20]  # Limit for testing

# Helper function to safely get classification
get_classification_safe <- function(name) {
  tryCatch({
    taxize::classification(name, db = "worms")
  }, error = function(e) {
    message(paste("Error retrieving:", name))
    return(NULL)
  })
}

dtax_list[["ligia"]]  # Replace with a name from your list

# Optional: Name the list
names(dtax_list) <- dnamesfirst_clean


#venn.diagram()

venn.plot <- draw.pairwise.venn(length(knames), length(dnames), sum(dnames %in% knames), 
                                c("Kozloff", "2017 BioBlitz"),
                                fill = "orange")
grid.draw(venn.plot) 
grid.newpage()

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
  confirmed[i] = sum(dcompare$species[dcompare$Phylum = unique(k$Phylum)[i]] %in% kcompare$species[kcompare$Phylum = unique(k$Phylum)[i]])
  new_report[i] 
  undetected[i]
}
  for( i in 1:length(unique(k$Phylum)) ){
    confirmed[i]  = sum(dcompare$species[dcompare$Phylum == unique(k$Phylum)[i]] %in% kcompare$species[kcompare$Phylum == unique(k$Phylum)[i]])
    new_report[i] = sum(!(dcompare$species[dcompare$Phylum == unique(k$Phylum)[i]] %in% kcompare$species[kcompare$Phylum == unique(k$Phylum)[i]]))
    undetected[i] = sum(!(kcompare$species[kcompare$Phylum == unique(k$Phylum)[i]] %in% dcompare$species[dcompare$Phylum == unique(k$Phylum)[i]]))
  }


#stacked bar plot 

taxize::classification()
data <- data.frame(
category = c("dnamesfirst", "knamesfirst", "na"),
  sub_category = c("dnames", "knames", "na", "Y", "Y", "Y"),
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
# taxize package 



taxonomy <- taxize::classification( vector_of_first_word, db = "worms" )
write_csv(taxonomy, "data/output/taxonomy.csv")
