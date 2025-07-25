# Stacked bar plot for Phylum 

library(lubridate)
library(tidyverse)
library(VennDiagram)
library(ggplot2)


#files
d = read_csv("test_data/MarineGEOBC_bioblitz_specimen_20180403.csv")
k = read_csv("test_data/Koz_list.csv")


#access to columns 
d
d$firstname=gsub("([A-Za-z]+).*", "\\1", d$`scientificName (morphospecies)`)
class(d) 
dnames = d$`scientificName (morphospecies)`
knames = k$ScientificName_accepted
d$firstname=gsub("([A-Za-z]+).*", "\\1", d$`scientificName (morphospecies)`)
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

data_long
data_long <- phyla_compare %>%
  pivot_longer( !Phylum, names_to = "category", values_to = "count")

#test 
#  Sort Phylum alphabetically A–Z

data_long$Phylum <- factor(data_long$Phylum, levels = sort(unique(data_long$Phylum)))

#  Ensure categories match the color names
data_long$category <- factor(data_long$category, levels = c("confirmed", "new_report", "undetected"))


# Sort Phylum alphabetically A–Z (top to bottom in coord_flip)

ggplot( d = data_long, aes(y = count, x = fct_rev(Phylum), fill = category) ) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values = c("confirmed" = "red", "new_report" = "purple", "undetected" = "pink")) +
  labs(title = "Species Comparison by Phylum",
       x = "Phylum",
       y = "Species Count",
       fill = "Category") +
  theme_minimal()


#colors 
   # "confirmed" = "#EE7624",   # orange
  #  "new_report" = "#0047BB",  # sky blue
   # "undetected" = "#B026FF"   # teal green
#vsu <- c("#0047BB","#EE7624")
