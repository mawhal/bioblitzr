# Stacked bar plot for Phylum 

# library(lubridate)
library(tidyverse)
library(VennDiagram)
library(ggplot2)

d_focus = read_csv("test_data_clean/hakai17.csv")
d_compare = read_csv("test_data_clean/kozclean.csv")

# start of function to replace stacked_barplot (function will do more than stacked bar plots)
compareBlitz <-  function( d_focus, taxonomy_cols, rank_focus, d_compare, ... ){
  
  # Venn diagram of all taxa in the dataset
  grid.newpage()
  venn.plot <- draw.pairwise.venn(length(unique(d_focus$taxon)), length(unique(d_compare$taxon)), sum(unique(d_focus$taxon) %in% unique(d_compare$taxon)), 
                                  rev(c("focal\nobservations", "comparison")),
                                  ext.text = FALSE, 
                                  # cat.pos = c(0, 0),
                                  inverted = TRUE,
                                   fill = c("#0047BB","#EE7624"),
                                  lty = "blank",
                                  cex = 2,
                                  cat.cex = 1.5,
                                  cat.pos = c(285, 105),
                                  cat.dist = c(0.09,0),
                                  # cat.just = list(c(-1, -1), c(1, 1)),
                                  ext.pos = 0,
                                  ext.dist = -0.05,
                                  ext.length = 0.85,
                                  ext.line.lwd = 2,
                                  ext.line.lty = "dashed" )
  return( grid.draw(venn.plot) )
  
  
  
  # code to repeat calculations for each Phylum
  # use a "for loop" to calculate the same quantities over each Phylum
  confirmed  <- c()
  new_report <- c()
  undetected <- c()
  d_focus$Phylum <- d_focus$phylum
  
  for( i in 1:length(unique(d_compare$Phylum)) ){
    confirmed[i]  = sum(d_focus$taxon[d_focus$Phylum == unique(d_compare$Phylum)[i]] %in% d_compare$taxon[d_compare$Phylum == unique(d_compare$Phylum)[i]])
    new_report[i] = sum(!(d_focus$taxon[d_focus$Phylum == unique(d_compare$Phylum)[i]] %in% d_compare$taxon[d_compare$Phylum == unique(d_compare$Phylum)[i]]))
    undetected[i] = sum(!(d_compare$taxon[d_compare$Phylum == unique(d_compare$Phylum)[i]] %in% d_focus$taxon[d_focus$Phylum == unique(d_compare$Phylum)[i]]))
  }
  
  
  
  
  phyla_compare <- data.frame( Phylum = unique(d_compare$Phylum), confirmed, new_report, undetected )
  
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
  
  
  
}










