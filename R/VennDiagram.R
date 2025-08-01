library(VennDiagram)
library(grid)  # For grid.draw()

# Function to create and plot a pairwise Venn diagram
plot_venn_species <- function(set1, set2, label1 = "Set 1", label2 = "Set 2", fill_colors = c("orange", "blue")) {
  # Calculate sizes and intersection
  area1 <- length(set1)
  area2 <- length(set2)
  cross_area <- sum(set1 %in% set2)
  
  # Draw Venn diagram
  venn_plot <- draw.pairwise.venn(
    area1 = area1,
    area2 = area2,
    cross.area = cross_area,
    category = c(label1, label2),
    fill = fill_colors
  )
  
  # Draw on grid device
  grid.newpage()
  grid.draw(venn_plot)
}

# Example usage:
# plot_venn_species(dnames, knames, label1 = "2017 BioBlitz", label2 = "Kozloff", fill_colors = c("orange", "green"))