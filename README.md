# bioblitzr
repository for an R package to facilitate visualization and analysis of BioBlitz data

06/11/2025 config

# BioDataCompareR: A general-purpose R package for comparing two biodiversity datasets

# This is a basic structure for setting up and using your R package

# How to install the development version (if using GitHub):
# install.packages("devtools")
# devtools::install_github("yourusername/BioDataCompareR")

# Load necessary packages
library(tidyverse)
library(VennDiagram)
library(ggplot2)

# Load your data (example with a sample .rds file)
bcraw <- readRDS("bcraw.rds")

# Example function to summarize species counts
summarize_species <- function(data, id_column = "phylum") {
  data %>%
    group_by(.data[[id_column]]) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
}

# Example function to generate a Venn diagram comparing two datasets
compare_taxa <- function(data1, data2, col = "phylum") {
  taxa1 <- unique(data1[[col]])
  taxa2 <- unique(data2[[col]])
  
  venn.plot <- draw.pairwise.venn(
    area1 = length(taxa1),
    area2 = length(taxa2),
    cross.area = length(intersect(taxa1, taxa2)),
    category = c("Dataset 1", "Dataset 2"),
    fill = c("skyblue", "pink"),
    lty = "blank"
  )
  
  grid.draw(venn.plot)
}

# If you don’t have historical data:
# Create a dummy dataset with relevant structure
generate_dummy_data <- function(n = 50, categories = c("phylum", "class")) {
  tibble(
    phylum = sample(c("Chordata", "Arthropoda", "Mollusca", "Annelida"), n, replace = TRUE),
    class = sample(c("Mammalia", "Insecta", "Gastropoda", "Polychaeta"), n, replace = TRUE)
  )
}

# Troubleshooting tips:
# - Make sure your .rds files are in the correct path
# - Ensure required columns like 'phylum' exist
# - Use `str(data)` to inspect the structure of your datasets