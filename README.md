# bioblitzr
repository for an R package to facilitate visualization and analysis of BioBlitz data

06/11/2025 config


How to install this package:


What this package can do for you:


Specific functions to use:


# ---------------------------------------------
# 🔧 Installation Instructions
# ---------------------------------------------

# 1. Clone this GitHub repository:
# (replace YOUR-REPOSITORY-NAME with the actual repo name)
# Run this in your terminal (not R):
# git clone https://github.com/YOUR-REPOSITORY-NAME.git

# 2. Set working directory in R to the cloned folder:
setwd("PATH/TO/CLONED/FOLDER")

# 3. Install required dependencies:
install.packages(c("tidyverse", "VennDiagram", "lubridate", "ggplot2", "taxize", "devtools"))

# 4. Install the package from the local source:
devtools::install()

# 5. Load the package:
library(bioblitzr)

# ---------------------------------------------
# 📌 What This Package Can Do
# ---------------------------------------------
# - Clean and standardize scientific names using `taxize`
# - Compare overlapping species between datasets
# - Create Venn diagrams of shared vs unique species
# - Summarize and visualize taxonomic data by Phylum
# - Generate exportable plots like stacked bar charts

# ---------------------------------------------
# 📁 Data Requirements
# ---------------------------------------------
# Two .csv files with:
# - A shared taxonomic column (e.g., scientific name)
# - Optionally: phylum, genus, collection dates

# ---------------------------------------------
# 🔍 Example Dependencies and Usage
# ---------------------------------------------

# Load packages used in plotting and taxonomic cleaning
library(tidyverse)
library(VennDiagram)
library(ggplot2)
library(taxize)

# ---------------------------------------------
# 🔧 Package Functions
# ---------------------------------------------

# 1. clean_names(df)
# Extracts the first word from collector name into `firstname`
# Input: dataframe with a collector name column
# Output: same dataframe with a new 'firstname' column

# 2. filter_na_taxa(df)
# Removes rows with missing taxonomic data (e.g., Phylum)
# Input: dataframe
# Output: cleaned dataframe

# 3. summarize_by_phylum(df)
# Summarizes record counts by phylum
# Input: dataframe with 'Phylum' column
# Output: summary dataframe

# 4. plot_phylum_barplot(df)
# Creates stacked barplot of species counts per phylum
# Input: dataframe with 'Phylum' and 'dataset' columns
# Output: ggplot2 object

# 5. get_taxonomic_classification(name)
# Uses taxize to retrieve full taxonomic classification
# Input: scientific name (string)
# Output: classification dataframe or list

# 6. compare_species_lists(df1, df2)
# Identifies shared and unique species
# Input: two dataframes with scientific names
# Output: list with shared, unique_df1, unique_df2

# 7. generate_venn(shared, unique_df1, unique_df2)
# Draws a Venn diagram comparing species lists
# Input: vectors of species
# Output: Venn diagram plot

# 8. read_and_prepare(file_path)
# Reads a CSV and performs basic cleaning
# Input: file path
# Output: ready-to-analyze dataframe
