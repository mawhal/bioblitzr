# Load required libraries
library(tidyverse)
library(ggplot2)
library(VennDiagram)
library(lubridate)
library(forcats)

# -------------------------------------------------------
# Load and Prepare Data
# -------------------------------------------------------
d <- read_csv("test_data/MarineGEOBC_bioblitz_specimen_20180403.csv")
k <- read_csv("test_data/Koz_list.csv")

# Extract species names
dnames <- d$`scientificName (morphospecies)`
knames <- k$ScientificName_accepted

# Venn Diagram: shared species
venn.plot <- draw.pairwise.venn(
  area1 = length(knames),
  area2 = length(dnames),
  cross.area = sum(dnames %in% knames),
  category = c("Kozloff", "2017 BioBlitz"),
  fill = "orange"
)
grid.draw(venn.plot)
grid.newpage()

# -------------------------------------------------------
# Prepare Taxonomic Comparison Data
# -------------------------------------------------------
dcompare <- d %>%
  select(species = `scientificName (morphospecies)`, Phylum, Genus) %>%
  distinct()

kcompare <- k %>%
  select(species = ScientificName_accepted, Phylum, Genus) %>%
  distinct()

# -------------------------------------------------------
# Compare by Phylum
# -------------------------------------------------------
phyla <- sort(unique(kcompare$Phylum))
confirmed <- new_report <- undetected <- numeric(length(phyla))

for (i in seq_along(phyla)) {
  p <- phyla[i]
  spp_d <- dcompare$species[dcompare$Phylum == p]
  spp_k <- kcompare$species[kcompare$Phylum == p]
  
  confirmed[i]  <- sum(spp_d %in% spp_k)
  new_report[i] <- sum(!(spp_d %in% spp_k))
  undetected[i] <- sum(!(spp_k %in% spp_d))
}

phyla_compare <- tibble(
  Phylum = phyla,
  confirmed = confirmed,
  new_report = new_report,
  undetected = undetected
)

# -------------------------------------------------------
# Transform for Plotting
# -------------------------------------------------------
data_long <- phyla_compare %>%
  pivot_longer(cols = -Phylum, names_to = "category", values_to = "count") %>%
  mutate(
    Phylum = factor(Phylum, levels = sort(unique(Phylum))),
    category = factor(category, levels = c("confirmed", "new_report", "undetected"))
  )

# -------------------------------------------------------
# Plot: Stacked Bar by Phylum
# -------------------------------------------------------
ggplot(data_long, aes(x = fct_rev(Phylum), y = count, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values = c(
    "confirmed" = "#EE7624",     # orange
    "new_report" = "#0047BB",    # blue
    "undetected" = "#B026FF"     # purple
  )) +
  labs(
    title = "Species Comparison by Phylum",
    x = "Phylum",
    y = "Species Count",
    fill = "Category"
  ) +
  theme_minimal()