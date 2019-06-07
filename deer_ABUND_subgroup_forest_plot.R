## Load libraries ####
library(metafor)
library(tidyverse)
library(metaviz)

## Import and load data ####
## Load data ####
source(
  "~/Desktop/side_projects/Crystal-Ornelas_et_al_deer_meta/scripts/deer_ma/deer_source_data.R"
)

### Calculate effects size
effect_sizes_abundance <-
  escalc(
    "SMD",
    # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
    m1i = abundance_raw_data$mean_t,
    n1i = abundance_raw_data$sample_size_t,
    # Then, follow with all of the columns needed to compute SMD
    sd1i = abundance_raw_data$SD_t,
    m2i = abundance_raw_data$mean_c,
    n2i = abundance_raw_data$sample_size_c,
    sd2i = abundance_raw_data$SD_c,
    data = abundance_raw_data
  )

# show the resulting data
effect_sizes_abundance 

## Only for nesting location
mixed_effect_abundance_nesting <- rma(
  yi,  # outcome
  vi,  # measure of variance
  mods = ~ Nesting_Location - 1, # multiple moderating variables modeled as main effects
  method = "REML",
  data = effect_sizes_abundance,
  weighted = TRUE,
  slab = paste(author, pub_year, sep = "")
)
mixed_effect_abundance_nesting

# Forest plot nesting data
mixed_effect_abundance_nesting
me_forest_plot_nesting <-
  viz_forest(
    x = mixed_effect_abundance_nesting,
    method = "REML",
    type = "summary_only",
    summary_label = c(
      "Brood Parasite",
      "Building",
      "Cavity",
      "Ground",
      "Shrub",
      "Tree"
    ),
    confidence_level = 0.95,
    xlab = "Hedge's g",
    col = "Greys",
    text_size = 7
  )
me_forest_plot_nesting
pdf(file = "~/Desktop/side_projects/Crystal-Ornelas_et_al_deer_meta/figures/forest_plot_abundance_and_nesting_location.pdf")
me_forest_plot_nesting
dev.off()
dev.off()

## To get all the real data, have to run separate rmas for each nesting type
rma_parasite <- rma(
  yi,  # outcome
  vi,  # measure of variance
  method = "REML",
  data = effect_sizes_abundance,
  subset = (Nesting_Location=="brood parasite"))
rma_parasite

rma_building <- rma(
  yi,  # outcome
  vi,  # measure of variance
  method = "REML",
  data = effect_sizes_abundance,
  subset = (Nesting_Location=="building"))
rma_building

rma_cavity <- rma(
  yi,  # outcome
  vi,  # measure of variance
  method = "REML",
  data = effect_sizes_abundance,
  subset = (Nesting_Location=="cavity"))
rma_cavity

rma_ground <- rma(
  yi,  # outcome
  vi,  # measure of variance
  method = "REML",
  data = effect_sizes_abundance,
  subset = (Nesting_Location=="ground"))
rma_ground

rma_shrub <- rma(
  yi,  # outcome
  vi,  # measure of variance
  method = "REML",
  data = effect_sizes_abundance,
  subset = (Nesting_Location=="shrub"))
rma_shrub

rma_tree <- rma(
  yi,  # outcome
  vi,  # measure of variance
  method = "REML",
  data = effect_sizes_abundance,
  subset = (Nesting_Location=="tree"))
rma_tree
