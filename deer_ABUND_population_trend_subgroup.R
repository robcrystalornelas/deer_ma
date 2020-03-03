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
mixed_effect_abundance_trend <- rma(
  yi,  # outcome
  vi,  # measure of variance
  mods = ~ IUCN_Trend - 1, # multiple moderating variables modeled as main effects
  method = "REML",
  data = effect_sizes_abundance,
  weighted = TRUE,
  slab = paste(author, pub_year, sep = "")
)
mixed_effect_abundance_trend

# Forest plot nesting data
# first, if we want to add in any summary-level info create a new summary table
plyr::count(effect_sizes_abundance$IUCN_Trend)
summary_table_population_trend <- data.frame(
  "Population Trend" = c("Decreasing","Increasing","Stable","Unknown"),
  N = c(33,35,62,1))
head(summary_table_population_trend)

me_forest_plot_population_trend <-
  viz_forest(
    x = mixed_effect_abundance_trend,
    method = "REML",
    type = "summary_only",
    summary_table = summary_table_population_trend,
    confidence_level = 0.95,
    xlab = "Hedges' g",
    col = "Greys",
    text_size = 7,
    annotate_CI = TRUE
  )

me_forest_plot_population_trend
pdf(file = "~/Desktop/side_projects/Crystal-Ornelas_et_al_deer_meta/figures/forest_plot_abundance_population_trend.pdf", width = 18, height = 5)
me_forest_plot_population_trend
dev.off()
dev.off()
dev.off()
head(effect_sizes_abundance)

## To get all the real data, have to run separate rmas for each nesting type
rma_decreasing <- rma(
  yi,  # outcome
  vi,  # measure of variance
  method = "REML",
  data = effect_sizes_abundance,
  subset = (IUCN_Trend =="Decreasing"))
rma_decreasing

rma_increasing <- rma(
  yi,  # outcome
  vi,  # measure of variance
  method = "REML",
  data = effect_sizes_abundance,
  subset = (IUCN_Trend =="Increasing"))
rma_increasing

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
