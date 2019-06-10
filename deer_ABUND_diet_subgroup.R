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

names(abundance_raw_data)

## Diet
mixed_effect_abundance_diet <- rma(
  yi,  # outcome
  vi,  # measure of variance
  mods = ~ Diet - 1, # multiple moderating variables modeled as main effects
  method = "REML",
  data = effect_sizes_abundance,
  weighted = TRUE,
  slab = paste(author, pub_year, sep = "")
)
mixed_effect_abundance_diet

plyr::count(abundance_raw_data$Diet)
summary_table_diet <- data.frame(
  "Diet" = c("Carnivore","Insectivore","Nectivore","Omnivore","Granivore"),
  "N" = c(4,88,2,16,21))
summary_table_diet

me_forest_plot_diet <-
  viz_forest(
    x = mixed_effect_abundance_diet,
    method = "REML",
    type = "summary_only",
    summary_table = summary_table_diet,
    confidence_level = 0.95,
    xlab = "Hedges' g",
    col = "Greys",
    text_size = 7,
    annotate_CI = TRUE
  )
me_forest_plot_diet
