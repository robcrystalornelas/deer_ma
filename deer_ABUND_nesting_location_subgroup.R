## Load libraries ####
library(metafor)
library(tidyverse)
library(metaviz)

## Import and load data ####
## Load data ####
source(
  "~/Desktop/research/side_projects/Crystal-Ornelas_et_al_deer_meta/scripts/deer_ma/deer_source_data.R"
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
mixed_effect_abundance_nesting <- rma.mv(
  yi,  # outcome
  vi,  # measure of variance
  mods = ~ Nesting_Location - 1, # multiple moderating variables modeled as main effects
  random = ~ 1 | unique_id,
  method = "REML",
  digits = 4,
  data = effect_sizes_abundance
)
mixed_effect_abundance_nesting

# Forest plot nesting data
subgroup_samplesize <- c(2,1,25,28,37,38)
forest_plot_nesting_location <- forest(mixed_effect_abundance_nesting$b,
                                     ci.lb = mixed_effect_abundance_nesting$ci.lb,
                                     ci.ub = mixed_effect_abundance_nesting$ci.ub,
                                     ilab = subgroup_samplesize,
                                     ilab.xpos = c(-4),
                                     annotate = TRUE,
                                     xlab = "Hedges' g",
                                     slab = c("Brood parasite", "Building", "Cavity","Ground","Shrub","Tree"),
                                     cex = 2,
)

op <- par(cex=2, font=2)
text(-8, 7.2, "Location")
text(-4, 7.2, "Sample Size")
text(8.3, 7.2, "Hedges' g [95% CI]")
dev.off()


# first, if we want to add in any summary-level info create a new summary table
summary_table_nesting_locations <- data.frame(
  "Location" = c("Brood Parasite","Building","Cavity","Ground","Shrub","Tree"),
  N = c(2,1,25,28,37,38))
head(summary_table_nesting_locations)