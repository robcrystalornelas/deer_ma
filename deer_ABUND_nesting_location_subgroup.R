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

# Subset only nesting locations greater than 7
plyr::count(abundance_raw_data$Nesting_Location)
nesting_locations_with_more_than_seven <- effect_sizes_abundance %>% filter(Nesting_Location == "cavity" | Nesting_Location == "ground" | Nesting_Location == "shrub" | Nesting_Location == "tree")
plyr::count(nesting_locations_with_more_than_seven$Nesting_Location)

## Only for nesting location
mixed_effect_abundance_nesting <- rma.mv(
  yi,  # outcome
  vi,  # measure of variance
  mods = ~ Nesting_Location - 1, # multiple moderating variables modeled as main effects
  random = ~ 1 | unique_id,
  method = "REML",
  digits = 4,
  data = nesting_locations_with_more_than_seven
)
mixed_effect_abundance_nesting

# Forest plot nesting data
subgroup_samplesize <- c(44,40,45,58)
forest_plot_nesting_location <- forest(mixed_effect_abundance_nesting$b,
                                     ci.lb = mixed_effect_abundance_nesting$ci.lb,
                                     ci.ub = mixed_effect_abundance_nesting$ci.ub,
                                     ilab = subgroup_samplesize,
                                     ilab.xpos = c(-1.2),
                                     annotate = TRUE,
                                     xlab = "Hedges' g",
                                     slab = c("Cavity", "Ground", "Shrub","Tree"),
                                     cex = 2)

op <- par(cex=2, font=2)
text(-1.75, 5.2, "Location")
text(-1.2, 5.2, "Sample Size")
text(.9, 5.2, "Hedges' g [95% CI]")
dev.off()


# first, if we want to add in any summary-level info create a new summary table