## Load libraries ####
library(metafor)
library(tidyverse)
library(metaviz)

## Import and load data ####
## Load data ####
source("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/scripts/deer_ma/deer_source_data.R")
head(abundance_raw_data)

### Calculate effects size
# First calculate an effect size
effect_sizes_abundance <- escalc("SMD", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                 m1i = abundance_raw_data$mean_t,       
                                 n1i = abundance_raw_data$sample_size_t, # Then, follow with all of the columns needed to compute SMD
                                 sd1i = abundance_raw_data$SD_t, 
                                 m2i = abundance_raw_data$mean_c,
                                 n2i = abundance_raw_data$sample_size_c, 
                                 sd2i = abundance_raw_data$SD_c,
                                 data = abundance_raw_data)

effect_sizes_abundance # show the resulting data

# Meta-regression model
mixed_effect_abundance_results <- rma(yi, # outcome
                               vi, # measure of variance
                               mods = ~ Nesting_Location + IUCN_Trend - 1, # multiple moderating variables modeled as main effects
                               method = "REML",
                               data = effect_sizes_abundance,
                               slab = paste(author, pub_year, sep = ""))
mixed_effect_abundance_results

## Only for IUCN trend
mixed_effect_abundance_results_IUCN <- rma(yi, # outcome
                                      vi, # measure of variance
                                      mods = ~ IUCN_Trend - 1, # multiple moderating variables modeled as main effects
                                      method = "REML",
                                      data = effect_sizes_abundance,
                                      slab = paste(author, pub_year, sep = ""))
mixed_effect_abundance_results_IUCN

## Only for nesting location
mixed_effect_abundance_nesting <- rma(yi, # outcome
                                      vi, # measure of variance
                                      mods = ~ Nesting_Location - 1, # multiple moderating variables modeled as main effects
                                      method = "REML",
                                      data = effect_sizes_abundance,
                                      slab = paste(author, pub_year, sep = ""))
mixed_effect_abundance_nesting

# Using ONLY the mixed-effects model with nesting data
mixed_effect_abundance_nesting
me_forest_plot_nesting <- viz_forest(x = mixed_effect_abundance_nesting, 
           method = "REML",
           # group = "Nesting Location",
           type = "summary_only",
           summary_label = c("Brood Parasite", "Building","Cavity","Ground","Shrub","Tree"), 
           xlab = "Hedge's d",
           col = "Greys",
           variant = "thick",
           text_size = 7,
           annotate_CI = TRUE
           )
me_forest_plot_nesting

me_forest_plot_nesting$data
pdf(file="~/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/figures/forest_plot_abundance_and_nesting_location.pdf")
me_forest_plot_nesting
dev.off()
dev.off()

