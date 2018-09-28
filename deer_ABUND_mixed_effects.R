## Load libraries ####
library(metafor)

## Load data ####
source("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/scripts/deer_ma/deer_source_data.R")

## Run analysis ####
# First calculate an effect size
abundance_effect_sizes <- escalc("SMD", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                       n1i = abundance_raw_data$sample_size_t, # Then, follow with all of the columns needed to compute SMD
                       n2i = abundance_raw_data$sample_size_c, 
                       m1i = abundance_raw_data$mean_t, 
                       m2i = abundance_raw_data$mean_c, 
                       sd1i = abundance_raw_data$SD_t, 
                       sd2i = abundance_raw_data$SD_c,
                       data = abundance_raw_data)

# Mixed effects model w/ moderators
names(abundance_effect_sizes)
mixed_effect_abunda_res <- rma(yi, # outcome
           vi, # measure of variance
           mods = ~ Nesting_Location + Diet + Habitat_Type + IUCN_Trend + Migrant, # multiple moderating variables modeled as main effects
           method = "REML",
           data = abundance_effect_sizes,
           slab = paste(author, pub_year, sep = ""))
mixed_effect_abunda_res
## Diet + Habitat_Type + IUCN_Trend + Migrant
