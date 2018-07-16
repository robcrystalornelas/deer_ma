## Load Libraries ####
library(metafor)
library(tidyverse)

## Load data ####
deer_raw_data <- read.csv("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/data_for_Crystal_Ornealas_et_al_deer_ABUNDANCE_v3.csv", header = TRUE)
deer_raw_data

## Clean data ####
# only rows 1-141 have real data
head(deer_raw_data)
deer_data <- deer_raw_data[1:141,] %>%
  select(unique_id:notes)

# First calculate an effect size
effect_sizes_abundance <- escalc("SMD", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                 n1i = deer_data$sample_size_t, # Then, follow with all of the columns needed to compute SMD
                                 n2i = deer_data$sample_size_c, 
                                 m1i = deer_data$mean_t, 
                                 m2i = deer_data$mean_c, 
                                 sd1i = deer_data$SD_t, 
                                 sd2i = deer_data$SD_c,
                                 data = deer_data)
effect_sizes_abundance # show the resulting data

## Mixed effects model ####
unique(effect_sizes_abundance$bird_family)

mixed_effects_abund_family <- rma(yi, vi, mods = ~ bird_family,
                              method = "REML",
                              data = effect_sizes_abundance,
                              slab = paste(author, pub_year, sep = ", "))
forest(mixed_effects_richness)
