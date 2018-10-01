## Load libraries ####
library(metafor)
library(tidyverse)

## Load data ####
source("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/scripts/deer_ma/deer_source_data.R")

## Clean data ####
deer_richness <- richness_raw_data[1:10,] %>%
  select(unique_id:notes)

head(deer_richness)

## Analyze data ####

# First calculate effect size
richness_effect_sizes <- escalc("SMD", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                n1i = deer_richness$sample_size_t, # Then, follow with all of the columns needed to compute SMD
                                n2i = deer_richness$sample_size_c, 
                                m1i = deer_richness$mean_t, 
                                m2i = deer_richness$mean_c, 
                                sd1i = deer_richness$SD_t, 
                                sd2i = deer_richness$SD_c, data = deer_richness)
richness_effect_sizes # show the resulting data

# Then, calculate a mixed effects model
# Do some moderators find bigger or smaller effect?
names(richness_effect_sizes)
levels(richness_effect_sizes$bird.guild)
mixed_effects_richness <- rma(yi, 
                              vi, 
                              mods = ~ island_or_mainland,
                              method = "REML",
                              data = richness_effect_sizes,
                              slab = paste(author, pub_year, sep = ""))
mixed_effects_richness


