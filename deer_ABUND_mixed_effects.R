## Load libraries ####
library(metafor)
library(tidyverse)

## Load data ####
source("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/scripts/deer_ma/deer_source_data.R")

## Clean data ####
# only rows 1-141 have data
head(abundance_raw_data)
tail(abundance_raw_data, n = 10L)
dim(abundance_raw_data)
abundance_raw_data <- abundance_raw_data[1:133,] %>%
  select(unique_id:notes)
head(abundance_raw_data)
tail(abundance_raw_data)

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
dim(abundance_effect_sizes)

# Try a mixed effects model w/ moderators
names(abundance_effect_sizes)
res <- rma(yi, # outcome
           vi, # measure of variance
           mods = ~ Nesting_Location + Habitat_Type + Diet, # multiple moderating variables modeled as main effects
           method = "REML",
           data = abundance_effect_sizes,
           slab = paste(author, pub_year, sep = ""))
res

