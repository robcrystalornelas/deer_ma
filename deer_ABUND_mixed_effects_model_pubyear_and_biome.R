## Load libraries ####
library(metafor)
library(tidyverse)

## Load data ####
deer_raw_data <- read.csv("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/data_for_Crystal_Ornealas_et_al_deer_ABUNDANCE_v3.csv", header = TRUE)
deer_raw_data

## Clean data ####
# only rows 1-141 have data
head(deer_raw_data)
deer_data <- deer_raw_data[1:141,] %>%
  select(unique_id:notes)

head(deer_data)

## Run analysis ####

# First calculate an effect size
effect_sizes <- escalc("SMD", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                       n1i = deer_data$sample_size_t, # Then, follow with all of the columns needed to compute SMD
                       n2i = deer_data$sample_size_c, 
                       m1i = deer_data$mean_t, 
                       m2i = deer_data$mean_c, 
                       sd1i = deer_data$SD_t, 
                       sd2i = deer_data$SD_c)
effect_sizes # show the resulting data


# Try a mixed effects model w/ PUBLICATION YEAR and BIOME as moderator
res <- rma(effect_sizes$yi, # outcome
           effect_sizes$vi, # measure of variance
           mods = ~ pub_year + biome # multiple moderating variables modeled as main effects
           , data = deer_data)
res
