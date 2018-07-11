## Load libraries ####
library(metafor)
library(tidyverse)

## Load data ####
richness_raw_data <- read.csv("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/data_for_Crystal-Ornelas_et_al_deer_RICHNESS.csv", header = TRUE)
head(richness_raw_data)

## Clean data ####
deer_richness <- richness_raw_data[1:10,] %>%
  select(unique_id:notes)

head(deer_richness)

## Analyze data ####
# First calculate an effect size
richness_effect_sizes <- escalc("SMD", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                       n1i = deer_richness$sample_size_t, # Then, follow with all of the columns needed to compute SMD
                       n2i = deer_richness$sample_size_c, 
                       m1i = deer_richness$mean_t, 
                       m2i = deer_richness$mean_c, 
                       sd1i = deer_richness$SD_t, 
                       sd2i = deer_richness$SD_c)
richness_effect_sizes # show the resulting data

# Fixed effects model
richness_rma <- rma(yi, vi, method = "FE", data=richness_effect_sizes)
richness_rma
summary(richness_rma)

richness_rma_dl <- rma(yi, vi, method = "DL", data = richness_effect_sizes)
summary(richness_rma_dl)
richness_rma_reml <- rma(yi, vi, method = "REML", data = richness_effect_sizes)
summary(richness_rma_reml)
