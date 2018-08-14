## Load Libraries ####
library(metafor)
library(tidyverse)
library(ggplot2)

## Load data ####
source("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/scripts/deer_ma/deer_source_data.R")

## Clean data ####
unique(abundance_raw_data$unique_id)

# First calculate an effect sizes
effect_sizes_abundance <- escalc("SMD", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
  m1i = abundance_raw_data$mean_t,       
  n1i = abundance_raw_data$sample_size_t, # Then, follow with all of the columns needed to compute SMD
  sd1i = abundance_raw_data$SD_t, 
  m2i = abundance_raw_data$mean_c,
  n2i = abundance_raw_data$sample_size_c, 
  sd2i = abundance_raw_data$SD_c,
  data = abundance_raw_data)

effect_sizes_abundance # show the resulting data

# Fixed effects MA model
fma_abundance <- rma(yi = effect_sizes_abundance$yi, # Outcome variable
                     vi = effect_sizes_abundance$vi, # variances
                     method = "FE")
summary(fma_abundance)
