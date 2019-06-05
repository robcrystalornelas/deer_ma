## Load libraries ####
library(metafor)
library(metaviz)

## Load data ####
source("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/scripts/deer_ma/deer_source_data.R")

## Clean data ####
dim(richness_raw_data)
tail(richness_raw_data)

## Analyze data ####
# First calculate effect size
richness_effect_sizes <- escalc("SMD", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                       n1i = richness_raw_data$sample_size_t, # Then, follow with all of the columns needed to compute SMD
                       n2i = richness_raw_data$sample_size_c, 
                       m1i = richness_raw_data$mean_t, 
                       m2i = richness_raw_data$mean_c, 
                       sd1i = richness_raw_data$SD_t, 
                       sd2i = richness_raw_data$SD_c, data = richness_raw_data)
richness_effect_sizes # show the resulting data

# Combine effect size df and df with author and year
names(richness_effect_sizes)
richness_effect_sizes$author <- richness_effect_sizes$author
richness_effect_sizes$pub_year <- richness_effect_sizes$pub_year
richness_effect_sizes$unique_id

# First, use a fixed effects model
richness_rma <- rma(yi, vi, method = "FE", data=richness_effect_sizes)
richness_rma
summary(richness_rma)


