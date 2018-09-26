## Load libraries ####
library(metafor)
library(tidyverse)

## Load data ####
source("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/scripts/deer_ma/deer_source_data.R")

# subset only studies that we include in the meta-analysis
all_studies_combined <- rbind(descriptives_richness,descriptives_abundance)
dim(all_studies_combined)
unique_studies <- unique(all_studies_combined)
unique_studies
unique_studies <- slice(unique_studies, c(1,2,3,4,5,6,7,8,10,12,14))
unique_studies

# Average study length
mean(unique_studies$experiment_length_in_days, na.rm = TRUE)/365

# Longest study
max(unique_studies$experiment_length_in_days, na.rm = TRUE)/365
               
# Shortest study         
min(unique_studies$experiment_length_in_days, na.rm = TRUE)

# Geographic location
levels(unique_studies$country)
plot(unique_studies$country)

# Island vs. mainland
plot(unique_studies$island_or_mainland)

# native or invasive
plot(unique_studies$native_or_nonnative)
unique_studies$native_or_nonnative
