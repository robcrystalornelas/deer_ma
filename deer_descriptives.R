## Load libraries ####
library(metafor)
library(tidyverse)

## Load data ####
source("/Users/rpecchia/Desktop/side_projects/Crystal-Ornelas_et_al_deer_meta/scripts/deer_ma/deer_source_data.R")

# subset only studies that we include in the meta-analysis
descriptives_abundance
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

# number of bird species
unique(abundance_raw_data$bird_species_latin)
unique(abundance_raw_data$bird_family)

# average number of species per manuscript
head(abundance_raw_data)
abundance_raw_data %>%
  group_by('unique_id') %>%
  count(bird_species_latin)

abundance_raw_data

# Number of sites we have data from abundance
unique_sites_and_code <- select(descriptives_abundance, sample_size_t, sample_size_c, unique_id) %>%
  distinct()
unique_sites_and_code
# 126 treatment sites, 228 control

# Number of sites we have from richness
unique_sites_and_code_richness <- select(descriptives_richness, sample_size_t, sample_size_c, unique_id) %>%
  distinct()
unique_sites_and_code_richness
# 491 for treatment and 489 for control
counted_species <- abundance_raw_data %>% group_by(unique_id, bird_species_latin) %>% summarize(count=n())
counted_species

