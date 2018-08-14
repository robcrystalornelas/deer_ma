## Load libraries ####
library(tidyverse)

# Load data
richness_raw_data <- read.csv("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/data_for_Crystal-Ornelas_et_al_deer_RICHNESS.csv", header = TRUE)
head(richness_raw_data)
names(richness_raw_data)
descriptives_richness <- select(richness_raw_data, unique_id, author, pub_year, study_year_begin,deer_common_name,native_or_introduced,island_or_mainland,experiment_length_in_days,latitude,longitude,country)

abundance_raw_data <- read.csv("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/data_for_Crystal_Ornealas_et_al_deer_ABUNDANCE_v4.csv", header = TRUE)
descriptives_abundance <- select(abundance_raw_data, unique_id, author, pub_year, study_year_begin,deer_common_name,native_or_introduced,island_or_mainland,experiment_length_in_days,latitude,longitude,country)
dim(abundance_raw_data)
