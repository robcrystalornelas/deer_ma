## Load libraries ####
library(tidyverse)

# Load data
richness_raw_data <-
  read.csv(
    "~/Desktop/side_projects/Crystal-Ornelas_et_al_deer_meta/data_for_Crystal-Ornelas_et_al_deer_RICHNESS_v2.csv",
    header = TRUE
  )

head(richness_raw_data)
names(richness_raw_data)
descriptives_richness <-
  select(
    richness_raw_data,
    unique_id,
    author,
    pub_year,
    study_year_begin,
    deer_common_name,
    native_or_nonnative,
    island_or_mainland,
    experiment_length_in_days,
    latitude,
    longitude,
    country,
    sample_size_c,
    sample_size_t
  )

abundance_raw_data <-
  read.csv("~/Desktop/side_projects/Crystal-Ornelas_et_al_deer_meta/data_for_Crystal_Ornealas_et_al_deer_ABUNDANCE_v6.csv",
           header = TRUE
  )
descriptives_abundance <-
  select(
    abundance_raw_data,
    unique_id,
    author,
    pub_year,
    study_year_begin,
    deer_common_name,
    native_or_nonnative,
    island_or_mainland,
    experiment_length_in_days,
    latitude,
    longitude,
    country,
    sample_size_c,
    sample_size_t
  )
dim(abundance_raw_data)
