## Load libraries ####
library(tidyverse)
library(mice)

# Load data
richness_raw_data <-
  read.csv(
    "~/Desktop/research/side_projects/Crystal-Ornelas_et_al_deer_meta/data_for_Crystal-Ornelas_et_al_deer_RICHNESS_v2.csv",
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

### Import abundance data
abundance_raw_data <-
  read.csv("~/Desktop/research/side_projects/Crystal-Ornelas_et_al_deer_meta/data_for_Crystal_Ornealas_et_al_deer_ABUNDANCE_v6.csv",
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
head(abundance_raw_data)
set.seed(111)

# Get just the columns we need
abundance_raw_data_with_nas <- dplyr::select(abundance_raw_data, 
                                             unique_id,
                                             mean_c, 
                                             SD_c, 
                                             sample_size_c,
                                             mean_t,
                                             SD_t,
                                             sample_size_t)

# Imputation for abundance data ####
abundance_raw_data_with_nas <- abundance_raw_data_with_nas %>%
  mutate(
    study_code = as.factor(unique_id),
                           mean_c = as.numeric(mean_c),
    SD_c = as.numeric(SD_c),
    sample_size_c = as.numeric(sample_size_c),
    mean_t = as.numeric(mean_t),
    SD_t = as.numeric(SD_t),
    sample_size_t = as.numeric(sample_size_t)
  )

# Here's some boiler plate code for starting the imputation
init <- mice(abundance_raw_data_with_nas, maxit = 0) 
init
meth <- init$method
meth
predM <- init$predictorMatrix
predM

# We can remove some meaningless variables as predictors, for example study code
predM
predM[, c("unique_id")] = 0

# Skip any variables for imputation, this variable will be used for prediction
meth[c("mean_c","mean_t","sample_size_c","sample_size_t")]=""

# What are we going to impute, gotta pick different methods for ordinal, factors or continuous
meth[c("SD_c","SD_t")]="norm" 

# Now it's time to run the multiple imputation
imputed_sds_abundance <- mice(abundance_raw_data_with_nas, method="pmm", predictorMatrix=predM, m=5, seed = 100)
# impute data with probable means
class(imputed_sds_abundance)

# Create a dataset after the imputation
imputed_sds_abundance <- mice::complete(imputed_sds_abundance)
imputed_sds_abundance

# Do we still have missing values? Hopefully not!
sapply(imputed_sds_abundance, function(x) sum(is.na(x)))

# Now that we have imputed values, replace whole column in R
abundance_raw_data$SD_c <- imputed_sds_abundance$SD_c
abundance_raw_data$SD_t <- imputed_sds_abundance$SD_t

