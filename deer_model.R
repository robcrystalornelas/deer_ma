library(meta)
library(tidyverse)

# Load in data
deer_raw_data <- read.csv("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/data_for_Crystal_Ornealas_et_al_deer_ABUNDANCE_v2.csv", header = TRUE)

# only rows 1-142 have real data
deer_data <- deer_raw_data[1:142,] %>%
  select(unique_id:notes)
head(deer_data)

# make sure certain columns don't have NAs
deer_data_removed_na <- deer_data[!is.na(deer_data$mean_c),]
deer_data_removed_na <- deer_data[!is.na(deer_data$mean_t),]
deer_data_removed_na <- deer_data[!is.na(deer_data$SD_c),]
deer_data_removed_na <- deer_data[!is.na(deer_data$SD_t),]
deer_data_removed_na <- deer_data[!is.na(deer_data$sample_size_c),]
deer_data_removed_na <- deer_data[!is.na(deer_data$sample_size_t),]

# make sure author and year are character
deer_data_removed_na$author <- as.character(deer_data_removed_na$author)
deer_data_removed_na$pub_year <- as.character(deer_data_removed_na$pub_year)

# Do a meta-analysis using standardized mean difference as effect size
m1 <- metacont(sample_size_t, mean_t, SD_t, sample_size_c, mean_c, SD_c, sm = "SMD",
               data = deer_data_removed_na, studlab = paste(author, pub_year))
m1

m1$TE.random # this is treatment estimate in random effects model
m1$seTE.random^2 # this is the standard error from therandom effects model.
# so here we have the SDM and its variance, we use random effects model!
# Run simple meta-analysis
# Ne = sample size patient
# Me = mean experimental
#Se is SD of expeirmental
# also, Nc, Mc, Sc
# author
# year

# Creating a forest plot
forest(m1, comb.random = FALSE, xlab = 
         "Difference in mean response (invaded - noninvaded")

# Since the test of between study heterogeneity is signficant, 
# we might consider a subgroup analysis
