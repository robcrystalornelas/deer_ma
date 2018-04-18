library(metafor)
library(tidyverse)

deer_raw_data <- read.csv("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/data_for_Crystal_Ornealas_et_al_deer_ABUNDANCE_v2.csv", header = TRUE)

# only rows 1-142 have real data
deer_data <- deer_raw_data[1:142,] %>%
  select(unique_id:notes)

head(deer_data)


# First calculate an effect size
effect_sizes <- escalc("SMD", 
       n1i = deer_data$sample_size_t, 
       n2i = deer_data$sample_size_c, 
       m1i = deer_data$mean_t, 
       m2i = deer_data$mean_c, 
       sd1i = deer_data$SD_t, 
       sd2i = deer_data$SD_c)

# then, run a random effects meta-analysis model
rma1 <- rma(yi = effect_sizes$yi, vi = effect_sizes$vi, methods = "REML")
# Here, i selected restricted Maximum liklihood random effects model

names(rma1) # these are all the different parts of an rma
# b is the summary effect
# ci.lb is left CI
# ci.up is the right bound of the CI
# vb is variance-covariance of summary
# fit.stats is model fit statistics
#yi is vector of effect sizes
#vi is vector of variances of effect size

# So this is summary effect
rma1$b

# these are lower and upper CI
rma1$ci.lb
rma1$ci.ub

# Which study contributed the most?
contributions <- 1/rma1$vi/sum(1/rma1$vi) * 100
contributions

# chck out summary info from MA
summary(rma1)

# Hows p-values from Q-test for heterogeneity?
rma1$QEp

# Making a forest plot
forest(rma1)

