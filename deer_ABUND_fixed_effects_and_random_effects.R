## Load Libraries ####
library(metafor)
library(tidyverse)

## Load data ####
deer_raw_data <- read.csv("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/data_for_Crystal_Ornealas_et_al_deer_ABUNDANCE_v3.csv", header = TRUE)
deer_raw_data

# only rows 1-141 have real data
head(deer_raw_data)
deer_data <- deer_raw_data[1:141,] %>%
  select(unique_id:notes)

# First calculate an effect size
effect_sizes_abundance <- escalc("SMD", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
       n1i = deer_data$sample_size_t, # Then, follow with all of the columns needed to compute SMD
       n2i = deer_data$sample_size_c, 
       m1i = deer_data$mean_t, 
       m2i = deer_data$mean_c, 
       sd1i = deer_data$SD_t, 
       sd2i = deer_data$SD_c)
effect_sizes_abundance # show the resulting data
effect_sizes_abundance <- na.omit(effect_sizes_abundance)
effect_sizes_abundance

# First, try fitting with a fixed effects model
fma_abundance <- rma(yi = effect_sizes_abundance$yi, # Outcome variable
                     vi = effect_sizes_abundance$vi, # variances
                     method = "FE")

# Then take the effect sizes we calculated, and run a random effects meta-analysis model
rma1 <- rma(yi = effect_sizes_abundance$yi, # Outcome variable
            vi = effect_sizes_abundance$vi, # variances
            methods = "REML") # REML is common estimator
print(rma1, digits=2)

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

# Making a forest plot with the random effects model
forest(rma1, cex.lab = 1, cex.axis = 1, cex = 1)

# funnel plot
# first, carry out trim and fill
par(mar=c(5,4,1,2))
taf_rma <- trimfill(rma1)
taf_rma

# draw funnel plot with missing studies filled in
funnel(rma1)
funnel(rma1, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0)
       