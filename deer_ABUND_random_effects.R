## Load Libraries ####
library(metafor)
library(tidyverse)
library(ggplot2)

## Load data ####
source("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/scripts/deer_ma/deer_source_data.R")

## Clean data ####
# First calculate an effect size
effect_sizes_abundance <- escalc("SMD", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                 m1i = abundance_raw_data$mean_t,       
                                 n1i = abundance_raw_data$sample_size_t, # Then, follow with all of the columns needed to compute SMD
                                 sd1i = abundance_raw_data$SD_t, 
                                 m2i = abundance_raw_data$mean_c,
                                 n2i = abundance_raw_data$sample_size_c, 
                                 sd2i = abundance_raw_data$SD_c,
                                 data = abundance_raw_data)

# Then take the effect sizes we calculated, and run a random effects meta-analysis model
rma1 <- rma(yi = effect_sizes_abundance$yi, # Outcome variable
            vi = effect_sizes_abundance$vi, # variances
            methods = "REML") # REML is common estimator
print(rma1, digits=5)

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

