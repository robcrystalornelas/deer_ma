## Load libraries ####
library(metafor)
library(tidyverse)

## Load data ####
richness_raw_data <- read.csv("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/data_for_Crystal-Ornelas_et_al_deer_RICHNESS.csv", header = TRUE)
head(richness_raw_data)

## Clean data ####
deer_richness <- richness_raw_data[1:10,] %>%
  select(unique_id:notes)

head(deer_richness)

## Analyze data ####

# First calculate effect size
richness_effect_sizes <- escalc("SMD", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                       n1i = deer_richness$sample_size_t, # Then, follow with all of the columns needed to compute SMD
                       n2i = deer_richness$sample_size_c, 
                       m1i = deer_richness$mean_t, 
                       m2i = deer_richness$mean_c, 
                       sd1i = deer_richness$SD_t, 
                       sd2i = deer_richness$SD_c, data = deer_richness)
richness_effect_sizes # show the resulting data

# Combine effect size df and df with author and year
names(deer_richness)
richness_effect_sizes$author <- deer_richness$author
richness_effect_sizes$pub_year <- deer_richness$pub_year

# First, use a fixed effects model
richness_rma <- rma(yi, vi, method = "FE", data=richness_effect_sizes)
richness_rma
summary(richness_rma)

# Then use a random effects model
richness_rma_dl <- rma(yi, vi, method = "DL", data = richness_effect_sizes, slab = paste(author, pub_year))
summary(richness_rma_dl)

# Prediction intervals are they way to go instead of confidence intervals
predint <- function(x, pi){
pi <- 95
alpha <- (1-(pi*.01))/2
t <- abs(qt(alpha,(x$k-1)))
sdp <- sqrt(x$se^2+x$tau2)
lo <- x$b - (sdp*t)
hi <- x$b + (sdp*t)
paste(pi, "% prediction interval:", round(lo, digits = 2), round(hi, digits = 2))
}

predint(richness_rma_dl, 95)
# can totally include 0! and neg!
# What this means is that studies can find opposite effect of overall
# mean effect or even most of the studies included in our MA.
# Lots of possible reasons why we might get opposite results
# doesn't mean our conclusion is wrong, just means they're possible!

## Make forest plots ####
# Forest plots require that we do any labeling in the function where we run the analysis
forest(richness_rma_dl)
?forest
## Make funnel plot ####
# Controversial, especially if you've done an exhaustive literature search
funnel(richness_rma_dl)
