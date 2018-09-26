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

# Calculating prediction intervals
predint <- function(x,pi) {
alpha <- (1-(pi*.01))/2
t <- abs(qt(alpha,(x$k-1)))
sdp <- sqrt(x$se^2+x$tau2) # pooled standard deviation
lo<-x$b-(sdp*t) # low PI
hi <- x$b+(sdp*t)  # high pi
paste(pi, "% prediction interval:", round(lo, digits =2), ",", round(hi,digits=2))  
}
predint(rma1, 95)

# figures ####
# Forest plot
forest(rma1, cex.lab = 1, cex.axis = 1, cex = 1)

## Figures ####
# Two types of funnel plots
funnel(rma1)
funnel(rma1, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0)

## Publication bias #### 
# Funnel plot
plot(effect_sizes_abundance$yi,effect_sizes_abundance$vi)
qplot(yi, vi, colour = author, 
      data = effect_sizes_abundance)

# Trim-and-fill
tf1 <- trimfill(rma1)
print(tf1, digits = 2, comb.fixed = TRUE)
