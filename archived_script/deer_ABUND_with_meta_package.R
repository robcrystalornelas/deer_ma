## Load Libraries ####
library(meta)
library(tidyverse)

## Load data ####
deer_raw_data <- read.csv("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/data_for_Crystal_Ornealas_et_al_deer_ABUNDANCE_v3.csv", header = TRUE)
deer_raw_data

## Run meta-analysis
names(deer_raw_data)
m <- metacont(
  sample_size_t,
  mean_t,
  SD_t,
  sample_size_c,
  mean_c,
  SD_c,
  sm = "SMD",
  studlab=paste(author, pub_year),
  data = deer_raw_data
)

# Make a forest plot
forest(m, xlab = "standardized mean difference")

# Funnel plot
m$seTE
plot(m$TE, m$seTE, ylim = c(2, 0))
funnel(m)
radial(m)

# Trim and fill method
tf1 <- trimfill(m)
funnel(tf1)
