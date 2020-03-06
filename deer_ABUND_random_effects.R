## Load Libraries ####
library(metafor)
library(tidyverse)
library(ggplot2)

## Load data ####
source(
  "~/Desktop/research/side_projects/Crystal-Ornelas_et_al_deer_meta/scripts/deer_ma/deer_source_data.R"
)

## Clean data ####

# Calculate effect sizes for each row of data
effect_sizes_abundance <-
  escalc(
    "SMD", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
    m1i = abundance_raw_data$mean_t,
    n1i = abundance_raw_data$sample_size_t, 
    # Follow with all of the columns needed to compute SMD
    sd1i = abundance_raw_data$SD_t,
    m2i = abundance_raw_data$mean_c,
    n2i = abundance_raw_data$sample_size_c,
    sd2i = abundance_raw_data$SD_c,
    data = abundance_raw_data
  )

# random effects model, assigning random effect to each row in database
random_effects_abundance_results <-
  rma(yi = effect_sizes_abundance$yi, # Outcome variable
      vi = effect_sizes_abundance$vi,# Variance
      method = "REML",
      weighted = TRUE) # REML is common estimator
random_effects_abundance_results

## Mixed effects meta-analytic model account for data coming from the same articles
mixed_effects_abundance <- rma.mv(yi, vi, random = ~ 1 | author, data = effect_sizes_abundance)
mixed_effects_abundance

# figures ####
# First, order by years
effect_sizes_abundance <- effect_sizes_abundance[order(effect_sizes_abundance$pub_year),]
effect_sizes_abundance$pub_year
plyr::count(effect_sizes_abundance$unique_id)
# First, get labels, so that we don't repeat farming systems
abundance_study_labels <- c(
  "DeGraaf, 1991",
  strrep("", 1:5),
  "Berger, 2001",
  strrep("", 1:11),
  "McShea, 2002",
  strrep("", 1:2),
  "Anderson, 2007",
  strrep("", 1:15),
  "Okuda, 2012",
  strrep("", 1:31),
  "Tymkiw, 2013",
  strrep("", 1:26),
  "Graham, 2014",
  strrep("", 1:33),
  "Carpio, 2015")
length(abundance_study_labels)

plyr::count(effect_sizes_abundance$author)
forest(
  effect_sizes_abundance$yi,
  effect_sizes_abundance$vi,
  annotate = FALSE,
  xlab = "Hedge's g",
  slab = abundance_study_labels,
  ylim = c(-1,138),
  cex = 1.3,
  pch = 15,
  cex.lab = 1.3,
  col = c(
    rep('#73D055FF', 6),
    rep('#cc6a70ff', 12),
    rep("#1F968BFF", 3),
    rep('#F66B4D', 16),
    rep('#481567FF', 32),
    rep('#f9b641ff', 27),
    rep('#404788FF', 34),
    rep ("#3CBB75FF", 1)))
addpoly(random_effects_abundance_results, row = 0 , cex = 1.5,col ="black", annotate = TRUE, mlab = "Summary")
dev.off()