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
effect_sizes_abundance$ID <- seq.int(nrow(effect_sizes_abundance))
random_effects_abundance_results <-
  rma(yi = effect_sizes_abundance$yi, # Outcome variable
      vi = effect_sizes_abundance$vi,# Variance
      method = "REML",
      weighted = TRUE) # REML is common estimator
random_effects_abundance_results

re_with_row_numbers <-   rma.mv(yi, vi, random = ~ 1 |
                                  ID, data = effect_sizes_abundance)
re_with_row_numbers

## Mixed effects meta-analytic model account for data coming from the same articles
mixed_effects_abundance <-
  rma.mv(yi, vi, random = ~ 1 |
           author, data = effect_sizes_abundance)
mixed_effects_abundance

# figures ####
# First, order by years
effect_sizes_abundance <- effect_sizes_abundance[order(effect_sizes_abundance$pub_year),]
View(effect_sizes_abundance)
effect_sizes_abundance$pub_year
plyr::count(effect_sizes_abundance$unique_id)
# First, get labels, so that we don't repeat farming systems
abundance_study_labels <- c(
  "DeGraaf, 1991",
  strrep("", 1:5),
  "McShea, 2000",
  strrep("", 1:2),
  "Berger, 2001",
  strrep("", 1:11),
  "Anderson, 2007",
  strrep("", 1:15),
  "Martin, 2008",
  strrep("", 1:12),
  "Martin, 2011",
  strrep("", 1:29),
  "Okuda, 2012",
  strrep("", 1:31),
  "Cardinal, 2012",
  "Tymkiw, 2013",
  strrep("", 1:26),
  "Graham, 2014",
  strrep("", 1:33),
  "Carpio, 2015",
  "Chollet, 2016",
  strrep("",1:16))
length(abundance_study_labels)

plyr::count(effect_sizes_abundance$author)
forest(
  effect_sizes_abundance$yi,
  effect_sizes_abundance$vi,
  annotate = FALSE,
  xlab = "Hedge's g",
  slab = abundance_study_labels,
  ylim = c(-1,200),
  cex = 1.3,
  pch = 15,
  cex.lab = 1.3,
   col = c(
     rep('#a6cee3', 6),
     rep('#1f78b4', 3),
     rep('#cc6a70ff', 12),
     rep("#b2df8a", 16),
     rep('#33a02c', 13),
     rep('#fb9a99', 30),
     rep('#f9b641ff', 32),
     rep('#e31a1c', 1),
     rep ("#b15928", 27),
     rep ("#ff7f00", 34),
     rep ("#cab2d6", 1),
     rep ("#6a3d9a", 17)))
addpoly(mixed_effects_abundance, row = -4 , cex = 1.3,col ="#eb8055ff", annotate = TRUE, mlab = "Summary")
dev.off()
