## Load libraries ####
library(metafor)
library(metaviz)
library(cowplot)

## Load data ####
source(
  "~/Desktop/research/side_projects/Crystal-Ornelas_et_al_deer_meta/scripts/deer_ma/deer_source_data.R"
)

## Clean data ####
dim(richness_raw_data)
tail(richness_raw_data)

## Analyze data ####
# First calculate effect size
names(richness_raw_data)
richness_effect_sizes <-
  escalc(
    "SMD",
    # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
    n1i = richness_raw_data$sample_size_t,
    # Then, follow with all of the columns needed to compute SMD
    n2i = richness_raw_data$sample_size_c,
    m1i = richness_raw_data$mean_t,
    m2i = richness_raw_data$mean_c,
    sd1i = richness_raw_data$SD_t,
    sd2i = richness_raw_data$SD_c,
    data = richness_raw_data
  )
richness_effect_sizes # show the resulting data

# Random effects model
richness_rma_dl <-
  rma(
    yi,
    vi,
    method = "REML",
    data = richness_effect_sizes
  )
summary(richness_rma_dl)

## Make forest plots ####
richness_effect_sizes <- richness_effect_sizes[order(richness_effect_sizes$pub_year),]
richness_effect_sizes$pub_year
plyr::count(richness_effect_sizes$unique_id)
# First, get labels, so that we don't repeat farming systems
richness_study_labels <- c(
  "DeGraaf, 1991",
  "McShea, 1992",
  "DeCalesta, 1994a",
  "DeCalesta, 1994b",
  "DeCalesta, 1994c",
  "Anderson, 2007",
  "Okuda, 2012",
  "Tymkiw, 2013",
  "Graham, 2014",
  "Chollet, 2014")
length(richness_study_labels)

plyr::count(richness_effect_sizes$author)
forest(
  richness_effect_sizes$yi,
  richness_effect_sizes$vi,
  annotate = FALSE,
  xlab = "Hedge's g",
  slab = richness_study_labels,
  #ylim = c(-1,10),
  cex = 1.3,
  pch = 15,
  cex.lab = 1.3,
  col = c(
    rep('#73D055FF', 1),
    rep('#cc6a70ff', 1),
    rep("#1F968BFF", 1),
    rep("#1F968BFF", 1),
    rep("#1F968BFF", 1),
    rep('#F66B4D', 1),
    rep('#481567FF', 1),
    rep('#f9b641ff', 1),
    rep('#404788FF', 1),
    rep ("#3CBB75FF", 1)))
addpoly(richness_rma_dl, row = .5 , cex = 1.3,col ="black", annotate = TRUE, mlab = "Summary")
dev.off()
