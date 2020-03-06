library(metafor)
library(cowplot)

## Load data ####
source(
  "~/Desktop/research/side_projects/Crystal-Ornelas_et_al_deer_meta/scripts/deer_ma/deer_source_data.R"
)


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

# abundance histogram
abundance_hist <-ggplot(effect_sizes_abundance, aes(x=yi)) + 
  geom_histogram(binwidth=0.3,color="black", fill="white") +
  ylab("Frequency") +
  xlab("Hedges' g") +
  theme_cowplot() +
  theme(plot.title = element_text(size = 25, hjust = -.06, face = "bold"),
        axis.title = element_text(size = 25, face = "bold")) +
  scale_y_continuous(expand = c(0,0),limits=c(0,20))
abundance_hist

# richness histogram
richness_hist <-ggplot(richness_effect_sizes, aes(x=yi)) + 
  geom_histogram(binwidth=.2,color="black", fill="white") +
  ylab("Frequency") +
  xlab("Hedges' g") +
  theme_cowplot() +
  theme(plot.title = element_text(size = 25, hjust = -.06, face = "bold"),
        axis.title = element_text(size = 25, face = "bold")) +
  
  scale_y_continuous(expand = c(0,0),limits=c(0,2))
richness_hist

plot_grid(abundance_hist, richness_hist, labels = c('a', 'b'), label_size = 25)

