## Load libraries ####
library(metafor)
library(metaviz)

## Load data ####
source("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/scripts/deer_ma/deer_source_data.R")

## Clean data ####
dim(richness_raw_data)
tail(richness_raw_data)

## Analyze data ####
# First calculate effect size
names(richness_raw_data)
richness_effect_sizes <- escalc("SMD", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                n1i = richness_raw_data$sample_size_t, # Then, follow with all of the columns needed to compute SMD
                                n2i = richness_raw_data$sample_size_c, 
                                m1i = richness_raw_data$mean_t, 
                                m2i = richness_raw_data$mean_c, 
                                sd1i = richness_raw_data$SD_t, 
                                sd2i = richness_raw_data$SD_c, data = richness_raw_data)
richness_effect_sizes # show the resulting data

# Random effects model
richness_rma_dl <- rma(yi, vi, method = "REML", data = richness_effect_sizes, slab = paste(author, pub_year))
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
richness_rma_dl$level
richness_rma_dl$slab
richness_rma_dl[1:12,]

richness_forest_plot_random_effects <- viz_forest(x = richness_rma_dl,
           method = "REML",
           study_labels = richness_rma_dl$slab, # In order to label your y-axis
           xlab = "Hedge's d",
           col = "Greys",
           variant = "thick",
           text_size = 6,
           annotate_CI = TRUE
)
richness_forest_plot_random_effects

pdf(file="~/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/figures/forest_plot_richness_full.pdf")
richness_forest_plot_random_effects
dev.off()
dev.off()

## publication bias ####
# Controversial, especially if you've done an exhaustive literature search
funnel(richness_rma_dl)

tf1 <- trimfill(richness_rma_dl)
print(tf1, digits = 2, comb.fixed = TRUE)

