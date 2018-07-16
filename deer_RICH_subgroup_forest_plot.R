## Load libraries ####
library(metafor)
library(tidyverse)

## Import data ####
## Load data ####
richness_raw_data <- read.csv("/Users/rpecchia/Desktop/Deer Meta Analysis Brown J Beardsley C Ornealas R Lockwood J/data_for_Crystal-Ornelas_et_al_deer_RICHNESS.csv", header = TRUE)
head(richness_raw_data)

## Clean data ####
deer_richness <- richness_raw_data[1:10,] %>%
  select(unique_id:notes)

head(deer_richness)

### fit random-effects model (use slab argument to define study labels)

### Calculate effects sizes
richness_effect_sizes <- escalc("SMD", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                n1i = deer_richness$sample_size_t, # Then, follow with all of the columns needed to compute SMD
                                n2i = deer_richness$sample_size_c, 
                                m1i = deer_richness$mean_t, 
                                m2i = deer_richness$mean_c, 
                                sd1i = deer_richness$SD_t, 
                                sd2i = deer_richness$SD_c, data = deer_richness)
richness_effect_sizes # show the resulting data

mixed_effects_richness <- rma(yi, vi, mods = ~ pub_year + native_or_introduced + island_or_mainland + biome,
                              method = "REML",
                              data = richness_effect_sizes,
                              slab = paste(author, pub_year, sep = ", "))
forest(mixed_effects_richness)

### set up forest plot (with 2x2 table counts added; rows argument is used
### to specify exactly in which rows the outcomes will be plotted)
### decrease margins so the full space is used
par(mar=c(4,4,1,2))

op <- par(cex=0.75, font=1)
forest(mixed_effects_richness, 
       xlim=c(-8, 4), # What are horizontal limits of your plot (I got this from the original, non-moderated forest plot)
       at=c(-6, -4, -2, 0, 1),  # Where do x axis tick marks go?
#      atransf=exp, # use this if we're doing any transforming
#      ilab=cbind(dat.bcg$tpos, dat.bcg$tneg, dat.bcg$cpos, dat.bcg$cneg), # vector with more info to be added to plot
#      ilab.xpos=c(-9.5,-8,-6,-4.5), # x-axis position for the labels
       cex=1, 
       ylim=c(-1, 20), # what are limits of the plot in the y-axis
    order=order(richness_effect_sizes$native_or_introduced), 
       rows=c(3,7:1), #you have to decide where lines of text go from bottom of forest plot to top. Make sure all rows are captures in this function
       xlab="Standardized Mean Difference", 
       mlab="", 
       psize=1)


# Add labels to our subgroup graph
op <- par(cex=0.75, font=4)
text(-8, c(4,16), pos=4, c("Introduced",
                               "Native"))

### switch to bold font
par(font=2)
### add column headings to the plot
text(-8, 19, "Author and Year",  pos=4)
text(3, 19, "Hedge's D [95% CI]", pos=2)

### set par back to the original settings
par(op)