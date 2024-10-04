# ## exploring DART OUTPUTS
#install.packages('CasualImpact')
library(tidyterra)


## load in the DART outputs from preliminary polygons from Gayle 
# bring in results for one polygon
load("C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\Restorationsuccess\\Restorationsuccess\\outdir\\ID_1033.RData")

### polygons considered 
head(padpoly) # the polygon considered 
plot(padpoly)

head(padpixels) # extracted environmental data for the pixels in the treated area
# also identifies which pixels were considered for sampling? 
# Gayle ran this with sunsampling = T so number of pixels in treated area is reduces by factor of 4 
##!!!!!!! we might want to change this for the smaller treatemnt areas



## candidate and chosen cadidate pixels from the untreated area 
head(candidates)
head(chosenCandidates)
#plot(chosenCandidates) 

## Dart parameters
print(dpar) # list of all the parameters users for inputs and saving


## plot the pad poly and pad pixels to figure out if pixels are in the treatment or untrt area
pixel_map <- ggplot()+
  geom_spatvector(data = terra::vect(padpoly), fill = 'red')+
  geom_spatvector(data = terra::vect(padpixels),  fill ='black')+
  theme_void()
pixel_map

# # seems like the padpixels are the treated pixels considered for matching 
# plot(pixel_map) # seems to be a ggplot theme  for map

## add on the possible and chosen candidates 
pixel_map <- ggplot()+
  geom_spatvector(data = terra::vect(padpoly), fill = 'red')+
  geom_spatvector(data = terra::vect(padpixels),  fill ='black')+
  geom_spatvector(data = terra::vect(candidates),  fill ='grey')+
  geom_spatvector(data = terra::vect(chosenCandidates),  fill ='orange')+
  theme_void()
pixel_map




########## check on the extracted data 
head(extraction) # extraction is all the RAP data for each functional group and year -
# this data includes three datasets - refIndex, extractedTarget,  extractedReference, and ExtractedTimeElapsed
summary(as.factor(extraction$extractedReference$ID))
# extractedTarget includes 4 unique values in the ID col - this is in the treated area
# extractedReference contains 3000+ unique ID cols - these are for the chosen candiadtes in the untreated area
# I seems like these are organized by row name (index) from the original candidate dataset
head(extraction$extractedReference) 
## this is not a spatial dataset yet - we would need to use x and y locations in the df, or merge with the candidates spatial data frame 


#################################
## view the outputs of the synth control modeling 
# two componentes - model and data

synthControl$models # model outputs - actual, mean and 95% CI ? 
# this comes from the summary() function in Casual IMpact 

head(synthControl$data) # this is the $series result after running causal impact 
# the data includes point level and cumulative effects for each functional group 
# these are built off the selected treated and untreated pixels 

# in this case I think that the row name is a year index 
# right now, I think DART is set to pull only 10 years after -
# this checks out with the extracted rap data, which only goes until 2015 (which is 10 YAT)
# however all of the pre-treatment RAP years are included

# grab one functional group
afg_syncontrol= synthControl$data$annual_forb_and_grass_cover$effect
head(afg_syncontrol)

summary(afg_syncontrol$cum.effect)

### the $data$functional group $ effect is the result from $series 
## this includes point wise and cumulative effects
# does not include the original data - since those are in the extract datasets

## plot the point level effects
afg_point_effects_plot <- ggplot(data = afg_syncontrol, aes(x = seq_along(point.effect), y = point.effect)) +
  geom_line(linewidth = 2) +  # Connect the dots with a line
  geom_point() +  # Add points
  geom_ribbon(aes(ymin = point.effect.lower, ymax = point.effect.upper), alpha = 0.2) +  # Add confidence interval ribbon
  labs(x = "year (?)", y = "Point Effect") +  # Optional: Add axis labels
  geom_hline(yintercept = 0, linetype = 'dashed')+
  geom_vline(xintercept = 10, linetype = 'dashed')+
  theme_minimal()  # Optional: Use a minimal theme
plot(afg_point_effects_plot)

# plot the sumulative effects
afg_cum_effects_plot <- ggplot(data = afg_syncontrol, aes(x = seq_along(cum.effect), y = cum.effect)) +
  geom_line(linewidth = 2) +  # Connect the dots with a line
  geom_point() +  # Add points
  geom_ribbon(aes(ymin = cum.effect.lower, ymax = cum.effect.upper), alpha = 0.2) +  # Add confidence interval ribbon
  labs(x = "year (?)", y = "Cumulative Effect") +  # Optional: Add axis labels
  geom_hline(yintercept = 0, linetype = 'dashed')+
  geom_vline(xintercept = 10, linetype = 'dashed')+
  theme_minimal()  # Optional: Use a minimal theme
plot(afg_cum_effects_plot)


head(padpoly) # trt year is 2005 


# not sure if the treatment year is year index 10 or 11...
# we should ask gayle about it, and include a function to translate year index into actual year or years since treatment
# we also might want more years if available



### do this for all functional groups 

# start with point level effects
afg_point_effects_plot <- ggplot(data = synthControl$data$annual_forb_and_grass_cover$effect, 
                                 aes(x = seq_along(point.effect), y = point.effect)) +
  geom_hline(yintercept = 0, linewidth = 2, color = 'grey')+
  geom_vline(xintercept = 10, linewidth = 1.5, linetype = 'dashed')+
  geom_line(linewidth = 2) +  
  geom_ribbon(aes(ymin = point.effect.lower, ymax = point.effect.upper), alpha = 0.2) +  # Add confidence interval ribbon
  labs(x = "year (?)", y = "Point Effect") +   theme_classic()  
plot(afg_point_effects_plot)

pfg_point_effects_plot <- ggplot(data = synthControl$data$perennial_forb_and_grass_cover$effect, 
                                 aes(x = seq_along(point.effect), y = point.effect)) +
  geom_hline(yintercept = 0, linewidth = 2, color = 'grey')+
  geom_vline(xintercept = 10, linewidth = 1.5, linetype = 'dashed')+
  geom_line(linewidth = 2) +  
  geom_ribbon(aes(ymin = point.effect.lower, ymax = point.effect.upper), alpha = 0.2) +  # Add confidence interval ribbon
  labs(x = "year (?)", y = "Point Effect") +   theme_classic()  
shr_point_effects_plot <- ggplot(data = synthControl$data$shrub_cover$effect, 
                                 aes(x = seq_along(point.effect), y = point.effect)) +
  geom_hline(yintercept = 0, linewidth = 2, color = 'grey')+
  geom_vline(xintercept = 10, linewidth = 1.5, linetype = 'dashed')+
  geom_line(linewidth = 2) +  
  geom_ribbon(aes(ymin = point.effect.lower, ymax = point.effect.upper), alpha = 0.2) +  # Add confidence interval ribbon
  labs(x = "year (?)", y = "Point Effect") +   theme_classic()  
tre_point_effects_plot <- ggplot(data = synthControl$data$tree_cover$effect, 
                                 aes(x = seq_along(point.effect), y = point.effect)) +
  geom_hline(yintercept = 0, linewidth = 2, color = 'grey')+
  geom_vline(xintercept = 10, linewidth = 1.5, linetype = 'dashed')+
  geom_line(linewidth = 2) +  
  geom_ribbon(aes(ymin = point.effect.lower, ymax = point.effect.upper), alpha = 0.2) +  # Add confidence interval ribbon
  labs(x = "year (?)", y = "Point Effect") +   theme_classic()  
ltr_point_effects_plot <- ggplot(data = synthControl$data$litter_cover$effect, 
                                 aes(x = seq_along(point.effect), y = point.effect)) +
  geom_hline(yintercept = 0, linewidth = 2, color = 'grey')+
  geom_vline(xintercept = 10, linewidth = 1.5, linetype = 'dashed')+
  geom_line(linewidth = 2) +  
  geom_ribbon(aes(ymin = point.effect.lower, ymax = point.effect.upper), alpha = 0.2) +  # Add confidence interval ribbon
  labs(x = "year (?)", y = "Point Effect") +   theme_classic()  
bgr_point_effects_plot <- ggplot(data = synthControl$data$bare_ground_cover$effect, 
                                 aes(x = seq_along(point.effect), y = point.effect)) +
  geom_hline(yintercept = 0, linewidth = 2, color = 'grey')+
  geom_vline(xintercept = 10, linewidth = 1.5, linetype = 'dashed')+
  geom_line(linewidth = 2) +  
  geom_ribbon(aes(ymin = point.effect.lower, ymax = point.effect.upper), alpha = 0.2) +  # Add confidence interval ribbon
  labs(x = "year (?)", y = "Point Effect") +   theme_classic()

# combined 
point_effects_combined <- 
  ggpubr::ggarrange(afg_point_effects_plot, pfg_point_effects_plot, shr_point_effects_plot,
                                 tre_point_effects_plot, ltr_point_effects_plot, bgr_point_effects_plot,
                    labels = c('AFG', "PFG", "SHR", "TRE", "LTR", "BGR"),
                    ncol = 2, nrow =3)
point_effects_combined



## cumulative effects
afg_cum_effects_plot <- ggplot(data = synthControl$data$annual_forb_and_grass_cover$effect, 
                                 aes(x = seq_along(cum.effect), y = cum.effect)) +
  geom_hline(yintercept = 0, linewidth = 2, color = 'grey')+
  geom_vline(xintercept = 10, linewidth = 1.5, linetype = 'dashed')+
  geom_line(linewidth = 2) +  
  geom_ribbon(aes(ymin = cum.effect.lower, ymax = cum.effect.upper), alpha = 0.2) +  # Add confidence interval ribbon
  labs(x = "year (?)", y = "Point Effect") +   theme_classic()  
pfg_cum_effects_plot <- ggplot(data = synthControl$data$perennial_forb_and_grass_cover$effect, 
                                 aes(x = seq_along(cum.effect), y = cum.effect)) +
  geom_hline(yintercept = 0, linewidth = 2, color = 'grey')+
  geom_vline(xintercept = 10, linewidth = 1.5, linetype = 'dashed')+
  geom_line(linewidth = 2) +  
  geom_ribbon(aes(ymin = cum.effect.lower, ymax = cum.effect.upper), alpha = 0.2) +  # Add confidence interval ribbon
  labs(x = "year (?)", y = "Point Effect") +   theme_classic()  
shr_cum_effects_plot <- ggplot(data = synthControl$data$shrub_cover$effect, 
                                 aes(x = seq_along(cum.effect), y = cum.effect)) +
  geom_hline(yintercept = 0, linewidth = 2, color = 'grey')+
  geom_vline(xintercept = 10, linewidth = 1.5, linetype = 'dashed')+
  geom_line(linewidth = 2) +  
  geom_ribbon(aes(ymin = cum.effect.lower, ymax = cum.effect.upper), alpha = 0.2) +  # Add confidence interval ribbon
  labs(x = "year (?)", y = "Point Effect") +   theme_classic()  
tre_cum_effects_plot <- ggplot(data = synthControl$data$tree_cover$effect, 
                                 aes(x = seq_along(cum.effect), y = cum.effect)) +
  geom_hline(yintercept = 0, linewidth = 2, color = 'grey')+
  geom_vline(xintercept = 10, linewidth = 1.5, linetype = 'dashed')+
  geom_line(linewidth = 2) +  
  geom_ribbon(aes(ymin = cum.effect.lower, ymax = cum.effect.upper), alpha = 0.2) +  # Add confidence interval ribbon
  labs(x = "year (?)", y = "Point Effect") +   theme_classic()  
ltr_cum_effects_plot <- ggplot(data = synthControl$data$litter_cover$effect, 
                                 aes(x = seq_along(cum.effect), y = cum.effect)) +
  geom_hline(yintercept = 0, linewidth = 2, color = 'grey')+
  geom_vline(xintercept = 10, linewidth = 1.5, linetype = 'dashed')+
  geom_line(linewidth = 2) +  
  geom_ribbon(aes(ymin = cum.effect.lower, ymax = cum.effect.upper), alpha = 0.2) +  # Add confidence interval ribbon
  labs(x = "year (?)", y = "Point Effect") +   theme_classic()  
bgr_cum_effects_plot <- ggplot(data = synthControl$data$bare_ground_cover$effect, 
                                 aes(x = seq_along(cum.effect), y = cum.effect)) +
  geom_hline(yintercept = 0, linewidth = 2, color = 'grey')+
  geom_vline(xintercept = 10, linewidth = 1.5, linetype = 'dashed')+
  geom_line(linewidth = 2) +  
  geom_ribbon(aes(ymin = cum.effect.lower, ymax = cum.effect.upper), alpha = 0.2) +  # Add confidence interval ribbon
  labs(x = "year (?)", y = "Point Effect") +   theme_classic()

# combined 
cum_effects_combined <- 
  ggpubr::ggarrange(afg_cum_effects_plot, pfg_cum_effects_plot, shr_cum_effects_plot,
                    tre_cum_effects_plot, ltr_cum_effects_plot, bgr_cum_effects_plot,
                    labels = c('AFG', "PFG", "SHR", "TRE", "LTR", "BGR"),
                    ncol = 2, nrow =3)
cum_effects_combined

