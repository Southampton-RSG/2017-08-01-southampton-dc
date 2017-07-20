#' ---
#' title: "Competition in Desert Rodents"
#' author: A. Bailey 
#' date: 21st June 2017
#' ---

#' This script recapitulates some of the analysis found in the 1994 paper by 
#' Heske et. al., Long-Term Experimental Study of a Chihuahuan Desert Rodent 
#' Community: 13 Years of Competition, DOI: 10.2307/1939547.
#'
#' This paper examined the effect on the populations of small seed eating rodents
#' as a result of the exclusion of larger competitor kangeroo rats over a period 
#' from 1977 to 1991.
#'
#'### Load the tidyverse set of packages
library(tidyverse)
library(forcats)

#'### Read in the data
#' Assuming the data has been downloaded to the data folder:
surveys <- read_csv('../data/portal_data_joined.csv')

#' This data has 34,786 observations of 13 variables.
#' #'
#' We only need the observations for 8 species and 8 plots.
#'
# Kangeroo Rats:
# DM 	Dipodomys merriami 	        Rodent  Merriam's kangaroo rat
# DO 	Dipodomys ordii             Rodent  Ord's kangaroo rat
# DS 	Dipodomys spectabilis 	    Rodent 	Banner-tailed kangaroo rat

# Granivores:
# PP 	Chaetodipus penicillatus  	Rodent 	Desert pocket mouse
# PF 	Perognathus flavus 	        Rodent 	Silky pocket mouse
# PE 	Peromyscus eremicus         Rodent  Cactus mouse
# PM 	Peromyscus maniculatus 	    Rodent 	Deer Mouse
# RM 	Reithrodontomys megalotis 	Rodent 	Western harvest mouse

# Create a vector for the experimental plots, four controls, four kangeroo rat
# exclusion plots
exp_plots <- c(8,11,12,14,3,15,19,21)

# Create a character vector for rodents
# Create a named vector as a lookup table, where the names of each vector
# element correspond with the species_id, and the values of each vector 
# element are either Kangaroo Rat or Granivore
lut <- c("DM" = "Kangaroo Rat",
         "DO" = "Kangaroo Rat",
         "DS" = "Kangaroo Rat",
         "PP" = "Granivore",
         "PF" = "Granivore",
         "PE" = "Granivore",
         "PM" = "Granivore",
         "RM" = "Granivore")

surveys_subset <- surveys %>% 
  # Filter observations from 1977 to 1990, for the 3 K-rats and 5 granivores, 
  # and for the 4 experimental plots
  filter(year <= 1990,                    
         species_id %in% names(lut),         
         plot_id %in% exp_plots) %>%      
  # Use lookup table to create variable to indicate whether species is K-rat 
  # or Granivore
  mutate(rodent_type = lut[species_id],
         # Make combined date variable
         date = lubridate::dmy(sprintf('%02d%02d%04d', 
                                       day, month, year)),
         # Add the quartley period
         quarter = lubridate::quarter(date,with_year = TRUE)) %>% 
  # Drop unwanted variables
  select(-sex,-hindfoot_length,-weight)

#'### Summarise data 
# Summarise the data by rodent type, quarterly survey and plot type
by_quarter <- surveys_subset %>% 
  group_by(rodent_type,quarter,plot_type) %>% 
  summarise(captures = n()/3) 

#'### Plot the data 
# Create rodent levels by getting the unique values for rodent type
# and putting them in reverse order for plotting
rodent_levels <-  rev(unique(by_quarter$rodent_type))

# Convert rodent_type to factors such that Kangeroo rat is first and Granivore
# is second
by_quarter$rodent_type <- factor(by_quarter$rodent_type, 
                                 levels = rodent_levels)

# Now recreate the published figure in black and white
pub_plot <- ggplot(data = by_quarter,
                   aes(x=quarter, y=captures, linetype = plot_type,
                       shape = plot_type)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ rodent_type, nrow = 2) +
  scale_x_continuous(breaks = seq(1978,1990,2)) +
  labs(x ="YEAR",
       y ="CAPTURES / PERIOD",
       linetype = "",
       shape = "") +
  theme_linedraw() +
  theme(axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size=12),
        panel.grid = element_blank(),
        aspect.ratio = 0.6, 
        legend.position = "top")

#+ fig.height=10
pub_plot