# Ecology data analysis
# A. Bailey 3rd May 2017

# In this script we are going to recapitulate some of the analysis found in 
# the paper by Heske et. al., "Long-Term Experimental Study of a Chihuahuan 
# Desert Rodent Community: 13 Years of Competition", DOI:10.2307/1939547.

# Specifically we are going to look at the effect on the populations of small 
# seed eating rodents as a result of the exclusion of larger competitor kangeroo
# rats over a period from 1977 to 1991.

# This will provide the opportunnity to practice importing data, transforming 
# data, and plotting data, and finally turning this script into a report.

# We will do this using the tidyverse set of packages created by Hadley
# Wickham.

# Load the tidyverse set of packages -------------------------------------------

library(tidyverse)

# Download the and import the data ---------------------------------------------

# First we assign the web address as a string to a R object called 'url'.
# Then we assign another string object called 'path' that states where we 
# want to import the data to.
# Next we use and 'if not' statement to check if the data has alredy been 
# downloaded. The '!' in front of 'file.exists' is a logical operator for 'not'.
# If there is no file called 'portal_data_joined.csv' in the 'data' folder,
# download the file.

# URL for portal joined survey data
url <- "https://ndownloader.figshare.com/files/2292169"
# Path and filename for downloading
path <- "novice/R-ecology-lesson/data/combined.csv"

# Check if the file exists, and if not download it to the data folder
if (!file.exists(path)) { download.file(url,path) }

# Next we import the data using the 'readr' package and 'read_csv' function
# We'll assign the data to a R object called 'surveys'. 
# read_csv creates a 'tibble' a modified form of data frame with enhanced
# printing and checking capabilites.
#surveys <- read_csv('novice/R-ecology-lesson/data/portal_data_joined.csv')
surveys <- read_csv('novice/R-ecology-lesson/data/portal_data_joined.csv')

# Inspect the data with 'glimpse' ----------------------------------------------

glimpse(surveys)

# This data is tidy already: each variable is a column, each observation is a
# row and each cell contains a single variable.

# Q: Is there missing data?

# Q: How long a period does the dataset cover?

# Q: How many different taxa are there? 
# (taxa are organism types e.g. plants or birds)

# Q: How many species are there?
# (individual species belong in their taxa)

# For this analysis we need to subset the plots that were used in the paper,
# the species and taxa that were analysed, the time period of the experiment, and
# remove any missing data.

# Subsetting data --------------------------------------------------------------

# Q: Which plots do we need for this analysis?
exp_plots <- c("Control",
               "Long-term Krat Exclosure",  
               "Short-term Krat Exclosure")

# Q: Which species do we need for this analysis?
# Use: https://github.com/weecology/PortalData/blob/master/Rodents/Portal_rodent_species.csv for codes.

# Kangeroo Rats:
# DM 	Dipodomys merriami 	Rodent 	Merriam's kangaroo rat
# DO 	Dipodomys ordii 	Rodent 	Ord's kangaroo rat
# DS 	Dipodomys spectabilis 	Rodent 	Banner-tailed kangaroo rat

# Granivores:
# PP 	Chaetodipus penicillatus 	Rodent 	Desert pocket mouse
# PF 	Perognathus flavus 	Rodent 	Silky pocket mouse
# PE 	Peromyscus eremicus 	Rodent 	Cactus mouse
# PM 	Peromyscus maniculatus 	Rodent 	Deer Mouse
# RM 	Reithrodontomys megalotis 	Rodent 	Western harvest mouse

# Grasshopper mouse:
# OX 	Onychomys sp. 	Rodent 	Grasshopper mouse

# List of species_id
#rodents <- c("DM","DO","DS","PP","PF","PE","PM","RM","OX")
rodents <- c("DM","DO","DS","PP","PF","PE","PM","RM")

# Control Plots: 2,8,12,22
# Treatment Plots: 3,15,19,21
exp_plots <- c(8,11,12,14,3,15,19,21)

# Let's get filter the data we need:
# surveys_sub <- surveys %>%
#   filter(species_id != "",            # remove missing species_id
#          !is.na(weight),              # remove missing weight
#          !is.na(hindfoot_length),     # remove missing hindfoot_length
#          sex != "",                   # remove missing sex
#          year <= 1991,                # Subset time period 1977 to 1991
#          plot_id %in% plots,    # Subset the plots used in the paper
#          species_id %in% rodents) %>%
#   mutate(Date = dmy(sprintf('%02d%02d%04d', day, month, year))) %>%
#   filter(Date >= as.Date("1977-09-01") & Date <= as.Date("1990-12-31")) %>%
#   mutate(Quarter = lubridate::quarter(Date,with_year = TRUE))

# We get a warning but, we've filtered them out
surveys_sub <- surveys %>% 
  # Add a date columns
  mutate(Date = dmy(sprintf('%02d%02d%04d', day, month, year))) %>%
  # Drop unwanted columns
  select(-sex,-hindfoot_length,-weight) %>% 
  # Filter time period in paper
  filter(plot_id %in% plots, # Subset the plots
         species_id %in% rodents, # Subset the species of interest
         Date >= as.Date("1977-09-01") & Date <= as.Date("1990-12-31")) %>% 
  # Add the quartley period
  mutate(Quarter = lubridate::quarter(Date,with_year = TRUE))

# Check whether there are any NAs
which(is.na(surveys_sub$Date))

glimpse(surveys_sub)

# Q: How many of each species by genus?
surveys_sub %>% group_by(genus) %>% count(species)

# Check our selected plots
distinct(surveys_sub,plot_id,plot_type)

# Factors ----------------------------------------------------------------------

# Let's turnthe genus,species and plot_types into factors.
# Then we can group them accordingly.

# Check levels
levels(surveys_sub$species_id)

# We have three groups we want: the Kangeroo Rats, the smaller granivores,
# and the grasshopper mouse.

# Kangeroo Rats:
# DM 	Dipodomys merriami 	Rodent 	Merriam's kangaroo rat
# DO 	Dipodomys ordii 	Rodent 	Ord's kangaroo rat
# DS 	Dipodomys spectabilis 	Rodent 	Banner-tailed kangaroo rat

# Granivores:
# PP 	Chaetodipus penicillatus 	Rodent 	Desert pocket mouse
# PF 	Perognathus flavus 	Rodent 	Silky pocket mouse
# PE 	Peromyscus eremicus 	Rodent 	Cactus mouse
# PM 	Peromyscus maniculatus 	Rodent 	Deer Mouse
# RM 	Reithrodontomys megalotis 	Rodent 	Western harvest mouse

surveys_sub <- surveys_sub %>% 
  mutate(species_name = fct_collapse(species_id,
                                "Kangeroo Rat" = c("DM","DO","DS"),
                                "Granivore" = c("PP","PF","PE","PM","RM")))#, 
                                #"Grasshopper mouse"= "OX")) 

  #mutate(species = as.factor(species))

# Check the new levels
surveys_sub %>% group_by(genus,species,plot_type) %>% count(species)
surveys_sub %>% group_by(genus,plot_type) %>% count(genus)


# Transform the data for plotting ----------------------------------------------

# Let's aggregate the time data to every three months and calculate the number
# of caputures
by_month <- surveys_sub %>% 
  group_by(species_name,Date,plot_type) %>% 
  summarise(captures = n()) 

by_quarter <- surveys_sub %>% 
  group_by(species_name,Quarter,plot_type) %>% 
  summarise(captures = n()/3) 


ggplot(data = by_quarter,aes(x=Quarter,y=captures,colour=plot_type)) + 
  geom_line() +
  geom_point() + 
  facet_grid(~ species_name) +
  scale_x_continuous(breaks = seq(1977,1991,1))

ggplot(data = by_month,aes(x=Date,y=captures,colour=plot_type)) + geom_line() +
  geom_point() + 
  facet_grid(~ species_name) 

ggplot(data = by_quarter,aes(x=Quarter,y=captures,colour=genus)) + geom_line() +
  geom_point() + 
  facet_grid(plot_type ~ genus)

by_species <- group_by(surveys_sub,species,plot_type) %>% 
  filter(genus == "Granivore")
species_sum <- summarise(by_species,count = n())

ggplot(data= species_sum) +
  geom_bar(
    mapping = aes(x = plot_type, y = count, fill = plot_type),
    position = "dodge",
    stat = "identity"
  ) +
  facet_wrap( ~ species)
