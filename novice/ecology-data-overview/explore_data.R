# Explore data carpentry data
# A.Bailey 20th April 2017

# Concretely, we will aim to understand how rodent species populations vary over 
# time and by location in this survey data

# Load packages and data -------------------------------------------------------
library(tidyverse)
# Using this we avoid stringsasfactors
surveys <- read_csv("data/portal_data_joined.csv")

# Explore data -----------------------------------------------------------------
# How many variables and how many observations?
# What classes are they?
glimpse(surveys)

# Just the variable name
names(surveys)

# General indexing rules
surveys[1]
surveys[,2]
surveys[1,1]
surveys[1,6]

surveys %>% distinct(plot_id)
surveys %>% distinct(species)

# Questions:
# Over what timespan did this survey take place?
survey_timerange <- range(surveys$year)
survey_timerange[2]-survey_timerange[1]
# How many plots?
n_plots <- unique(surveys$plot_id)
str(n_plots)
# How many different plot types?
n_plot_types <- unique(surveys$plot_type)
# How many species?
n_species <- unique(surveys$species)
str(n_species)
# Is there missing data?
head(is.na.data.frame(surveys))

# Surveys over 25 years, 24 plots of 5 different types surveying 40 species
# of rodent.


# Create a complete dataset ----------------------------------------------------
# Use dplyr and logical operators to remove missing values
surveys_complete <- surveys %>%
  filter(species_id != "",         # remove missing species_id
         !is.na(weight),           # remove missing weight
         !is.na(hindfoot_length),  # remove missing hindfoot_length
         sex != "")                # remove missing sex

## Extract the most common species_id
species_counts <- surveys_complete %>%
  group_by(species_id) %>%
  tally %>%
  filter(n >= 50)

## Only keep the most common species
surveys_complete <- surveys_complete %>%
  filter(species_id %in% species_counts$species_id)

## Plot -----------------------------------------------------


# Subset species ---------------------------------------------------------------
# Lets subset the data for three species, harrisi
ggplot(surveys_complete,aes(x=sex)) + geom_bar() + facet_grid(.~species)

survey_sub <- surveys_complete %>% 
  filter(species %in% c("penicillatus","ordii","baileyi") & plot_type %in% c("Control","Rodent Exclosure","Spectab exclosure")) %>% 
  group_by(sex,species)
  
# Remove NAs
glimpse(survey_sub)
any(is.na(survey_sub))
#unique(survey_sub$species)
#levels(survey_sub$species)

# Is there a relationship between variables?
# Introduce factors to discuss comparing catergorical variables
# Sex, Year, Month extra are catergories whilst weight is not
# So it's more useful to work with them as factors
survey_sub$sex <- factor(survey_sub$sex)
levels(survey_sub$sex)
# Let's rename them to something more useful
levels(survey_sub$sex) <- c("Female","Male")
# Or in one go using factor labels
survey_sub$sex <- factor(survey_sub$sex,labels=c("Female","Male"))
survey_sub$species <- factor(survey_sub$species,
                             labels=c("Bush Tail","Kangeroo","Bailey"))
#survey_sub$month <- factor(survey_sub$month)
#survey_sub$year <- factor(survey_sub$year)
plot(survey_sub$sex)
plot(survey_sub$species)

# Use grouping  and chaining ---------------------------------------------------
# Are there differences in weight between sex?
# Yes, males are on average heavier than females for Bush Tail and Kangeroo
# but they are similar for Bailey rats. We can't see the variation.
survey_sub %>%
  group_by(sex,species,plot_type) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))

# Summarize the exteremes
survey_sub %>%
  group_by(sex, species,plot_type) %>%
  summarize(mean_weight = mean(weight),
            min_weight = min(weight))

annual_rat_pop <- survey_sub %>%
  group_by(sex,species,year,plot_type) %>%
  tally

survey_sub %>%
  group_by(sex,species,year,plot_type) %>%
  summarise(n())

monthly_rat_pop <- survey_sub %>%
  group_by(sex,species,month,plot_type) %>%
  tally

# Plotting the data to explore it ----------------------------------------------

# Plot rat numbers
# Are the populations changing over time
ggplot(annual_rat_pop,aes(year,n,group=species,colour=sex)) + 
  geom_line() +
  #geom_smooth() +
  facet_wrap(plot_type~species)


# Are the populations changing over time
ggplot(monthly_rat_pop,aes(month,n,group=species,colour=sex)) + 
  geom_point() +
  geom_smooth() +
  facet_wrap(sex~species)


# How do these species compare for size?
ggplot(survey_sub,aes(x=weight,fill=species)) +
  geom_density()

# Are there differences in weight between sex?
# Yes, males are on average heavier than females for Bush Tail and Kangeroo
# but they are similar for Bailey rats. We see more how the distributions vary.
ggplot(survey_sub,aes(x=sex,y=weight,group=sex,fill=sex)) +
  geom_boxplot() + 
 # geom_jitter(alpha=0.2) +
  facet_wrap(plot_type~species)

# ggplot(survey_sub,aes(month,weight,colour=sex)) + 
#   geom_point() + 
#   geom_smooth() +
#   facet_wrap(sex~species)

ggplot(survey_sub,aes(plot_type,weight,fill=sex)) + 
  geom_boxplot() + 
  # geom_smooth() +
  facet_wrap(sex~species)

# Does weight vary over the year on average?
ggplot(survey_sub,aes(month,weight,colour=sex)) + 
  geom_boxplot() +
  #geom_point(aes(alpha=0.7)) +
  geom_jitter(alpha=0.2) +
  #geom_smooth(aes(group=1),se=F) +
  facet_wrap(sex~species)

# Reorientate axis

#library(manipulate)


ggplot(survey_sub,aes(x=year,y=weight,fill=species)) +
  geom_boxplot() + facet_wrap(~species)


# Model Kangeroo population ---------------------------------------------------
library(modelr)

bailey <- filter(annual_rat_pop, species == "Bailey")
#bailey$year <- as.integer(bailey$year)

bailey %>% 
  ggplot(aes(year,n)) +
  geom_line() +
  ggtitle("Full data = ")

bailey_mod <- lm(n ~ as.integer(year), data = bailey)

bailey %>% 
  add_predictions(bailey_mod) %>% 
  ggplot(aes(year,pred)) +
  geom_line() +
  ggtitle("Linear Trend + ")

bailey %>% 
  add_residuals(bailey_mod) %>% 
  ggplot(aes(year,resid)) +
  geom_line() +
  ggtitle("Remaining variation")
