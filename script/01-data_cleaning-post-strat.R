#### Preamble ####
# Purpose: To create a valid and self-contained dataset, which matches all the data in our survey dataset.
# Author: Haoming Hu
# Data: 30 October 2020
# Contact: haoming.hu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data. 
raw_data <- read_dta("usa_00001.dta"
                     )
# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
names(raw_data)

reduced_data <- 
  raw_data %>% 
  select(region,
         stateicp,
         sex, 
         age, 
         race, 
         hispan,
         marst, 
         bpl,
         citizen,
         educd,
         labforce,
         inctot)
rm(raw_data)

### Use this two functions to check our variables' details ###
#summary(reduced_data)
#levels(factor(reduced_data$stateicp))  

# Keep 6 variables that we are interested in
voters_data <- reduced_data %>%
  select(sex,
         race,
         educd,
         stateicp,
         age,
         labforce)
rm(reduced_data)

# Divided our 'race' vairble into 4 types:
# 'white', 'black', 'asian', and 'others'



voters_data <- voters_data %>% 
  mutate(race = case_when(
    race=="white" ~ "Others",
    race=="black/african american/negro" ~ "Black",
    race=="chinese" ~ "Asian",
    race=="japanese" ~ "Asian",
    race=="other asian or pacific islander" ~ "Asian",
    TRUE ~ "Others"
  )) 

# Divided our 'race' vairble into 4 types and 
# create a new column called 'education' to store them:
# 'highschool', 'undergraduatea or similar degree', 
# 'undergraduatea or similar degree', and 'less than highschool'
voters_data <- voters_data %>% filter(educd != "n/a") %>%
  mutate(education = case_when(
    educd=="grade 9" ~ "Highschool",
    educd=="grade 10" ~ "Highschool",
    educd=="grade 11" ~ "Highschool",
    educd=="grade 12" ~ "Highschool",
    educd=="12th grade, no diploma" ~ "Highschool",
    educd=="regular high school diploma" ~ "Highschool",
    educd=="ged or alternative credential" ~ "Highschool",
    educd=="some college, but less than 1 year" ~ "Undergraduates or similar degree",
    educd=="1 year of college" ~ "Undergraduates or similar degree",
    educd=="1 or more years of college credit, no degree" ~ "Undergraduates or similar degree",
    educd=="2 years of college" ~ "Undergraduates or similar degree",
    educd=="associate's degree, type not specified" ~ "Undergraduates or similar degree",
    educd=="associate's degree, occupational program" ~ "Undergraduates or similar degree",
    educd=="associate's degree, academic program" ~ "Undergraduates or similar degree",
    educd=="bachelor's degree" ~ "Undergraduates or similar degree",
    educd=="4 years of college" ~ "Undergraduates or similar degree",
    educd=="3 years of college" ~ "Undergraduates or similar degree",
    educd=="5+ years of college" ~ "Undergraduates or similar degree",
    educd=="6 years of college (6+ in 1960-1970)" ~ "Undergraduates or similar degree",
    educd=="7 years of college" ~ "Undergraduates or similar degree",
    educd=="8+ years of college" ~ "Undergraduates or similar degree",
    educd=="master's degree" ~ "Graduates or higher degree",
    educd=="professional degree beyond a bachelor's degree" ~ "Graduates or higher degree",
    educd=="doctoral degree" ~ "Graduates or higher degree",
    TRUE ~ "Less than highschool"
  )) 

# Simply rename sex to match the name in our survey
voters_data <- voters_data %>% 
  rename(gender = sex)

voters_data <- voters_data %>% 
  mutate(gender = case_when(
    gender=="male" ~ "Male",
    gender=="female" ~ "Female"
  ))

# Divided our 'age' vairble into 5 types called 'age_group' and
# store them. We also create a new data set to store the information.
voters_data_fit_age <- voters_data %>% mutate(age_data = as.numeric(unlist(voters_data$age))-1) %>%
  filter(age_data >= 18) %>%
  mutate(age_group = case_when(
    age_data >= 18 & age_data <= 29 ~ "Age_18-29",
    age_data >= 30 & age_data <= 44 ~ "Age_30-44",
    age_data >= 45 & age_data <= 59 ~ "Age_45-59",
    age_data >= 60 & age_data <= 74 ~ "Age_60-74",
    age_data >= 75 ~ "Age_75+"
  ))
rm(voters_data)

# Divided variable 'labforce' into 2 groups and store them
# in variable 'labforce'. This classification has one falw:
# Only two options, might cause inaccurate inferences
voters_data_fit_age <- voters_data_fit_age %>% 
  mutate(employment = case_when(
    labforce == "no, not in the labor force" ~ "unemployed",
    labforce == "yes, in the labor force" ~ "employed"
  ))

# Created variable called state_ab to store the Abbreviation 
# for all the state in post-strat to match those in survey data
voters_data_fit_age <- voters_data_fit_age %>% 
  mutate(state_ab = case_when(
    stateicp == "alabama" ~ "AL",
    stateicp == "alaska" ~ "AK",
    stateicp == "arkansas" ~ "AR",
    stateicp == "arizona" ~ "AZ",
    stateicp == "california" ~ "CA",
    stateicp == "colorado" ~ "CO",
    stateicp == "connecticut" ~ "CT",
    stateicp == "delaware" ~ "DE",
    stateicp == "district of columbia" ~ "DC",
    stateicp == "florida" ~ "FL",
    stateicp == "georgia" ~ "GA",
    stateicp == "hawaii" ~ "HI",
    stateicp == "idaho" ~ "ID",
    stateicp == "illinois" ~ "IL",
    stateicp == "indiana" ~ "IN",
    stateicp == "iowa" ~ "IA",
    stateicp == "kansas" ~ "KS",
    stateicp == "kentucky" ~ "KY",
    stateicp == "louisiana" ~ "LA",
    stateicp == "maryland" ~ "MD",
    stateicp == "massachusetts" ~ "MA",
    stateicp == "michigan" ~ "MI",
    stateicp == "minnesota" ~ "MN",
    stateicp == "mississippi" ~ "MS",
    stateicp == "missouri" ~ "MO",
    stateicp == "montana" ~ "MT",
    stateicp == "nebraska" ~ "NE",
    stateicp == "nevada" ~ "NV",
    stateicp == "new hampshire" ~ "NH",
    stateicp == "new jersey" ~ "NJ",
    stateicp == "new mexico" ~ "NM",
    stateicp == "new york" ~ "NY",
    stateicp == "north carolina" ~ "NC",
    stateicp == "north dakota" ~ "ND",
    stateicp == "ohio" ~ "OH",
    stateicp == "oklahoma" ~ "OK",
    stateicp == "oregon" ~ "OR",
    stateicp == "pennsylvania" ~ "PA",
    stateicp == "rhode island" ~ "RI",
    stateicp == "south carolina" ~ "SC",
    stateicp == "south dakota" ~ "SD",
    stateicp == "tennessee" ~ "TN",
    stateicp == "texas" ~ "TX",
    stateicp == "utah" ~ "UT",
    stateicp == "vermont" ~ "VT",
    stateicp == "virginia" ~ "VA",
    stateicp == "washington" ~ "WA",
    stateicp == "wisconsin" ~ "WI"
  ))

# Create a new data set, which includes all the data we want
all_voters_data <- voters_data_fit_age %>%
  select(gender, race, education, state_ab, age_group, employment)
rm(voters_data_fit_age)

# We used the next 4 steps to generate the number of valid responders in each 
# state, and the number of valid people in each individual cell. After which,
# we calculated the cell_prop_of_division_total.
all_voters_data <- all_voters_data %>%
  group_by(state_ab) %>% mutate(people_in_each_state = n()) %>% ungroup()

all_voters_data <- all_voters_data %>%
  group_by(state_ab, gender, race, education,age_group, employment) %>%
  mutate(number_in_cell = n()) %>% ungroup()

all_voters_data <- all_voters_data %>% distinct()

all_voters_data <- all_voters_data %>% 
  mutate(cell_prop_of_division_total = number_in_cell / people_in_each_state)

# Assorted our dataset to enable a better understanding
sorted_all_voters_data <- all_voters_data[order(all_voters_data$state_ab, all_voters_data$gender, 
                                                all_voters_data$race, all_voters_data$education,
                                                all_voters_data$age_group, all_voters_data$employment), ]
rm(all_voters_data)

# Rename to fit our survey dataset
sorted_all_voters_data <- sorted_all_voters_data %>% 
  rename(state = state_ab,
         age = age_group)

# Way to rearrange the columns
sorted_all_voters_data[,c(2,1,3,4)]

# Use the function below to produce a complete and valid dataset
write.csv(sorted_all_voters_data, file="post-strat-data.csv")

