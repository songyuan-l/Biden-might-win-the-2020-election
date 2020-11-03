#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from：
# Tausanovitch, Chris and Lynn Vavreck. 2020. Democracy Fund + UCLA Nationscape, October 10-17, 2019 (version 20200814). 
# https://www.voterstudygroup.org/downloads?key=1c2ff38b-5ade-4f75-856b-616f859becff
# Author: Chelsea Shen 
# Data: 26 October 2020
# Contact: chelsea.shen@mail.utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
library(ggplot2)
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("input/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
raw_data
# Kept variables with respondent's information 
reduced_data <- 
  raw_data %>% 
  select(vote_2020,
         employment,
         gender,
         race_ethnicity,
         education,
         state,
         age)

# renamed some variable, race and supports_Trump
names(reduced_data)[names(reduced_data) == "race_ethnicity"] <- "race"
names(reduced_data)[names(reduced_data) == "vote_2020"] <- "supports_Trump"

#summary(reduced_data$supports_Trump)
#levels(factor(reduced_data$supports_Trump))  

name <- c("Donald Trump", "Joe Biden")
# Changed some variable values, removed unneccesarily features and values. 
reduced_data <- reduced_data %>% filter(supports_Trump %in% name) %>%
  mutate(trump = case_when(
    supports_Trump=="Donald Trump" ~ 1,
    supports_Trump=="Joe Biden" ~ 0
  )) 

#levels(factor(reduced_data$employment))
# mutated some variable 
reduced_data <- reduced_data %>% 
  mutate(employment = case_when(
    employment=="Full-time employed" ~ 'employed',
    employment=="Homemaker" ~ 'unemployed',
    employment=="Retired" ~ 'unemployed',
    employment=="Unemployed or temporarily on layoff" ~ 'unemployed',
    employment=="Part-time employed" ~ 'employed',
    employment=="Permanently disabled" ~ 'unemployed',
    employment=="Student" ~ 'unemployed',
    employment=="Self-employed" ~ 'employed'
  )) 

#levels(factor(reduced_data$race))
# categorized racial groups
reduced_data <- reduced_data %>% 
  mutate(race = case_when(
    race=="Black, or African American" ~ 'Black',
    race=="American Indian or Alaska Native" ~ 'Others',
    race=="Pacific Islander (Native Hawaiian)" ~ 'Others',
    race=="Pacific Islander(Guamanian)" ~ 'Others',
    race=="Pacific Islander(Samoan)" ~ 'Others',
    race=="Pacific Islander(Other)" ~ 'Others',
    race=="Some other race" ~ 'Others',
    race=="Asian (Asian Indian)" ~ 'Asian',
    race=="Asian (Chinese)" ~ 'Asian',
    race=="Asian (Filipino)" ~ 'Asian',
    race=="Asian (Japanese)" ~ 'Asian',
    race=="Asian (Korean)" ~ 'Asian',
    race=="Asian (Vietnamese)" ~ 'Asian',
    race=="Asian (Other)" ~ 'Asian',
    race=="White" ~ 'White'
  )) 

# levels(factor(reduced_data$education))
# categorized education 
reduced_data <- reduced_data %>% 
  mutate(education = case_when(
    education=="3rd Grade or less" ~ 'Less than highschool',
    education=="Middle School - Grades 4 - " ~ 'Less than highschool',
    education=="Completed some high schoo" ~ 'Highschool',
    education=="High school graduate" ~ 'Highschool',
    education=="Other post high school vocational training" ~ 'Undergraduates or similar degree',
    education=="Completed some college, but no degree" ~ 'Undergraduates or similar degree',
    education=="Associate Degree" ~ 'Undergraduates or similar degree',
    education=="College Degree (such as B.A., B.S.)" ~ 'Undergraduates or similar degree',
    education=="Completed some graduate, but no degree" ~ 'Undergraduates or similar degree',
    education=="Masters degree" ~ 'Graduates or higher degree',
    education=="Doctorate degree" ~ 'Graduates or higher degree',
  ))

# changed age to age group
reduced_data <- reduced_data %>% 
  mutate(age = case_when(
    age <= 29 ~ 'Age_18-29',
    age >= 30 & age <= 44 ~ 'Age_30-44',
    age >= 45 & age <= 59 ~ 'Age_45-59',
    age >= 60 & age <= 74 ~ 'Age_60-74',
    age >= 75  ~ 'Age_75+'
  ))

######
reduced_data <- reduced_data %>% select(gender, race, education, state, age, employment, trump)

#levels(factor(reduced_data$race))
summary(reduced_data)


# Sort the data
reduced_data <- reduced_data[order(reduced_data$state, reduced_data$gender, reduced_data$race, reduced_data$education,
                                   reduced_data$age, reduced_data$employment),]


# write the csv file
 write.csv(reduced_data, file="survey-data.csv")



# Lets look at the age distribution 

support_set <- reduced_data %>% group_by(trump, age) %>% count()


# we divide our dataset into 2 groups by their opinions
support <- support_set %>% filter(trump == 1)
support <- support %>% mutate(percent = round(100*n/sum(support$n), 1), value = n) # Add a percentage variable on the dataset
opposed <- support_set %>% filter(trump == 0)
opposed <- opposed %>% mutate(percent = round(100*n/sum(opposed$n), 1), value = n) # Add a percentage variable on the dataset

# Using a pie chart to illustrates for those who support Trump , what are their age group
support_graph<- ggplot(support, aes(x="", y=percent, fill=`age`))+
  geom_bar(width = 1, stat = "identity") + 
  geom_text(aes(label = percent), size=3, position = position_stack(vjust = 0.5))
pie_support <- support_graph + coord_polar("y", start=0)

# Using a pie chart to illustrates for those who support Biden , what are their age group
opposed_graph<- ggplot(opposed, aes(x="", y=percent, fill=`age`))+
  geom_bar(width = 1, stat = "identity") + 
  geom_text(aes(label = percent), size=3, position = position_stack(vjust = 0.5))
pie_opposed <- opposed_graph + coord_polar("y", start=0)


# Defined theme for a more clear pie chart
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

# We made a better pie chart to show for those who support the Trump, what are their age group
pie_support <- pie_support + blank_theme +
  theme(axis.text.x=element_blank()) +
  labs(fill = "supports_ALP",
       x = NULL,
       y = NULL,
       title = "Age Group of different voters to Trump",
       caption = "")

pie_opposed <- pie_opposed + blank_theme +
  theme(axis.text.x=element_blank()) +
  labs(fill = "supports_ALP",
       x = NULL,
       y = NULL,
       title = "Age Group of different voters to Biden",
       caption = "")
# Figure 1, histgram comparing Donald Trump and Biden's supporter
hist(reduced_data$trump)
# Pie charts of age groups of our respondents
pie_support
pie_opposed

