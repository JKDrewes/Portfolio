library(tidycensus)
library(tidyverse)
library(data.table)
library(srvyr)
source("/home/nixy/Desktop/Personal_Projects/Health_Coverage_IL/keys.R")

#HICOV #boolean health insurance
##### b = NA
##### 1 = Yes
##### 2 = No
#COW #Class of Worker
##### b N/A (less than 16 years old/NILF who last worked more than 5 years ago or never worked)
##### 1 Employee of a private for-profit company or business, or of an individual, for wages, salary, or commissions
##### 2 Employee of a private not-for-profit, tax-exempt, or charitable organization
##### 3 Local government employee (city, county, etc.)
##### 4 State government employee
##### 5 Federal government employee
##### 6 Self-employed in own not incorporated business, professional practice, or farm
##### 7 Self-employed in own incorporated business, professional practice or farm
##### 8 Working without pay in family business or farm
##### 9 Unemployed and last worked 5 years ago or earlier or never worked
#SCH #School attendence
##### b = NA
##### 1 = No, has not attended in past 3 months
##### 2 = Yes, public school
##### 3 = Yes, Private school
#SEX #gender binary
##### 1 = Male
##### 2 = Female
#DIS #Disability Boolean
##### 1 = Yes
##### 2 = No
#ESP #Employment of parents
#HINCP #Income last 12 months
#PUMA #area
#AGEP #age
#WKHP #Usual amount of hours worked per week for past 12 months

raw_il_data = copy(il_acs)
nrow(raw_il_data) #621164

#The ACS only surveys <5% of the population so we have to use re-sampling in order to estimate the population totals
survey_design = to_survey(raw_il_data)
grouped_person_data = survey_design %>% 
  survey_count(PUMA,
               HICOV,
               MULTG,
               COW,
               SCH,
               SEX,
               DIS,
               ESP,
               HINCP,
               AGEP,
               WKHP)

print("Fit Replicate Weights")

#Unroll the grouped data so it is fully weighted
person_data = grouped_person_data %>%
  uncount(n) %>%
  select(-c(n_se))

nrow(person_data) #12821813

#filter out children as they are not in target population
adult_il_data = person_data %>%
  filter(AGEP >= 18) %>%
  mutate(HICOV = case_when(HICOV == "2" ~ "0",
                           HICOV == "1" ~ "1",
                           .default = NA)) %>%
  mutate(across(where(is.character), ~na_if(., "b"))) %>%
  select(-c(ESP, PUMA)) #This becomes NA after we cut out children. 
#Re-implement PUMA after I've upgraded to tidymodels

nrow(adult_il_data) #9935060

#Split off an exploratory sample
sample_size = floor(0.01 * nrow(adult_il_data))
eda_ind = sample(seq_len(nrow(adult_il_data)), size = sample_size)
explore_sample = adult_il_data[eda_ind, ]
adult_il_data = adult_il_data[-eda_ind, ]

#Not going to impute today
adult_il_sample = adult_il_data %>%
  na.omit(adult_il_data) %>%
  sample_n(500) %>%
  mutate(across(where(is.character), factor))

#I could have avoided making some of these but did so for legibility.
#Removing those that are unneeded before the exploration and analysis stages
rm(adult_il_data, 
   person_data, 
   grouped_person_data, 
   il_acs, 
   raw_il_data)