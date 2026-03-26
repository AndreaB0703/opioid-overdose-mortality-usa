# clear environment
rm(list = ls())
install.packages("DHARMa")
# load packages
library(tidyverse)
library(here)
library(lme4)
library(DHARMa)
library(performance)

# read data
opioid_any <- read.csv2(here("data", "processed_data", "opioid_any_2013_2024_clean.csv"))

# basic check
glimpse(opioid_any)
dim(opioid_any)

# check missing values
colSums(is.na(opioid_any))

# check categories
sort(unique(opioid_any$year))
sort(unique(opioid_any$sex))
sort(unique(opioid_any$race))
sort(unique(opioid_any$age))
length(unique(opioid_any$state))

table(opioid_any$race, useNA = "ifany")
prop.table(table(opioid_any$race))

#colapse small race groups
opioid_any <- opioid_any |> 
  mutate(race_3cat = case_when(race == "White" ~ "White",
      race == "Black or African American" ~ "Black",TRUE ~ "Other"))
#check
table(opioid_any$race_3cat)
prop.table(table(opioid_any$race_3cat))

#recode race
opioid_any <- opioid_any |> 
  mutate(race_3cat = case_when(race == "White" ~ "White",
      race == "Black or African American" ~ "Black",TRUE ~ "Other"))

# convert variables
opioid_any <- opioid_any |> 
  mutate(state = factor(state),
    sex = factor(sex),
    age = factor(age),
    race_3cat = factor(race_3cat, levels = c("White", "Black", "Other")),
    year_c = year)

# check
glimpse(opioid_any)
table(opioid_any$race_3cat)

#model poisson
opioid_any <- opioid_any |> 
  mutate(year_centered = year - mean(year))

model_poisson_1 <- glmer(deaths ~ year_centered + sex + age + race_3cat +
    offset(log(population)) + (1 | state),
  data = opioid_any,family = poisson(link = "log"))

summary(model_poisson_1)

#simulation poisson
simulation_poisson_1 <- simulateResiduals(model_poisson_1)

plot(simulation_poisson_1)
testDispersion(simulation_poisson_1)
testZeroInflation(simulation_poisson_1)

#summary
table(opioid_any$deaths == 0)
summary(opioid_any$deaths)