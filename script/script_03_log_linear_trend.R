# load packs
library(tidyverse)
library(here)
library(readr)
# Log-linear regression to estimate overall temporal trend APC - Annual Percentage Change
# read data age_adjusted
age_adjusted_year <- read.csv2(here("output", "age_adjusted_rate_year.csv"))

#check
list.files("output")

# check structure
glimpse(age_adjusted_year)
age_adjusted_year

#Global APC (Annual Percentage Change)
model_apc <- lm(log(age_adjusted_rate) ~ year, data = age_adjusted_year)
summary(model_apc)

#exp
apc <- (exp(coef(model_apc)["year"]) - 1) * 100
apc

#Confident Interval APC
beta_ci <- confint(model_apc)["year", ]
apc_ci <- (exp(beta_ci) - 1) * 100
apc_ci

#dataframe for APC results (tibble X data.frame = null= year)
apc_results <- data.frame(
  measure = "Age-adjusted opioid overdose mortality rate",
  period = "2013-2024",
  apc = round((exp(coef(model_apc)["year"]) - 1) * 100, 2),
  ci_lower = round((exp(confint(model_apc)["year", 1]) - 1) * 100, 2),
  ci_upper = round((exp(confint(model_apc)["year", 2]) - 1) * 100, 2),
  p_value = summary(model_apc)$coefficients["year", "Pr(>|t|)"])

apc_results

#save apc results output
write.csv2(apc_results,here("output", "apc_results.csv"),
  row.names = FALSE)

#check 
list.files("output")

#fix
rownames(apc_results) <- NULL
apc_results

