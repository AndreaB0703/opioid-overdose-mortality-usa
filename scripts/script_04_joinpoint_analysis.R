# clear environment
rm(list = ls())

# This script prepares clean annual age-adjusted mortality rates and standard errors
# for Joinpoint regression, excluding age group 85+ and exporting CSV files directly
# from R to avoid spreadsheet-related decimal corruption.
# load packages
library(tidyverse)
library(here)

# read final processed database for any opioid
opioid_any <- read.csv2(here("data", "processed_data", "opioid_any_2013_2024_clean.csv"))

# read standard population weights
weights_age <- read.csv2(here("data", "raw_data", "age_standard_weights.csv"))

# convert weights to numeric and standardize age labels
weights_age <- weights_age |> 
  mutate(weight = as.numeric(weight),
    age_group = str_replace_all(age_group, "–", "-"))

# check
glimpse(opioid_any)
weights_age
sum(weights_age$weight)
sort(unique(weights_age$age_group))

# any opioid: exclude 85+
opioid_any_no85 <- opioid_any %>%
  filter(age != "85+")

# any opioid: aggregate by year and age
any_year_age <- opioid_any_no85 |> 
  group_by(year, age, weight) |> 
  summarise(deaths = sum(deaths),population = sum(population),.groups = "drop") |> 
  mutate(rate_age = deaths / population,
    var_component = (weight^2) * (deaths / (population^2)))

# any opioid: annual age-adjusted rate and standard error
joinpoint_any <- any_year_age |> 
  group_by(year) |> 
  summarise(age_adjusted_rate = sum(rate_age * weight) * 100000,
    variance = sum(var_component) * (100000^2),
    standard_error = sqrt(variance),.groups = "drop") |> 
  select(year, age_adjusted_rate, standard_error)

# save clean csv for Joinpoint
write.csv(joinpoint_any,here("output", "joinpoint_input_any_opioid.csv"),
  row.names = FALSE,quote = FALSE)

# check
joinpoint_any
readLines(here("output", "joinpoint_input_any_opioid.csv"), n = 5)

prepare_joinpoint_subtype <- function(code, label) {
  # read subtype files
  data_1 <- read.csv2(here("data", "raw_data", paste0(code, "_2013_2020.csv")))
  data_2 <- read.csv2(here("data", "raw_data", paste0(code, "_2021_2024.csv")))
  
  # merge periods and standardize age labels
  data_opioid <- bind_rows(data_1, data_2) |> 
    mutate(age = str_replace_all(age, "–", "-")) |> 
    filter(age != "85+")
  
  # keep only weights for ages present in the subtype and renormalize
  weights_sub <- weights_age |> 
    filter(age_group %in% unique(data_opioid$age)) |> 
    mutate(weight = weight / sum(weight))
  
  # merge weights
  data_opioid <- merge(data_opioid,weights_sub,
    by.x = "age",by.y = "age_group",all.x = TRUE)
  
  # aggregate by year and age
  year_age <- data_opioid |> 
    group_by(year, age, weight) |> 
    summarise(deaths = sum(deaths),
      population = sum(population),.groups = "drop") |> 
    mutate(rate_age = deaths / population,
      var_component = (weight^2) * (deaths / (population^2)))
  
  # annual age-adjusted rate and standard error
  joinpoint_input <- year_age |> 
    group_by(year) |> 
    summarise(age_adjusted_rate = sum(rate_age * weight) * 100000,
      variance = sum(var_component) * (100000^2),
      standard_error = sqrt(variance),.groups = "drop") |> 
    select(year, age_adjusted_rate, standard_error)
  
  # save clean csv
  write.csv(joinpoint_input,here("output", paste0("joinpoint_input_", label, ".csv")),
    row.names = FALSE,quote = FALSE)
  
  return(joinpoint_input)}

#testing - test ok
t401_joinpoint <- prepare_joinpoint_subtype("T40.1", "T40_1_heroin")

t401_joinpoint
readLines(here("output", "joinpoint_input_T40_1_heroin.csv"), n = 5)

#generate others
t402_joinpoint <- prepare_joinpoint_subtype("T40.2", "T40_2_natural_semisynthetic")
t403_joinpoint <- prepare_joinpoint_subtype("T40.3", "T40_3_methadone")
t404_joinpoint <- prepare_joinpoint_subtype("T40.4", "T40_4_synthetic_other_than_methadone")
t406_joinpoint <- prepare_joinpoint_subtype("T40.6", "T40_6_other_unspecified_narcotics")
#check
readLines(here("output", "joinpoint_input_T40_2_natural_semisynthetic.csv"), n = 3)
readLines(here("output", "joinpoint_input_T40_3_methadone.csv"), n = 3)
readLines(here("output", "joinpoint_input_T40_4_synthetic_other_than_methadone.csv"), n = 3)
readLines(here("output", "joinpoint_input_T40_6_other_unspecified_narcotics.csv"), n = 3)

#summary joinpoint any opioid 
joinpoint_summary_any <- tibble(
  opioid_type = c("Any opioid", "Any opioid"),
  joinpoints_n = c(1, 1),
  joinpoint_year = c("2022", "2022"),
  segment = c(1, 2),
  start_year = c(2013, 2022),
  end_year = c(2022, 2024),
  apc = c(12.9391, -18.8672),
  ci_lower = c(10.7317, -33.2249),
  ci_upper = c(19.1486, -2.3756),
  p_value = c("0.000400", "0.029594"),
  significant = c("Yes", "Yes"))

joinpoint_summary_any

#summary joinpoint T40.1 - heroin
joinpoint_summary_t401 <- tibble(
  opioid_type = c("T40.1 Heroin", "T40.1 Heroin", "T40.1 Heroin"),
  joinpoints_n = c(2, 2, 2),
  joinpoint_year = c("2016; 2020", "2016; 2020", "2016; 2020"),
  segment = c(1, 2, 3),
  start_year = c(2013, 2016, 2020),
  end_year = c(2016, 2020, 2024),
  apc = c(18.2567, -5.8022, -24.5820),
  ci_lower = c(13.2810, -7.9068, -28.4772),
  ci_upper = c(23.3424, -3.6399, -20.6587),
  p_value = c("<0.000001", "0.002000", "<0.000001"),
  significant = c("Yes", "Yes", "Yes"))

joinpoint_summary_t401

#summary joinpoint T40.2 - natural/semisynth opiods
joinpoint_summary_t402 <- tibble(
  opioid_type = c("T40.2 Natural and semisynthetic opioids",
                  "T40.2 Natural and semisynthetic opioids"),
  joinpoints_n = c(1, 1),
  joinpoint_year = c("2021", "2021"),
  segment = c(1, 2),
  start_year = c(2013, 2021),
  end_year = c(2021, 2024),
  apc = c(-0.0302, -15.9193),
  ci_lower = c(-2.1578, -30.6900),
  ci_upper = c(4.8330, -6.6561),
  p_value = c("0.876625", "<0.000001"),
  significant = c("No", "Yes"))

joinpoint_summary_t402

#summary joinpoin T40.3 - methadone
joinpoint_summary_t403 <- tibble(
  opioid_type = "T40.3 Methadone",
  joinpoints_n = 0,
  joinpoint_year = "None",
  segment = 1,
  start_year = 2013,
  end_year = 2024,
  apc = -0.0370,
  ci_lower = -1.7324,
  ci_upper = 1.7604,
  p_value = "0.985403",
  significant = "No")

joinpoint_summary_t403

#summary joinpoint T40.4 - synthetic opioid
joinpoint_summary_t404 <- tibble(
  opioid_type = c("T40.4 Synthetic opioids other than methadone",
                  "T40.4 Synthetic opioids other than methadone",
                  "T40.4 Synthetic opioids other than methadone"),
  joinpoints_n = c(2, 2, 2),
  joinpoint_year = c("2016; 2022", "2016; 2022", "2016; 2022"),
  segment = c(1, 2, 3),
  start_year = c(2013, 2016, 2022),
  end_year = c(2016, 2022, 2024),
  apc = c(84.6386, 20.4012, -20.7080),
  ci_lower = c(40.1212, 15.6465, -35.1724),
  ci_upper = c(497.4677, 28.3578, -4.8482),
  p_value = c("<0.000001", "<0.000001", "0.010798"),
  significant = c("Yes", "Yes", "Yes"))

joinpoint_summary_t404

#summary joinpoin T40.6 - other 
joinpoint_summary_t406 <- tibble(
  opioid_type = "T40.6 Other and unspecified narcotics",
  joinpoints_n = 0,
  joinpoint_year = "None",
  segment = 1,
  start_year = 2013,
  end_year = 2024,
  apc = -9.5901,
  ci_lower = -13.4876,
  ci_upper = -7.3317,
  p_value = "<0.000001",
  significant = "Yes")

joinpoint_summary_t406

#bind all
joinpoint_summary_all <- bind_rows(
  joinpoint_summary_any,
  joinpoint_summary_t401,
  joinpoint_summary_t402,
  joinpoint_summary_t403,
  joinpoint_summary_t404,
  joinpoint_summary_t406)

#save 
write.csv(joinpoint_summary_all,here("output", "joinpoint_summary_all.csv"),
      row.names = FALSE,quote = FALSE)
