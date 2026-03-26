
# observed burden and demographic profile
# opioid overdose mortality, USA, 2013-2024
#
# objective:
# describe the observed number of deaths in the analytic dataset
# by year, sex, age group, race, and opioid subtype
#
# note:
# these counts reflect the analytic dataset and may be affected
# by suppression or truncation of low-count cells in the source data


# load packages
library(tidyverse)
library(here)

# read data
opioid_any <- read.csv2(here("data", "processed_data", "opioid_any_2013_2024_clean.csv"))

# check data
glimpse(opioid_any)
dim(opioid_any)

sort(unique(opioid_any$age))
table(opioid_any$age, useNA = "ifany")

# recode race for communication
opioid_any <- opioid_any |> 
  mutate(race_3cat = case_when(race == "White" ~ "White",
      race == "Black or African American" ~ "Black",TRUE ~ "Other"))

# total observed deaths in analytic dataset
total_observed_deaths <- sum(opioid_any$deaths)

# observed deaths by year
deaths_by_year <- opioid_any |> 
  group_by(year) |> 
  summarise(total_deaths = sum(deaths),.groups = "drop")

# observed deaths by year and sex
deaths_by_year_sex <- opioid_any |> 
  group_by(year, sex) |> 
  summarise(total_deaths = sum(deaths),.groups = "drop") |> 
  group_by(year) |> 
  mutate(proportion = total_deaths / sum(total_deaths)) |> 
  ungroup()

# observed deaths by age group and sex
deaths_by_age_sex <- opioid_any |> 
  group_by(age, sex) |> 
  summarise(total_deaths = sum(deaths),.groups = "drop")

# observed deaths by race
deaths_by_race <- opioid_any |> 
  group_by(race_3cat) |> 
  summarise(total_deaths = sum(deaths),.groups = "drop") |> 
  mutate(proportion = total_deaths / sum(total_deaths))

total_observed_deaths
deaths_by_year
deaths_by_year_sex
deaths_by_age_sex
deaths_by_race

#graph observed deaths by year
# graph 1
# observed opioid overdose deaths by year

graph_observed_deaths_year <- ggplot(deaths_by_year,
  aes(x = year, y = total_deaths))+
  geom_line(linewidth = 1, color = "#4C78A8") +
  geom_point(size = 2, color = "#4C78A8") +
  geom_text(aes(label = scales::label_comma()(total_deaths)),
    vjust = -0.5, size = 3) +
  scale_x_continuous(breaks = 2013:2024) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Observed opioid overdose deaths by year",
    subtitle = "Analytic dataset, United States, 2013-2024",
    x = "Year",y = "Observed number of deaths") +
   theme_minimal()

graph_observed_deaths_year

#save
ggsave(filename = here("figure", "graph_observed_deaths_year.png"),
  plot = graph_observed_deaths_year,dpi = 600,width = 6,height = 4.5)

# graph 2
# proportion of observed opioid overdose deaths by sex and year

graph_sex_by_year <- ggplot(deaths_by_year_sex,
  aes(x = year, y = proportion, fill = sex)) +
  geom_col(width = 0.7) + geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)),
    position = position_stack(vjust = 0.5), size = 3) +
  scale_x_continuous(breaks = 2013:2024) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Female" = "#9ECAE1", "Male" = "#FDD0A2")) +
  labs( title = "Proportion of observed opioid overdose deaths by sex",
    subtitle = "Analytic dataset, United States, 2013-2024",
    x = "Year",y = "Observed deaths (%)",fill = "Sex"  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

graph_sex_by_year
#save
ggsave(filename = here("figure", "graph_observed_deaths_sex_year.png"),
  plot = graph_sex_by_year,dpi = 600,width = 6,height = 4.5)

# graph 3
# observed opioid overdose deaths by age group and sex

graph_age_sex <- ggplot(deaths_by_age_sex,
  aes(x = age, y = total_deaths, fill = sex)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = scales::label_comma() (total_deaths)),
            position = position_dodge(width = 0.8),
            vjust = -0.3,size = 3) +  
  scale_fill_manual(values = c("Female" = "#9ECAE1", "Male" = "#FDD0A2")) +
  scale_y_continuous(labels=scales::label_comma())+
  labs(title = "Observed opioid overdose deaths by age group and sex",
    subtitle = "Analytic dataset, United States, 2013-2024",
    x = "Age group",y = "Observed number of deaths", fill = "Sex") +
  theme_minimal()

graph_age_sex
#save
ggsave(filename = here("figure", "graph_observed_deaths_age_sex.png"),
  plot = graph_age_sex,dpi = 600, width = 7,height = 4.5)

# graph 4
# proportion of observed opioid overdose deaths by race
graph_race <- ggplot(deaths_by_race |> 
    mutate(race_3cat = factor(race_3cat, levels = c("White", "Black", "Other"))),
  aes(x = race_3cat, y = proportion, fill = race_3cat)) +
  geom_col(width = 0.7) +  
  geom_text( aes(label = scales::percent(proportion, accuracy = 0.1)),
             vjust = -0.5,size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("White" = "#D9D9D9", "Black" = "#A6CEE3", "Other" = "#B2DF8A")) +
  labs(title = "Proportion of observed opioid overdose deaths by race",
    subtitle = "Analytic dataset, United States, 2013-2024",
    x = "Race", y = "Observed deaths (%)",fill = "Race")+
  theme_minimal() +
  theme(legend.position = "none")

graph_race
#save
ggsave(filename = here("figure", "graph_observed_deaths_race.png"),
  plot = graph_race,dpi = 600,width = 6,height = 4.5)

#opioid type 
##prepare dataset
# read subtype datasets
t401_1 <- read.csv2(here("data", "raw_data", "T40.1_2013_2020.csv"))
t401_2 <- read.csv2(here("data", "raw_data", "T40.1_2021_2024.csv"))

t402_1 <- read.csv2(here("data", "raw_data", "T40.2_2013_2020.csv"))
t402_2 <- read.csv2(here("data", "raw_data", "T40.2_2021_2024.csv"))

t403_1 <- read.csv2(here("data", "raw_data", "T40.3_2013_2020.csv"))
t403_2 <- read.csv2(here("data", "raw_data", "T40.3_2021_2024.csv"))

t404_1 <- read.csv2(here("data", "raw_data", "T40.4_2013_2020.csv"))
t404_2 <- read.csv2(here("data", "raw_data", "T40.4_2021_2024.csv"))

t406_1 <- read.csv2(here("data", "raw_data", "T40.6_2013_2020.csv"))
t406_2 <- read.csv2(here("data", "raw_data", "T40.6_2021_2024.csv"))

# bind each subtype
t401 <- bind_rows(t401_1, t401_2) |> 
  mutate(opioid_type = "Heroin")

t402 <- bind_rows(t402_1, t402_2) |> 
  mutate(opioid_type = "Natural and semisynthetic")

t403 <- bind_rows(t403_1, t403_2) |> 
  mutate(opioid_type = "Methadone")

t404 <- bind_rows(t404_1, t404_2) |> 
  mutate(opioid_type = "Synthetic other than methadone")

t406 <- bind_rows(t406_1, t406_2) |> 
  mutate(opioid_type = "Other and unspecified narcotics")

# combine
opioid_subtypes <- bind_rows(t401, t402, t403, t404, t406)

# table
deaths_by_year_opioid <- opioid_subtypes |> 
  group_by(year, opioid_type) |> 
  summarise(total_deaths = sum(deaths),.groups = "drop")

deaths_by_year_opioid

# graph 5
# observed opioid overdose deaths by opioid subtype and year

graph_observed_deaths_opioid <- ggplot(
  deaths_by_year_opioid,
  aes(x = year, y = total_deaths, color = opioid_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2013:2024) +
  scale_y_continuous(labels=scales::label_comma())+
  labs(title = "Observed opioid overdose deaths by opioid subtype",
    subtitle = "Analytic dataset, United States, 2013-2024",
    x = "Year",y = "Observed number of deaths",color = "Opioid subtype") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

graph_observed_deaths_opioid
#save
ggsave(filename = here("figure", "graph_observed_deaths_opioid.png"),
       plot = graph_observed_deaths_opioid,dpi = 600,width = 6,height = 4.5)