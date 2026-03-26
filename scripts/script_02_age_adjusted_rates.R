# clean enviroment
rm(list = ls())
install.packages("maps")
# load packs
library(tidyverse)
library(here)
library(janitor)
library(readr)
library(maps)

# load file opiod_any
opioid_any <- read.csv2(here("data", "processed_data", "opioid_any_2013_2024_clean.csv"))

# check structure
glimpse(opioid_any)
names(opioid_any)

#age-specific rate
opioid_any <- opioid_any |> 
  mutate(rate_age_specific = deaths / population)

#check
glimpse(opioid_any)
summary(opioid_any$rate_age_specific)

#weighted contribution at each age to standarized rate
opioid_any <- opioid_any |> 
  mutate(weighted_rate = rate_age_specific * weight)
#check
summary(opioid_any$weighted_rate)
head(opioid_any)

#age_adjusted_year rate
##each row has dozens of deaths
#step1 group by year, age, weight
age_year_agg <- opioid_any |> 
  group_by(year, age, weight) |> 
  summarise(deaths = sum(deaths),
    population = sum(population),.groups = "drop")
##step2 rate by age
age_year_agg <- age_year_agg |> 
  mutate(rate_age = deaths / population)

#step3 age-adjusted rate by year
age_adjusted_year <- age_year_agg |> 
  group_by(year) |> 
  summarise(age_adjusted_rate = sum(rate_age * weight) * 100000,
    .groups = "drop")
#check OK
age_adjusted_year

#save file age_adjusted_rate_year folder "output"
write.csv2(age_adjusted_year, here("output", "age_adjusted_rate_year.csv"),
           row.names = FALSE)
#check
list.files("output")

#national graph
graph_national <- ggplot(age_adjusted_year, aes(x = year, y = age_adjusted_rate)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text(aes(label = round(age_adjusted_rate, 1)), vjust = -1.8, size = 3) +
  scale_x_continuous(breaks = age_adjusted_year$year) +
  labs(title = "Age-adjusted opioid overdose mortality rate. USA, 2013-2024",
    x = "Year",y = "Age-adjusted rate per 100,000") +
  theme_minimal()

graph_national

#save national graph
ggsave(filename = here("figure", "national_graph.png"),
  plot = graph_national,dpi = 600,  width = 4,height = 3.5)

#temporal graph sex year age
##step1 aggregated table
age_year_sex_agg <- opioid_any |> 
  group_by(year, sex, age, weight) |> 
  summarise(deaths = sum(deaths),
    population = sum(population),.groups = "drop")
##step2 check
head(age_year_sex_agg)
dim(age_year_sex_agg)
sort(unique(age_year_sex_agg$sex))
##step3 specific rate by age and sex
age_year_sex_agg <- age_year_sex_agg |> 
  mutate(rate_age = deaths / population)
#check
head(age_year_sex_agg)
summary(age_year_sex_agg$rate_age)

##step4 age-adjusted by sex
age_adjusted_year_sex <- age_year_sex_agg |> 
  group_by(year, sex) |> 
  summarise(age_adjusted_rate = sum(rate_age * weight) * 100000,.groups = "drop")

#check 
age_adjusted_year_sex

#save file age_adjusted_year_sex folder "output"
write.csv2(age_adjusted_year_sex, here("output", "age_adjusted_year_sex.csv"),
           row.names = FALSE)
#check
list.files("output")

#step5 graph_sex
graph_sex <- ggplot(age_adjusted_year_sex,
                    aes(x = year, y = age_adjusted_rate, group = sex, color = sex)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text(aes(label = round(age_adjusted_rate, 1)),vjust = -0.7,hjust = 1.4,
    size = 3,show.legend = FALSE) +
  scale_x_continuous(breaks = sort(unique(age_adjusted_year_sex$year))) +
  labs(title = "Age-adjusted opioid overdose mortality rate by sex. USA, 2013-2024",
    x = "Year",y = "Age-adjusted rate per 100,000",color = "Sex") +
  theme_minimal()

graph_sex

#save graph_sex
ggsave(filename = here("figure", "graph_sex.png"),
       plot = graph_sex,dpi = 600,  width = 4,height = 3.5)

#rate by race
sort(unique(opioid_any$race))
table(opioid_any$race)

##step1 unite asian/asian pacific
opioid_any <- opioid_any |> 
  mutate(race_group = case_when(race %in% c("Asian", "Asian or Pacific Islander") ~ "Asian / Pacific Islander",
      TRUE ~ race))
#check
table(opioid_any$race_group)
sort(unique(opioid_any$race_group))

##step2 aggregate table
age_year_race_agg <- opioid_any |> 
  group_by(year, race_group, age, weight) |> 
  summarise(deaths = sum(deaths),
    population = sum(population),.groups = "drop")
#check
head(age_year_race_agg)
dim(age_year_race_agg)
sort(unique(age_year_race_agg$race_group))

##step3 age race_group
age_year_race_agg <- age_year_race_agg |> 
  mutate(rate_age = deaths / population)

#check
head(age_year_race_agg <-age_year_race_agg)
summary(age_year_race_agg$rate_age)

##step4 age-adjusted by year and race_group
age_adjusted_year_race <- age_year_race_agg |> 
  group_by(year, race_group) |> 
  summarise(age_adjusted_rate = sum(rate_age * weight) * 100000,.groups = "drop")

age_adjusted_year_race

#check more than 1 race irregular (only last 4 years)
table(age_adjusted_year_race$race_group)

#filter only 2014/2024
race_bar_data <- age_adjusted_year_race |> 
  filter(year %in% c(2014, 2024),
         race_group != "More than one race")
#check
race_bar_data

#label for race
race_bar_data <- race_bar_data |> 
  mutate(race_label = case_when(
      race_group == "American Indian or Alaska Native" ~ "AI/AN",
      race_group == "Asian / Pacific Islander" ~ "Asian/PI",
      race_group == "Black or African American" ~ "Black",
      race_group == "White" ~ "White",
      TRUE ~ race_group))

#save file age_adjusted_year_race folder "output"
write.csv2(age_adjusted_year_race, here("output", "age_adjusted_year_race.csv"),
           row.names = FALSE)
#check
list.files("output")

#graph_race_bar
graph_race_bar <- ggplot(race_bar_data,
                         aes(x = race_label, y = age_adjusted_rate, fill = factor(year))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(age_adjusted_rate, 1)),position = position_dodge(width = 0.8),
            vjust = -0.3,size = 3) +
  labs(title = "Age-adjusted opioid overdose mortality rate by race, USA, 2014 vs 2024",
    x = "Race",y = "Age-adjusted rate per 100,000",fill = "Year") +  
  theme_minimal()

graph_race_bar

#save graph_race_bar
ggsave(filename = here("figure", "graph_race_bar.png"),
       plot = graph_race_bar,dpi = 600,  width = 4,height = 3.5)

#create a map 2013 2016 2020 2024
#step1 age-adjusted year and state
state_year_age_agg <- opioid_any |> 
  group_by(state, year, age, weight) |> 
  summarise(deaths = sum(deaths),
    population = sum(population),.groups = "drop")
     
#check
head(state_year_age_agg)
dim(state_year_age_agg)

#step2 age-adjusted year and state
state_year_age_agg <- state_year_age_agg |> 
  mutate(rate_age = deaths / population)

#check
head(state_year_age_agg)
summary(state_year_age_agg$rate_age)

#step3 age-adjusted year and state
state_year_age_adjusted <- state_year_age_agg |> 
  group_by(state, year) |> 
  summarise(age_adjusted_rate = sum(rate_age * weight) * 100000,.groups = "drop")

#check
head(state_year_age_adjusted)
dim(state_year_age_adjusted)
summary(state_year_age_adjusted$age_adjusted_rate)

#save file state_year_age_adjusted folder "output"
write.csv2(state_year_age_adjusted, here("output", "state_year_age_adjusted.csv"),
           row.names = FALSE)

#check
list.files("output")
#testing map
us_map <- map_data("state")
#check
head(us_map)
glimpse(us_map)

#match names data and names map
state_year_age_adjusted <- state_year_age_adjusted |> 
  mutate(region = tolower(state))
#check
head(state_year_age_adjusted)
sort(unique(state_year_age_adjusted$region))[1:10]

#4 years map
map_data_4years <- state_year_age_adjusted |> 
  filter(year %in% c(2013, 2016, 2020, 2024))
#check
table(map_data_4years$year)
head(map_data_4years)

#merge map and data
map_plot_data <- us_map |> 
  left_join(map_data_4years, by = "region")

#check years map
table(map_plot_data$year, useNA = "ifany")

#check 
summary(map_plot_data$age_adjusted_rate)

#map darkblue
map_4years_dark_blue <- ggplot(map_plot_data,
                           aes(x = long, y = lat, group = group, fill = age_adjusted_rate)) +
  geom_polygon(color = "white", linewidth = 0.1) +  coord_fixed(1.3) +
  facet_wrap(~ year) + 
  labs(title = "Age-adjusted opioid overdose mortality rate by state. USA",
    fill = "Rate per\n100,000") +
  theme_void() +
  theme(legend.position = "right",
    strip.text = element_text(size = 10, face = "bold"))

map_4years_dark_blue
#save map_4years_dark_blue
ggsave(filename = here("figure", "map_4_years_dark_blue.png"),
       plot = map_4years_dark_blue,dpi = 600,  width = 4,height = 3.5)

#create categories
map_plot_data <- map_plot_data |> 
  mutate(rate_cat = case_when(
      age_adjusted_rate < 10 ~ "<10",
      age_adjusted_rate >= 10 & age_adjusted_rate < 20 ~ "10-19.9",
      age_adjusted_rate >= 20 & age_adjusted_rate < 30 ~ "20-29.9",
      age_adjusted_rate >= 30 & age_adjusted_rate < 40 ~ "30-39.9",
      age_adjusted_rate >= 40 ~ "40+",
      TRUE ~ NA_character_))
table(map_plot_data$rate_cat, useNA = "ifany")

#map categories (high contrast)
map_4years_cat <- 
  ggplot( map_plot_data,aes(x = long, y = lat, group = group, fill = rate_cat)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  coord_fixed(1.3) +facet_wrap(~ year) +
  labs(title = "Age-adjusted opioid overdose mortality rate by state, USA - 2013, 2016, 2020, 2024",
    fill = "Rate per\n100,000") +
  theme_void() +
  theme(legend.position = "right",strip.text = element_text(size = 10, face = "bold"))

map_4years_cat

#save map_4years_cat
ggsave(filename = here("figure", "map_4years_cat.png"),
       plot = map_4years_cat,dpi = 600,  width = 4,height = 3.5)

#map categories elegant blue
map_4years_elegant_blue <- 
  ggplot(map_plot_data,aes(x = long, y = lat, group = group, fill = age_adjusted_rate)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  coord_fixed(1.3) +facet_wrap(~ year) + 
  scale_fill_gradient(low = "lightblue",high = "darkblue") +
  labs(title = "Age-adjusted opioid overdose mortality rate by state, USA - 2013, 2016, 2020, 2024",
    fill = "Rate per\n100,000") +
  theme_void() +
  theme(legend.position = "right",strip.text = element_text(size = 10, face = "bold"))

map_4years_elegant_blue

#save map_4years_elegant_blue
ggsave(filename = here("figure", "map_4years_elegant_blue.png"),
       plot = map_4years_elegant_blue,dpi = 600,  width = 4,height = 3.5)

#unite years age
age_year_only_agg <- opioid_any |> 
  group_by(year, age) |> 
  summarise(deaths = sum(deaths),
    population = sum(population),.groups = "drop")

#check
head(age_year_only_agg)
dim(age_year_only_agg)
sort(unique(age_year_only_agg$age))

#specific rate by age 100,000 (not age-adjusted)
age_year_only_agg <- age_year_only_agg |> 
  mutate(rate_per_100k = (deaths / population) * 100000)

#check
head(age_year_only_agg)
summary(age_year_only_agg$rate_per_100k)

#save file age_year_only_agg folder "output" (not age-adjusted)
write.csv2(age_year_only_agg, here("output", "age_year_only_agg.csv"),
           row.names = FALSE)

#check
list.files("output")
#graph by age
graph_age <- ggplot(age_year_only_agg,
                    aes(x = year, y = rate_per_100k, group = age, color = age)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = sort(unique(age_year_only_agg$year))) +
  labs(title = "Opioid overdose mortality rate by age group, USA. 2013-2024",
    x = "Year", y = "Rate per 100,000", color = "Age group") +
  theme_minimal()

graph_age

#save graph_age
ggsave(filename = here("figure", "graph_age.png"),
       plot = graph_age,dpi = 600,  width = 4,height = 3.5)


