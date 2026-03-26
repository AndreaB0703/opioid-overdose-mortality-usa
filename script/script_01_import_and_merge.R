#load packs
library(tidyverse)
library(here)
library(janitor)
library(readr)

#get directory
getwd()
#list files
list.files()
list.files("data")
list.files("data/raw_data")

# read files folder raw_data
data1 <- read_csv2(here("data", "raw_data", "any_opioid_2013_2020.csv"))
data2 <- read_csv2(here("data", "raw_data", "any_opioid_2021_2024.csv"))
dplyr::glimpse(data1)
dplyr::glimpse(data2)

#Fix variable crude_rate only data1
data1<-data1 |> mutate(crude_rate=na_if(crude_rate, "Unreliable"), 
                       crude_rate=as.numeric(crude_rate))

#delete 11 deaths 85+ data2 = no 85+ data1
data2<-data2 |> filter(age !="85+")
#check
sort(unique(data2$age))

#read age_standard_weights file
weights_age <- read.csv2(here("data", "raw_data", "age_standard_weights.csv"))
#check
weights_age 
sort(unique(weights_age$age_group))

#delete 85+ weights_age 
weights_age <-weights_age |> filter(age_group !="85+")
#check
weights_age
sort(unique(weights_age$age_group))
#weight as numeric
weights_age$weight <- as.numeric(as.character(weights_age$weight))
#sum weights = 0.980246
sum(weights_age$weight)
str(weights_age)
#transform sum weights = 100
weights_age <- weights_age |> 
  mutate(weight = weight / sum(weight))
#check sum weights = 100 yes
sum(weights_age$weight)
str(weights_age)
#compare age data1, data2, weight_age 
sort(unique(data1$age))
sort(unique(data2$age))
sort(unique(weights_age$age_group))
#transform chr weight_age = chr data1 and data2
weights_age$age_group <- gsub("–", "-", weights_age$age_group)
#check
sort(unique(weights_age$age_group))
#merge weight to data1
data1 <- merge(data1, weights_age, by.x = "age", by.y = "age_group", all.x = TRUE)
#check
names(data1)
sum(is.na(data1$weight))
head(data1)
#merge weight to data2
data2 <- merge(data2, weights_age, by.x = "age", by.y = "age_group", all.x = TRUE)
#check
names(data2)
sum(is.na(data2$weight))
head(data2)
#merge data1 data2 = new file = opioid_any
opioid_any <- bind_rows(data1, data2)
#check
##dimension dataset 8117 rows 9 cols ok
dim(opioid_any)
##order years - check 2013 to 2024 ok
sort(unique(opioid_any$year))
##sum NA col weight = 0 = All lines have a weight Ok 
sum(is.na(opioid_any$weight))
#save file opiod_any folder "processed_data"
write.csv2(opioid_any,here("data", "processed_data", "opioid_any_2013_2024_clean.csv"),
           row.names = FALSE)
#check
list.files("data/processed_data")


######general information about deaths
# read data
opioid_any <- read.csv2(here("data", "processed_data", "opioid_any_2013_2024_clean.csv"))

# total deaths in the whole period
sum(opioid_any$deaths)
                       