library(dplyr)
library(tidyr)

#Sorting/Cleaning
HR_data <- read.csv("C:/Users/nnotc/Desktop/info_stuff/final-projects-matruong2/data/Human_Resources.csv")

US_data <- read.csv("C:/Users/nnotc/Desktop/info_stuff/final-projects-matruong2/data/suicide_mortality.csv")

WHO_data <- read.csv("C:/Users/nnotc/Desktop/info_stuff/final-projects-matruong2/data/who_suicide_statistics.csv")

HR_US_data <- filter(HR_data, Country == "United States of America")

WHO_DATA_US <- filter(WHO_data, country == "United States of America")

US_data_grp_state <- group_by(US_data, STATE)

US_data_grp_year <- group_by(US_data, YEAR)

min_deaths_by_state <- select(
  merge(
    summarise(
      US_data_grp_state, DEATHS = min(DEATHS)), 
        US_data_grp_state),
  STATE, DEATHS, YEAR
)

min_deaths_by_year <- select(
  merge(
    summarise(
      US_data_grp_year, DEATHS = min(DEATHS)), 
    US_data_grp_state),
  STATE, DEATHS, YEAR
)

min_rate_by_state <- select(
  merge(
    summarise(
      US_data_grp_state, RATE = min(RATE)), 
    US_data_grp_state),
  STATE, RATE, YEAR
)

min_rate_by_year <- select(
  merge(
    summarise(
      US_data_grp_year, RATE = min(RATE)), 
    US_data_grp_state),
  STATE, RATE, YEAR
)

max_deaths_by_state <- select(
  merge(
    summarise(
      US_data_grp_state, DEATHS = max(DEATHS)), 
    US_data_grp_state),
  STATE, DEATHS, YEAR
)

min_deaths_by_year <- select(
  merge(
    summarise(
      US_data_grp_year, DEATHS = max(DEATHS)), 
    US_data_grp_state),
  STATE, DEATHS, YEAR
)

max_rate_by_state <- select(
  merge(
    summarise(
      US_data_grp_state, RATE = max(RATE)), 
    US_data_grp_state),
  STATE, RATE, YEAR
)

min_rate_by_year <- select(
  merge(
    summarise(
      US_data_grp_year, RATE = max(RATE)), 
    US_data_grp_state),
  STATE, RATE, YEAR
)
