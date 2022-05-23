library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)

US_data <- read.csv("data/suicide_mortality.csv")

US_data_grp_state <- group_by(US_data, STATE)

min_deaths_by_state <- select(
  merge(
    summarise(
      US_data_grp_state, DEATHS = min(DEATHS)), 
    US_data_grp_state),
  STATE, DEATHS, YEAR
)

max_deaths_by_state <- select(
  merge(
    summarise(
      US_data_grp_state, DEATHS = max(DEATHS)), 
    US_data_grp_state),
  STATE, DEATHS, YEAR
)

min_max_by_state <- merge(max_deaths_by_state, min_deaths_by_state, by = c("STATE"))

State_Table <- setDT(min_max_by_state)

names(State_Table)[names(State_Table) == 'DEATHS.x'] <- 'Minimum Deaths'
names(State_Table)[names(State_Table) == 'YEAR.x'] <- 'Year of Minimum Deaths'
names(State_Table)[names(State_Table) == 'DEATHS.y'] <- 'Maximum Deaths'
names(State_Table)[names(State_Table) == 'YEAR.y'] <- 'Year of Maximum Deaths'

