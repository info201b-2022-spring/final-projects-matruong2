library(dplyr)
library(tidyverse)
library(ggplot2)
library(usmap)

# Chart of US Map

US_data <- read.csv("data/suicide_mortality.csv")
WHO_data <- read.csv("data/who_suicide_statistics.csv")
colnames(US_data)[2] <- "state"

# Got rid of commas

US_data$DEATHS <- str_remove_all(US_data$DEATHS, ",") 

average_rates <- US_data %>% 
  group_by(state) %>% 
  summarize( avg_rate = mean(RATE, na.rm = TRUE),
             avg_death = mean(DEATHS, na.rm = TRUE))
  
average_rates

# shows average rates over all the data in the state across multiple years
plot_usmap(data = average_rates, values = "avg_rate") +
  labs(title = "Average Death Rates by State",
       subtitle = "Data from 2005, 2014-2016") +
  scale_fill_continuous(low = "white", high = "chartreuse4")


