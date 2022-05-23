library(dplyr)
library(tidyverse)
library(ggplot2)
library(usmap)

# Chart of US Map

US_data <- read.csv("data/suicide_mortality.csv")
WHO_data <- read.csv("data/who_suicide_statistics.csv")
colnames(US_data)[2] <- "state"

# Got rid of commas and convert to integer

US_data$DEATHS <- strtoi( str_remove_all(US_data$DEATHS, ",") )  

average_rates <- US_data %>% 
  group_by(state) %>% 
  summarize( avg_rate = mean(RATE, na.rm = TRUE),
             avg_death = mean(DEATHS, na.rm = FALSE))
  
average_rates

# shows average death rates per year
plot_usmap(data = average_rates, values = "avg_rate") +
  labs(title = "Average Death Rates by State per year",
       subtitle = "Data from 2005, 2014-2020") +
  scale_fill_continuous(low = "white", high = "chartreuse4")

# shows average deaths per year
plot_usmap(data = average_rates, values = "avg_death") +
  labs(title = "Average Total Deaths by State per year",
       subtitle = "Data from 2005, 2014-2020") +
  scale_fill_continuous(low = "white", high = "chartreuse4")

