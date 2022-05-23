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
  scale_fill_continuous(low = "white", high = "purple3")

# shows average total deaths per year
plot_usmap(data = average_rates, values = "avg_death") +
  labs(title = "Average Total Deaths by State per year",
       subtitle = "Data from 2005, 2014-2020") +
  scale_fill_continuous(low = "white", high = "purple3")

# The purpose of including the 2 maps is to compare the average death rate by 
# state per year to the average total deaths per year. This shows how a higher
# average death rate from suicide does not correlate to a higher average total 
# death rate. The reason for this is that states with a lower population will
# have a higher average suicide death rate compared to states with a higher 
# population because if they have the same amount of total deaths from suicide,
# then the state with the lower population will have a higher death rate.
# For example, California has the highest average total death, but has one of
# the lowest average death rates by suicide since it has one of the highest
# state population in the country.
