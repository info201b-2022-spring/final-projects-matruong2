library(dplyr)
library(tidyr)

#Sorting/Cleaning------------------------------------------------------------
#I filtered down the US and WHO data to only the years they both have data
#for, but we don't have to keep this. 
HR_data <- filter(
  read.csv("data/Human_Resources.csv"),
  Country == "United States of America") %>%
  rename(year = Year)
  

US_data <- read.csv("data/suicide_mortality.csv") %>%
  filter(YEAR == 2005 | YEAR == 2014 | YEAR == 2015 | 
           YEAR == 2016)
  US_data$DEATHS <- str_remove_all(US_data$DEATHS, ",") 
    

WHO_data <- filter(
  read.csv("data/who_suicide_statistics.csv"),
  country == "United States of America") %>%
  filter(year == 2005 | year == 2014 | year == 2015 | 
           year == 2016 | year == 2017 | year == 2018 | 
           year == 2019 | year == 2020)

US_data_grp_state <- group_by(US_data, STATE)

US_data_grp_year <- group_by(US_data, YEAR) #%>%
  #summarise() %>%
  #print()

WHO_2016 <- WHO_data %>%
  filter(year == 2016) 

US_2016 <- US_data %>%
  filter(YEAR == 2016) %>%
  rename(year = YEAR)



#Making the list, Checking it twice------------------------------------------------------------

summary <- list()
summary$num_of_unique_years_observed <- nrow(
  summarise(US_data_grp_year)
) 
summary$unqiue_years_observed <- summarise(US_data_grp_year) 
  summary$unqiue_years_observed <- summary$unqiue_years_observed[["YEAR"]]
#US data stats------------------------------------------------------------
  #I left these as dataframe in the list, can change later
summary$min_deaths_by_state <- select(
  merge(
    summarise(
      US_data_grp_state, DEATHS = min(DEATHS)), 
        US_data_grp_state),
  STATE, DEATHS, YEAR
)

summary$min_deaths_by_year <- select(
  merge(
    summarise(
      US_data_grp_year, DEATHS = min(DEATHS)), 
    US_data_grp_state),
  STATE, DEATHS, YEAR
)

summary$min_rate_by_state <- select(
  merge(
    summarise(
      US_data_grp_state, RATE = min(RATE)), 
    US_data_grp_state),
  STATE, RATE, YEAR
)

summary$min_rate_by_year <- select(
  merge(
    summarise(
      US_data_grp_year, RATE = min(RATE)), 
    US_data_grp_state),
  STATE, RATE, YEAR
)

summary$max_deaths_by_state <- select(
  merge(
    summarise(
      US_data_grp_state, DEATHS = max(DEATHS)), 
    US_data_grp_state),
  STATE, DEATHS, YEAR
)

summary$min_deaths_by_year <- select(
  merge(
    summarise(
      US_data_grp_year, DEATHS = max(DEATHS)), 
    US_data_grp_state),
  STATE, DEATHS, YEAR
)

summary$max_rate_by_state <- select(
  merge(
    summarise(
      US_data_grp_state, RATE = max(RATE)), 
    US_data_grp_state),
  STATE, RATE, YEAR
)

summary$min_rate_by_year <- select(
  merge(
    summarise(
      US_data_grp_year, RATE = max(RATE)), 
    US_data_grp_state),
  STATE, RATE, YEAR
)
#WHO data stats------------------------------------------------------------
  #Did not distinguish by year, can do later
summary$most_common_sex <- names(
  which.max(
    table(WHO_data$sex)
  )
)

summary$most_common_age <- names(
  which.max(
    table(WHO_data$age)
  )
)
#HR data stats------------------------------------------------------------
summary$psychiatrists_per_100k <- round(
  HR_data$Psychiatrists[[1]]
)

summary$nurses_per_100k <- round(
  HR_data$Nurses[[1]]
)

summary$soc_workers_per_100k <- round(
  HR_data$Social_workers[[1]]
)

summary$psychologists_per_100k <- round(
  HR_data$Psychologists[[1]]
)

