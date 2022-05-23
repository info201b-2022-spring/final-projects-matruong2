library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
library(data.table)

WorldSuicide <- read_csv("who_suicide_statistics.csv")

suicidebyyearUSA <- filter(
  WorldSuicide,
  country == "United States of America") %>%
  filter(year >= 2000)%>% 
  group_by(year) %>% 
  summarize(suicides_no = sum(suicides_no))%>%
  na.omit(WorldSuicide)

suicidebyyearUSAtable <- setDT(suicidebyyearUSA) 

names(suicidebyyearUSAtable)[names(suicidebyyearUSAtable) == 'suicides_no'] <- '# of Suicides'
names(suicidebyyearUSAtable)[names(suicidebyyearUSAtable) == 'year'] <- 'Year'

