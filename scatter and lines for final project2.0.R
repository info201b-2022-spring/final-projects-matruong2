library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
library(hrbrthemes)

#Data Cleaning

WorldSuicide <- read_csv("data/who_suicide_statistics.csv")


suicidebyyearUSA <- filter(
  WorldSuicide,
  country == "United States of America") %>%
  filter(year >= 2000)%>% 
  group_by(year) %>% 
  summarize(suicides_no = sum(suicides_no))%>%
  na.omit(WorldSuicide)

#to check the "cleaned up"
print(suicidebyyearUSA)

#Variables:
#suicidebyyearUSA(Used to make line/scatter)
#WorldSuicide(Raw data)

#R-graph-gallery used to create graph of scatter+line

scatterline <- ggplot(suicidebyyearUSA, aes(x = year, y = suicides_no)) +
  geom_point() +
  labs(x="Years", y="No.of Suicides", title = "No. of Suicides By Year") +
  geom_line(color="gray28") +
  geom_point(shape=23, color="#000000", fill="#261bcc", size=4) +
  theme_ipsum()

  
print(scatterline)