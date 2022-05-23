library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)

US_data <- read.csv("data/suicide_mortality.csv")

US_data_grp_state <- group_by(US_data, STATE) %>%
  summarize(RATE = mean(RATE))

data <- data.frame(
  id=seq(1,50),
  individual=paste( "STATE", seq(1,50), sep=""),
  value=sample( seq(10,100), 50, replace=T)
)


label_data <- data


number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar    


label_data$hjust<-ifelse( angle < -90, 1, 0)

label_data$angle<-ifelse(angle < -90, angle+180, angle)



p <- ggplot(data = US_data_grp_state, aes(x=STATE, y=RATE)) +       
  
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  ggtitle("Deaths per 100,000")+

  coord_polar(start = 0) +
  
  geom_text(data=US_data_grp_state, aes(x = STATE, y = RATE, label = RATE), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
coord_polar(start = 0)

