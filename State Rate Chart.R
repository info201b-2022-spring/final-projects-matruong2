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

# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
label_data <- data

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #


# Start the plot
p <- ggplot(data = US_data_grp_state, aes(x=STATE, y=RATE)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  ggtitle("Deaths per 100,000")+
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=US_data_grp_state, aes(x = STATE, y = RATE, label = RATE), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
coord_polar(start = 0)

