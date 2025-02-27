# setting work directory
setwd("C:/Users/zyang/OneDrive/文档/data")


# import the data
hh<- read.csv("1_Household_Public.csv")
per <- read.csv("2_Person_Public.csv")
veh <- read.csv("3_Vehicle_Public.csv")
trip <- read.csv("4_Trip_Public.csv")

# require packages
library(ggplot2)
library(dplyr)

# Question 1
head(trip)
subset(trip[c(2,3,4,15,28:38)], PERSON_ID== 10014001)

# Question 2
mode_share <- trip %>%
  filter(!is.na(MODE_AGG))%>%
  group_by(MODE_AGG)%>%
  summarise(MODE_AGG = n())%>%
  mutate(percentage = MODE_AGG/sum(MODE_AGG)*100)


# Question 3
test <- merge(trip,hh, by = "HH_ID")

table(test$MODE_AGG[test$H_COUNTY ==  42101])

round(table(trip$MODE_AGG[trip$H_COUNTY == 42101])/sum(table(trip$MODE_AGG[trip$H_COUNTY == 42101])), 3)

Philadelphia <- left_join(trip, hh, by = "HH_ID")
Philadelphia <- Philadelphia %>%
  filter(H_COUNTY==42101)
Phildelphia <- Philadelphia %>%
  group_by(MODE_AGG)%>%
  summarise(MODE_AGG = n())%>%
  mutate(percentage = MODE_AGG/sum(MODE_AGG)*100)

#Question 4
library(doBy)
weighted.mean(trip$MODE_AGG==1, trip$P_WEIGHT, na.rm = TRUE)
weighted.mean(trip$MODE_AGG==2, trip$P_WEIGHT, na.rm = TRUE)
weighted.mean(trip$MODE_AGG==3, trip$P_WEIGHT, na.rm = TRUE)
weighted.mean(trip$MODE_AGG==4, trip$P_WEIGHT, na.rm = TRUE)
