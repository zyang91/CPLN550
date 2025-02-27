#set working dirctory

setwd("/Users/apple/Desktop/CPLN550")

#required packages
library(tidyverse)

#load required data
hh <- read.csv("data/1_Household_Public.csv")
per <- read.csv("data/2_Person_Public.csv")
veh <- read.csv("data/3_Vehicle_Public.csv")
trip <- read.csv("data/4_Trip_Public.csv")

#Question 6
trip <- trip %>%
  mutate(weighted_travel_time= Model_TravTime*P_WEIGHT)

total_weighted_travel_time <- sum(trip$weighted_travel_time, na.rm = TRUE)

total_weighted <- sum(trip$P_WEIGHT, na.rm = TRUE)
thour<- total_weighted_travel_time/60
average_weighted_travel_time <- total_weighted_travel_time/total_weighted

#Question 7
biketrip <- trip %>%
  filter(MODE_AGG == 2) %>%
  mutate(weighted_travel_time= Model_TravTime*P_WEIGHT)

total_bike <- sum(biketrip$P_WEIGHT, na.rm = TRUE)
total_bike_time <- sum(biketrip$weighted_travel_time, na.rm = TRUE)
total_bike_time/total_weighted_travel_time*100
average_bike_time <- total_bike_time/total_bike

# Question 10
hh_per <- left_join(hh, per, by = "HH_ID")
hh_per <- hh_per %>%
  filter(!is.na(INCOME)) %>%
  filter(INCOME != 98| INCOME != 99) %>%
  mutate(income = case_when(
    INCOME == 1 ~ 5000,
    INCOME == 2 ~ 22500,
    INCOME == 3 ~ 30000,
    INCOME == 4 ~ 42500,
    INCOME == 5 ~ 62500,
    INCOME == 6 ~ 87500,
    INCOME == 7 ~ 125000,
    INCOME == 8 ~ 175000,
    INCOME == 9 ~ 225000,
    INCOME == 10 ~ 300000,

  ))
hh_per <- hh_per %>%
  filter(!is.na(HOURS)) %>%
  filter(HOURS != 988| HOURS != 998 | HOURS !=1000) %>%
  mutate(wage= income/(HOURS*48))

hh_per <- hh_per %>%
  filter(!is.na(wage))
mean(hh_per$wage)
hh_per_sub <- hh_per %>%
  select(income, HOURS, wage)%>%
  filter(HOURS <= 300) %>%
  mutate(work_hour= HOURS*48)

mean(hh_per_sub$wage)

total_travel_cost <-mean(hh_per$wage)/2*thour
total_travel_cost

ggplot(hh_per_sub, aes(x=income, y=HOURS))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Income vs Wage",
       x = "Income",
       y = "Wage")
