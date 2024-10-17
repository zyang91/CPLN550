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
total_weighted_travel_time/60
average_weighted_travel_time <- total_weighted_travel_time/total_weighted

#Question 7
biketrip <- trip %>%
  filter(MODE_AGG == 2) %>%
  mutate(weighted_travel_time= Model_TravTime*P_WEIGHT)

total_bike <- sum(biketrip$P_WEIGHT, na.rm = TRUE)
total_bike_time <- sum(biketrip$weighted_travel_time, na.rm = TRUE)
total_bike_time/total_weighted_travel_time*100
average_bike_time <- total_bike_time/total_bike