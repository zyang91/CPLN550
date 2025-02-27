setwd("/Users/apple/Desktop/Data")

# Load the data
hh <- read.csv("1_Household_Public.csv")
per <- read.csv("2_Person_Public.csv")
veh <- read.csv("3_Vehicle_Public.csv")
trip <- read.csv("4_Trip_Public.csv")

# load packages
library(dplyr)
library(ggplot2)

# Question 1
sort(unique(hh$HH_ID))[sample(11:20,1)]

random <- hh %>%
  filter(HH_ID == 101162)

random_person <- per %>%
  filter(HH_ID == 101162)
# Question 2
random_trip <- trip %>%
  filter(HH_ID == 101162)

# Question 3
bike_trip <- trip %>%
  filter(MODE_AGG == 2)
bike_trip %>%
  summarise(total_trips=sum(P_WEIGHT, na.rm=TRUE))

# Question 4
## join the household and trip data
complet_trip <-left_join(hh, trip, by = "HH_ID")

H_trip <- complet_trip %>%
  filter(H_COUNTY== random$H_COUNTY)

H_trip_mode <- H_trip %>%
  filter(!is.na(MODE_AGG)) %>%
  group_by(MODE_AGG) %>%
  summarise(total_trips_by_mode=sum(P_WEIGHT, na.rm=TRUE))

H_tripPercentage <- H_trip_mode %>%
  mutate(percentage = total_trips_by_mode/sum(total_trips_by_mode)*100)


# Question 5
household <- left_join(hh, veh, by = "HH_ID")
 ## clean the data-removing income missing values
household <- household %>%
  filter(!INCOME %in% c(98, 99))
## Clean the data- removing the household do not have vehicle
household <- household %>%
  filter(TOT_VEH.y > 0)
## Clean the data- removing missing YEAR, changing the YEAR to Car age
household <- household %>%
  filter(!YEAR %in% c(9998, 9999))%>%
  mutate(VEHAGE= 2014- YEAR)

## plot boxplot
### Converted the income number into text
household <-household %>%
  mutate(income= case_when(INCOME==1 ~ "Under $10,000",
                           INCOME==2 ~ "$10,000-$24,999",
                           INCOME==3 ~ "$25,000-$34,999",
                           INCOME==4 ~ "$35,000-$49,999",
                           INCOME==5 ~ "$50,000-$74,999",
                           INCOME==6 ~ "$75,000-$99,999",
                           INCOME==7 ~ "$100,000-$149,999",
                           INCOME==8 ~ "$150,000-$199,999",
                           INCOME==9 ~ "$200,000-$249,999",
                           INCOME==10 ~ "$250,000 or more"))
household$income <- factor(household$income, levels = c("Under $10,000",
                                                        "$10,000-$24,999",
                                                        "$25,000-$34,999",
                                                        "$35,000-$49,999",
                                                        "$50,000-$74,999",
                                                        "$75,000-$99,999",
                                                        "$100,000-$149,999",
                                                        "$150,000-$199,999",
                                                        "$200,000-$249,999",
                                                        "$250,000 or more"))
ggplot(household, aes(x= income, y= VEHAGE))+
  geom_boxplot()+
  labs(title="Boxplot of Car Age by Household Income Level",
       x="Income level", y= "Vehicle Age")

### Without out lier plot
ggplot(household, aes(x = income, y = VEHAGE)) +
  geom_boxplot(outlier.shape = NA) +
  labs(
    title = "Boxplot of Car Age by Household Income Level",
    x = "Income level", y = "Vehicle Age")+
  ylim(0, 30)

# Question 6
modechoice<-left_join(hh, trip, by = "HH_ID")

## clean the data-removing income missing values
modechoice <- modechoice %>%
  filter(!INCOME %in% c(98, 99))
## Clean the data- removing missing MODE_AGG
modechoice <- modechoice %>%
  filter(!is.na(MODE_AGG))

## reorganize the MODE_AGG data into categorical variable
modechoice <- modechoice %>%
  mutate(mode= case_when(MODE_AGG==1 ~ "Walk",
                             MODE_AGG==2 ~ "Bike",
                             MODE_AGG==3 ~ "Private Vehicle",
                             MODE_AGG==4 ~ "Private Transit",
                             MODE_AGG==5 ~ "Public Transit",
                             MODE_AGG==6 ~ "School Bus",
                             MODE_AGG==7 ~ "Other"))

modechoice$mode <- factor(modechoice$mode, levels = c("Walk",
                                                      "Bike",
                                                      "Private Vehicle",
                                                      "Private Transit",
                                                      "Public Transit",
                                                      "School Bus",
                                                      "Other"))
## plot boxplot
ggplot(modechoice, aes(x= mode, y= INCOME, fill = mode))+
  geom_boxplot()+
  labs(title="Boxplot of Household Income by Mode Choice",
       x="Mode Choice", y= "Household Income")+
  theme(legend.position = "none")
