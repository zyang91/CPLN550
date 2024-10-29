library(tidyverse)

setwd("/Users/apple/Desktop/CPLN550/Data")

hh <- read.csv("1_Household_Public.csv")
per <- read.csv("2_Person_Public.csv")
veh <- read.csv("3_Vehicle_Public.csv")
trip <- read.csv("4_Trip_Public.csv")
load("data.Rda")

# 1.	Using the 2012 Philadelphia household travel survey, plot a histogram of the total number of trips people made (P_TOT_TRIPS). 
# Describe the distribution of trip-making.
ggplot(per, aes(x=P_TOT_TRIPS))+ 
  geom_histogram(binwidth= 1, fill="blue", color="black") + 
  labs(title="Histogram of Total Number of Trips People Made", x="Total Number of Trips", y="Frequency")

hist(per$P_TOT_TRIPS,
     breaks = 20,  # Adjust the number of breaks as needed
     col = "skyblue",
     border = "black",
     main = "Distribution of Total Trips in Philadelphia Travel Survey",
     xlab = "Total Number of Trips",
     ylab = "Frequency")

# 2.	Create a new variable that equals 1 if a person did not take any trips 
# (hint: dat$newvariable <- as.integer(dat$P_WEIGHT == 0)). How many people in the sample took no trips? 
# Summarize the race, age, and income of those who took trips on the survey day vs. those that did not.

per$no_trip <- as.integer(per$P_TOT_TRIPS == 0)
Per_no_trip<- per %>%
  select(no_trip)
sum(Per_no_trip$no_trip)

per_hh<- left_join(per, hh, by = "HH_ID")
per_hh <- per_hh %>%
  filter(INCOME != 98) %>%
  filter(INCOME != 99) %>%
  filter(AGECAT != 98) %>%
  filter(AGECAT != 99)
per_hh <-per_hh %>%
  mutate(age= case_when(AGECAT==1 ~ "Under 5",
                        AGECAT==2 ~ "6-12",
                        AGECAT==3 ~ "13-15",
                        AGECAT==4 ~ "16-17",
                        AGECAT==5 ~ "18-24",
                        AGECAT==6 ~ "25-34",
                        AGECAT==7 ~ "35-44",
                        AGECAT==8 ~ "45-54",
                        AGECAT==9 ~ "55-64",
                        AGECAT==10 ~ "65-74",
                        AGECAT==11 ~ "75-84",
                        AGECAT==12 ~ "85+"))
per_hh<-per_hh %>%
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
per_hh <- per_hh %>%
  filter(RACE!=98) %>%
  filter(RACE!=99)%>%
  filter(RACE!= 988)%>%
  filter(RACE!=999)
per_hh <- per_hh %>%
  mutate(race= case_when(RACE==1 ~ "White",
                         RACE==2 ~ "Black",
                         RACE==3 ~ "Hispanic",
                         RACE==4 ~ "Indian or Alaska Native",
                         RACE==5 ~ "Asian",
                         RACE==6 ~ "Hawaiian or Pacific Islander",
                         RACE==97 ~ "Other",
                         RACE==100 ~ "Multi-Racial",
                         ))

##  people who take trip age distribution
per_hh_travel <- per_hh %>%
  filter(no_trip ==0)
ggplot(per_hh_travel, aes(x=AGECAT))+
  geom_bar(position="dodge")+
  labs(title="Age Distribution of People Who Took Trips", x="Age Category", y="Frequency")

ggplot(per_hh_travel, aes(x=INCOME))+
  geom_bar(position="dodge")+
  labs(title="Income Distribution of People Who Took Trips", x="Income Category", y="Frequency")

ggplot(per_hh_travel, aes(x=race, fill=race))+
  geom_bar(position="dodge")+
  labs(title="Race Distribution of People Who Took Trips",x="RACE Category", y="Frequency")
##  people who did not take trip age distribution
per_hh_no_travel <- per_hh %>%
  filter(no_trip ==1)
ggplot(per_hh_no_travel, aes(x=AGECAT))+
  geom_bar(position="dodge")+
  labs(title="Age Distribution of People Who Did Not Take Trips", x="Age Category", y="Frequency")
ggplot(per_hh_no_travel, aes(x=INCOME))+
  geom_bar(position="dodge")+
  labs(title="Income Distribution of People Who Did Not Take Trips", x="Income Category", y="Frequency")
ggplot(per_hh_no_travel,aes(x=race, fill=race))+
  geom_bar(position="dodge")+
  labs(title="Race Distribution of People Who Did Not take Trips",x="Race Category", y="Frequency")
