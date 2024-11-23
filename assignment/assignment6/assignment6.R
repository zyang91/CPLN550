library(tidyverse)

setwd("/Users/apple/Desktop/CPLN550/Data")

load("TS2.1TimeSeriesOpExpSvcModeTOS2021.Rda")

NTD19<- NTD.ts %>%
  filter(Year==2019)

#1.Estimate the average of the following indicators 
#for the nation’s light rail (LR), heavy rail (HR), 
#commuter rail (CR), and bus (MB) systems in 2019 

# a.Passenger miles (PMT) per unlinked passenger trip (UPT)
NTD19<-NTD19 %>%
  filter(PMT!=0)

NTD19_a<- NTD19 %>%
  group_by(Mode) %>%
  summarise(mean_ratio = mean(PMT / UPT))

#b.	Passenger miles per directional route mile (DRM)
NTD19_b<- NTD19 %>%
  group_by(Mode) %>%
  summarise(mean_ratio = mean(PMT / DRM))

#c.	Fare revenue per passenger mile 
NTD19_c<- NTD19 %>%
  group_by(Mode) %>%
  summarise(mean_ratio = mean(FARES/ PMT))

#d.	Fare recovery ratio 
NTD19_d<- NTD19 %>%
  group_by(Mode) %>%
  summarise(mean_ratio = mean(FARES / OPEXP_TOTAL))

#e.	Average speed 
NTD19_e<- NTD19 %>%
  group_by(Mode) %>%
  summarise(mean_ratio =mean(VRM / VRH))

#2.Calculate the fare recovery ratio for all nation’s light rail, heavy rail, commuter rail, and bus systems in 2019
# (i.e., the average for the mode instead of the average fare recovery ratio of the different transit agencies.) 
#Compare this figure to the fare recovery ratios estimated in question 1 (average by agency). 
#Discuss any differences. 

NTD19_2<- NTD19 %>%
  group_by(Mode) %>%
  summarise(total_fare_reverse= sum(FARES)/sum(OPEXP_TOTAL))

#3.The Los Angeles Metropolitan Area has multiple providers of public transportation.
# Plot the fare recovery ratio for the Los Angeles MTA (TRS ID 90154) 
# and the Southern California Regional Rail Authority (TRS ID 90151) 
# for light rail, heavy rail, commuter rail, and bus from 2002 to 2021.

LA<- NTD.ts %>%
  filter(Year>=2002 & Year<=2021) %>%
  filter(City=="Los Angeles") %>%
  filter(VRM>0)%>%
  filter(NTD.ID==90154 | NTD.ID==90151) %>%
  filter(Mode=="LR" | Mode=="HR" | Mode=="CR" | Mode=="MB")

LA<- LA %>%
  mutate(FRR= FARES / OPEXP_TOTAL)

LA$Mode <- as.character(LA$Mode)
LA$Mode[LA$Mode == "MB" & LA$Service == "PT"] <- "MB_PT"
LA$Mode[LA1$Mode == "MB" & LA$Service == "DO"] <- "MB_DO"

ggplot(LA,  aes(x = Year, y = FRR ,  color = Mode)) + 
  geom_line() +
  ggtitle("Fare Recovery Ratio for Los Angeles MTA and Southern California Regional Rail Authority") +
  xlab("Year") +
  ylab("PMT") 
