

load("TS2.1TimeSeriesOpExpSvcModeTOS2021.Rda")

NTD.19 <- subset(NTD.ts, Year == 2019)


#NTD.19 <- subset(NTD.19, Service == "DO") #check

#a.	Passenger miles (PMT) per unlinked passenger trip (UPT)
mean(NTD.19$PMT[NTD.19$Mode == "LR" & NTD.19$PMT != 0]/NTD.19$UPT[NTD.19$Mode == "LR" & NTD.19$PMT != 0], na.rm = T)

mean(NTD.19$PMT[NTD.19$Mode == "HR" & NTD.19$PMT != 0]/NTD.19$UPT[NTD.19$Mode == "HR" & NTD.19$PMT != 0], na.rm = T)

mean(NTD.19$PMT[NTD.19$Mode == "CR" & NTD.19$PMT != 0]/NTD.19$UPT[NTD.19$Mode == "CR" & NTD.19$PMT != 0], na.rm = T)

mean(NTD.19$PMT[NTD.19$Mode == "MB" & NTD.19$PMT != 0]/NTD.19$UPT[NTD.19$Mode == "MB" & NTD.19$PMT != 0], na.rm = T)


#b.	Passenger miles per directional route mile (DRM)
mean(NTD.19$PMT[NTD.19$Mode == "LR" & NTD.19$PMT != 0]/NTD.19$DRM[NTD.19$Mode == "LR" & NTD.19$PMT != 0], na.rm = T)

mean(NTD.19$PMT[NTD.19$Mode == "HR" & NTD.19$PMT != 0]/NTD.19$DRM[NTD.19$Mode == "HR" & NTD.19$PMT != 0], na.rm = T)

mean(NTD.19$PMT[NTD.19$Mode == "CR" & NTD.19$PMT != 0]/NTD.19$DRM[NTD.19$Mode == "CR" & NTD.19$PMT != 0], na.rm = T)

mean(NTD.19$PMT[NTD.19$Mode == "MB" & NTD.19$PMT != 0]/NTD.19$DRM[NTD.19$Mode == "MB" & NTD.19$PMT != 0], na.rm = T)



#c.	Fare revenue per passenger mile (note that this is an estimate of how much people pay for each mile of service)
mean(NTD.19$FARES[NTD.19$Mode == "LR" & NTD.19$PMT != 0]/NTD.19$PMT[NTD.19$Mode == "LR" & NTD.19$PMT != 0], na.rm = T)

mean(NTD.19$FARES[NTD.19$Mode == "HR" & NTD.19$PMT != 0]/NTD.19$PMT[NTD.19$Mode == "HR" & NTD.19$PMT != 0], na.rm = T)

mean(NTD.19$FARES[NTD.19$Mode == "CR" & NTD.19$PMT != 0]/NTD.19$PMT[NTD.19$Mode == "CR" & NTD.19$PMT != 0], na.rm = T)

mean(NTD.19$FARES[NTD.19$Mode == "MB" & NTD.19$PMT != 0]/NTD.19$PMT[NTD.19$Mode == "MB" & NTD.19$PMT != 0], na.rm = T)


#d. Fare recovery ratio (total fare revenue divided by total operating costs.
mean(NTD.19$FARES[NTD.19$Mode == "LR" & NTD.19$PMT != 0]/NTD.19$OPEXP_TOTAL[NTD.19$Mode == "LR" & NTD.19$PMT != 0], na.rm = T)

mean(NTD.19$FARES[NTD.19$Mode == "HR" & NTD.19$PMT != 0]/NTD.19$OPEXP_TOTAL[NTD.19$Mode == "HR" & NTD.19$PMT != 0], na.rm = T)

mean(NTD.19$FARES[NTD.19$Mode == "CR" & NTD.19$PMT != 0]/NTD.19$OPEXP_TOTAL[NTD.19$Mode == "CR" & NTD.19$PMT != 0], na.rm = T)

mean(NTD.19$FARES[NTD.19$Mode == "MB" & NTD.19$PMT != 0]/NTD.19$OPEXP_TOTAL[NTD.19$Mode == "MB" & NTD.19$PMT != 0], na.rm = T)


#e.	Average speed (vehicle revenue miles divided by vehicle revenue hours). (Rail modes only.)
mean(NTD.19$VRM[NTD.19$Mode == "LR" & NTD.19$PMT != 0]/NTD.19$VRH[NTD.19$Mode == "LR" & NTD.19$PMT != 0], na.rm = T)

mean(NTD.19$VRM[NTD.19$Mode == "HR" & NTD.19$PMT != 0]/NTD.19$VRH[NTD.19$Mode == "HR" & NTD.19$PMT != 0], na.rm = T)

mean(NTD.19$VRM[NTD.19$Mode == "CR" & NTD.19$PMT != 0]/NTD.19$VRH[NTD.19$Mode == "CR" & NTD.19$PMT != 0], na.rm = T)

mean(NTD.19$VRM[NTD.19$Mode == "MB" & NTD.19$PMT != 0]/NTD.19$VRH[NTD.19$Mode == "MB" & NTD.19$PMT != 0], na.rm = T)

#2 TOTAL FARE RECOVERY
sum(NTD.19$FARES[NTD.19$Mode == "LR" & NTD.19$PMT != 0])/sum(NTD.19$OPEXP_TOTAL[NTD.19$Mode == "LR" & NTD.19$PMT != 0], na.rm = T)

sum(NTD.19$FARES[NTD.19$Mode == "HR" & NTD.19$PMT != 0])/sum(NTD.19$OPEXP_TOTAL[NTD.19$Mode == "HR" & NTD.19$PMT != 0], na.rm = T)

sum(NTD.19$FARES[NTD.19$Mode == "CR" & NTD.19$PMT != 0])/sum(NTD.19$OPEXP_TOTAL[NTD.19$Mode == "CR" & NTD.19$PMT != 0], na.rm = T)

sum(NTD.19$FARES[NTD.19$Mode == "MB" & NTD.19$PMT != 0],na.rm=T)/sum(NTD.19$OPEXP_TOTAL[NTD.19$Mode == "MB" & NTD.19$PMT != 0], na.rm = T)

#bigger systems are more efficient

#LA plots
##LA does not operate under one transportation authority
subset(NTD.19, City == "Los Angeles")

table(NTD.19$NTD.ID[NTD.19$City == "Los Angeles"])

##Include multiple operators
LA <- subset(NTD.ts, Year > 2001 & VRM > 0 & City == "Los Angeles")
LA <- subset(LA, NTD.ID == 90154 | NTD.ID == 90151 )



LA <- subset(LA, Mode == "HR" | Mode == "LR" | Mode == "MB" | Mode == "CR")

LA$FRR <- LA$FARES / LA$OPEXP_TOTAL
library(ggplot2)
LA$Mode <- as.character(LA$Mode)
LA$Mode[LA$Mode == "MB" & LA$Service == "PT"] <- "MB_PT"


ggplot(LA,  aes(x = Year, y = FRR ,  colour = Mode)) +
  geom_line()


#Did not adjust for inflation
subset(LA[c("Year", "UPT", "FARES", "OPEXP_TOTAL", "FRR", "Mode")], Mode == "CR")

subset(LA[c("Year", "UPT","FARES", "OPEXP_TOTAL", "FRR", "Mode")], Mode == "MB")

##OTHER USEFUL PLOTS
LA$FARES_PMT <- LA$FARES / LA$PMT
LA$FARES_UPT <- LA$FARES / LA$UPT

ggplot(LA,  aes(x = Year, y = FARES_PMT ,  colour = Mode)) +
  geom_line()


ggplot(LA,  aes(x = Year, y = FARES_UPT ,  colour = Mode)) +
  geom_line()


ggplot(LA,  aes(x = Year, y = PMT ,  colour = Mode)) +
  geom_line()
