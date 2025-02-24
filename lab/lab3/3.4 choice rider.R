# Load Rda file from data_processing and fit models
rm(list=ls())

library (stargazer)

load("choice_rider.rda")

summary(trip_train$p_transit)

plot(I(trip_train$Transit_JourneyTime-trip_train$Auto_TravelTime), trip_train$p_transit,)


logit0 <- glm(p_transit ~ 1, data = trip_train,family = binomial(link="logit"))

logit1 <- glm(p_transit ~     I(Transit_JourneyTime-Auto_TravelTime) +
                I(Transit_Fare- AutoCost),
              data = trip_train,family = binomial(link="logit"))

summary(logit1)

1- logLik(logit1)/logLik(logit0)

logit1$coefficients

exp(logit1$coefficients)


logit2 <- glm(p_transit ~ OP_VEH + 
                GEND2 + AGECAT2 + INCOME2 + RACE2 + EDUCA2 +has_under5 +
                I(Transit_JourneyTime-Auto_TravelTime) +
                I(Transit_Fare- AutoCost) 
              ,
              data = trip_train,family = binomial(link="logit"))


logit3 <- glm(p_transit ~ OP_VEH + 
                GEND2 + AGECAT2 + INCOME2 + RACE2 + EDUCA2 +has_under5
              + LandUseIndex +
                DistToCBD + PARKCOST_METER + 
                I(Transit_JourneyTime-Auto_TravelTime) +
                I(Transit_Fare- AutoCost) 
              + busFrqAvg + rail_800m,
              data = trip_train,family = binomial(link="logit"))


stargazer(logit1, logit2, logit3, type ="text")
stargazer(logit1, logit2, logit3, title= "Regression comparision", type="html", 
          align=TRUE,out="model_comparison_010621.html")




####Probability predictions
trip_train$prob <- predict(logit1, type = "response")

summary(trip_train$prob)


hist(trip_train$prob)

table(trip_train$prob >= .4 & trip_train$prob <= .6)


trip_test$prob <- predict(logit1, type = "response", newdata=trip_test)

boxplot(trip_test$prob ~ trip_test$p_transit)
