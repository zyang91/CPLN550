rm(list=ls())
setwd("/Users/apple/Desktop/CPLN550/Data")
####Load data
load("data.Rda")
load("data_test.Rda")
#Feel free to double click the dat or load with code

dat <- data_train
rm(data_train)

#Phoenix-Mesa-Scottsdale AZ if desired

####### DATA INSPECTION #######

# Look at the data
# Dummies
str(dat)

#Dictionary
labels <- c("Average weekday transit ridership (average of boardings and alightings)",
            "Trains in and out at the AM peak", "Number of bus connections within a quarter mile",
            "Park and ride dummy", "Commuter rail dummy", "terminal station", "airport station",
            "BRT station", "light rail station", "heavy rail station", "multimodal transfer center", 
            "average distance to next station stops", "network distance to central business district",
            "jobs accessible within 30m drive", "Population residing within a half mile", "jobs within a half mile",
            "Unique CBSA id", "CBSA name")

labels <- as.data.frame(cbind(names(dat),labels))
labels

head(dat)
summary(dat)
table(dat$parking_d)
hist(dat$rider)
library(tidyverse)
ggplot(dat, aes(x=rider)) + geom_histogram(binwidth=1000)

# What are CBSAs?
# https://en.wikipedia.org/wiki/Core-based_statistical_area

dat$jobs_30m_drive[dat$jobs_30m_drive < 0] <- NA
# This isn't a very helpful visualization because of outlier(s).
plot(density(dat$rider))
# What should normal look like?
normaldist <- rnorm(1000,mean=6300,sd=12775.94)
plot(density(normaldist))


par(mfrow=c(1,2))
plot(density(dat$rider))
plot(density(normaldist))

# Let's take a closer look
summary(dat$rider)
# How to find the row(s) with max. ridership?
dat[which.max(dat$rider),]

# Seems like most observations have ridership below 10,000.
hist(dat$rider[dat$rider < 10000], breaks = 20)

hist(dat$rider[dat$lrt_d == 1])
hist(dat$rider[dat$hrt_d == 1])

hist(dat$rider[dat$lrt_d == 1 & dat $ rider < 10000])
hist(dat$rider[dat$hrt_d == 1 & dat $ rider < 10000])

# Plot two variables
# What's the relationship?
par(mfrow=c(1,1))

plot(dat$frequency, dat$rider)
abline(lm(dat$rider ~ dat$frequency))

ggplot(dat, aes(x=frequency, y=rider)) + geom_point() + geom_smooth(method = "lm")

#Boxplot is a helpful way to look at distribution

boxplot(rider ~ hrt_d, dat)

# Outlier issue
boxplot(rider ~ hrt_d, dat, outline = F)
boxplot(rider~hrt_d, subset(dat, dat$rider <10000))
median(dat$rider[dat$hrt_d==0])
median(dat$rider[dat$hrt_d==1])

# Is New York causing problems again?
mean(dat$rider[dat$hrt_d==1])
mean(dat$rider[dat$hrt_d==1 & dat$CBSA_nam!="New York-Northern New Jersey-Long Island NY-NJ-PA"])


###### MODEL BUILDING ######

#Predict Average daily ridership as a function of population, jobs, and service frequency
# Model building

ggplot(dat, aes(x=pop_halfmile, y=rider)) + geom_point() + geom_smooth(method = "lm")

x <- lm(rider ~ pop_halfmile + jobs_halfmile + frequency, dat)
summary(x)


## How to interpret the results? Causality?
# Coefficients, Stand Error, Pr, p-value, R-squared or Adjusted R-squared?

2.012e-01+2.991e-02*1.96
2.012e-01-2.991e-02*1.96

# This is not something you need to know now
# but it will be handy in the future.

TheAwesomeCICalculator <- function(coefficient, stderror)
{
  upper <- coefficient + 1.96 * stderror
  lower <- coefficient - 1.96 * stderror
  cat("Upper = ", upper)
  cat(" Lower = ", lower)
}
TheAwesomeCICalculator(2.012e-01,2.991e-02) #Thank/blame Xiaoxia (Summer) Dong for this function

##Let's add bus connections
x2 <- lm(rider ~ pop_halfmile + jobs_halfmile + frequency + bus_connections, dat)
summary(x2)

# Did this improve our model
# What does the Adjusted R-squared tell you (be careful)?
library(stargazer)
stargazer(x,x2, type = "text")
# First check the Star (statistics significant)
# Second check whether the coefficient changes a lot

# 1. Informal: statistically significant variable? 
# 2. Theoretically logical? 
# 3. Formal Full Model Reduced model ANOVA test
anova(x,x2)

######Let's see what our models say
dat$p1 <- predict(x2)

hist(dat$p1)
# Notice something weird here? 

summary(dat$p1)

## Important to look at residuals.
# Is the model good enough?
plot(x2)

# Get the residuals
dat$r1<- resid(x2)
plot(dat$p1,dat$r1)

abline(0,0)


# Very clearly heteroscedastic (as opposed to homoscedastic).
# Indicates non-linear relationship, missing variables, etc.

##Let's try a non-linear function through transformation
hist(log(dat$rider))


# Why plus 1 here?


x3 <- lm(log(rider+1) ~ log(pop_halfmile+1) + log(jobs_halfmile+1) 
         + log(frequency) + log (bus_connections+1), dat)
summary(x3)
# R-square lower, but much better models

# Log transformation will get rid of negative values in prediction.
# How do these plots look? Better?
plot(x3)


# Let's predict using our new model
dat$p3 <- predict(x3)

# p3 are log transformed values.
dat$p3exp <- exp(dat$p3)-1

plot(density(dat$p3exp))
lines(density(dat$rider, na.rm=TRUE), col='red')

# Fitness v. Predicting power
# Try more on your own

####### PREDICTION #######

## Now let's see how well models predict on a new dataset
## Use the test set

load("data_test.Rda") ##NB directory must be properly set to load

# Let's use the model for prediction on another data set
data_test$p3 <- predict(x3, newdata=data_test)
data_test$p3exp <- exp(data_test$p3)-1

plot(density(data_test$p3exp))
lines(density(data_test$rider, na.rm=TRUE),col='green')

# Manual R-squared
mod <- lm(log(rider+1) ~ p3, data_test)
summary(mod)

1-sum((log(data_test$rider+1) - data_test$p3 )^2)/
  sum((log(data_test$rider+1) -mean(log(data_test$rider+1)))^2)

correlation <- cor(log(data_test$rider+1), data_test$p3)
correlation^2

##How about actual ridership
correlation <- cor(data_test$rider, data_test$p3exp)
correlation^2 #Even better


##Clean regression output with stargazer

library(stargazer)

stargazer(x,x2, type="text")

stargazer(ols1, type="html", out="models.htm"
          , dep.var.labels=c("Average light rail station boardings and alightings")
          , covariate.labels=c("People within  a half mile", "Jobs within a half mile", "AM peak service frequency",
                             "Bus connections nearby"))

##A few last thoughts

# Good fit does not always mean good predicting power. At a certain point, often in opposition.
# Prediction can only be as good as our data allow it to be
# "All models are wrong but some models are useful..." -George Box:
