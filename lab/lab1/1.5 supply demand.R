#Intro to Transportation planning#
#Quick lab on learning basic functions on R


rm(list=ls()) ##Clear my objects from memory

###Let's generate some supply and demand functions for per capita daily VMT in a theoretical monocentric city

##start with price since supply and demand depend on it

##we're going to put the cost of travel at 0 to 300 cents per VMT
price<-0:300

##VMT supplied (think willingness to provide all inputs: roads, gas, cars, etc.)
qs<-price*2/30

##let's plot price against VMT supply
plot(qs, price)

##Demand also depends on price
qd<- 20 - price * 1.8/30
##let's compare our plots
lines(qd, price)

###now let's solve for equilibrium
##at equilibrium qs == qd, so: 

#20 - price * 1.8/30 == price*2/30
#2price/30 +1.8price/30 == 20
#3.8price == 600
p_eq <- 600/3.8 
print(p_eq)


##now plug this into either equation to get 
q_eq <- 20 - p_eq * 1.8/30
print(q_eq)


       
###Now, let's add 22 cent external cost. What's total social cost of provision
sc <- (price-22)*2/30
lines(sc, price)


##What's our new equilibrium?


###let's turn our objects into a single dataset and learn to look at them
cbind(price,qd,qs,sc)

dat <-as.data.frame(cbind(price,qd,qs,sc))

##remove unused objects
rm(price,qd,qs,sc)

#look at data
head(dat)
summary(dat)
str(dat)

##econometrics in a nutshell
df <- lm(qd ~ price, dat)
summary(df)

sf <- lm(qs ~ price, dat)
summary(sf)

##Elasticity
#Equation from Powerpoint slide: Elasticity = B1*(mean(X1)/mean(y))
-.067 * mean(dat$price)/mean(dat$qd)



###let's clean up our graphics
plot(dat$qs, dat$price, t="line")
lines(dat$qd, dat$price)
lines(dat$sc, dat$price)

plot(dat$qs, dat$price, t="line", col=2, xlab="Average daily VMT per capita", 
     ylab = "Price per VMT in cents",
     main = "Price, social cost, and VMT")

lines(dat$qd, dat$price, col=4)
lines(dat$sc, dat$price, lty=2, col=2)

##RECREATE WITH LEGEND IN GGPLOT


###a lot of packages. Some make life easier
#you only need to install once per computer
install.packages("ggplot2")
# need to load each time though
library(ggplot2)

graph <- ggplot(dat, aes(y=price)) +
  geom_line(aes(x = qd, colour = "Demand")) + 
  geom_line(aes(x = qs, colour = "Supply")) +
  geom_line(aes(x = sc, colour = "Social Cost"))

graph 

graph + theme(legend.title=element_blank())
