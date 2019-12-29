library(flexmix)
library(rpart)
#-------------------------------------------------------------------
#Load The Data
#-------------------------------------------------------------------
cartst <- car.test.frame

#Summary Statistics
str(cartst)

#-------------------------------------------------------------------
#Part 1A Perform cluster-wise regressions (Single Cluster)
#-------------------------------------------------------------------
car1 <- flexmix(Price ~ Mileage + Weight + Disp. + HP, 
              data=car.test.frame, k=1)
summary(car1)
parameters(car1, component=1)

#-------------------------------------------------------------------
#Part 1B Continued: Perform cluster-wise regressions (Two Clusters)
#-------------------------------------------------------------------
set.seed(1)
car2 <- flexmix(Price ~ Mileage + Weight + Disp. + HP, 
              data=car.test.frame, k=2)
summary(car2)

parameters(car2, component=1)

parameters(car2, component=2)

#Done



