
#####  Part 1: Conduct exploratory data analysis (EDA) and prepare data for analysis                      ####
charity <- read.csv(file.choose()) 
dim(charity)
#8009 Observations
names(charity)
str(charity)
summary(charity)
#Training, validation, and test sets
summary(charity$part)

#Summarize binary variables.
#Region 5 = 1661
sum(charity$reg1==1)
1605/8009
sum(charity$reg2==1)
2555/8009
sum(charity$reg3==1)
1071/8009
sum(charity$reg4==1)
1117/8009
sum(1605,2555,1071,1117)
8009-6348
1661/8009
PopulationByRegion <- c(1605, 2555, 1071, 1117, 1661)
RegionName <- c("Reg1", "Reg2", "Reg3", "Reg4", "Reg5")
barplot(PopulationByRegion, names = RegionName,
        xlab = "Region", ylab = "Population", main = "Population by Region")

#There are 6940 homeowners, 1069 non-homeowners
sum(charity$home==1)
8009-6940
6940/8009
1069/8009

#There are 4848 female donors, 3161 male
sum(charity$genf==1)
8009-4848

#Number of children
table(charity$chld)
hist(charity$chld)

#Household income
table(charity$hinc)
hist(charity$hinc)

#Wealth rating
table(charity$wrat)
hist(charity$wrat)


#----Average home value                                                                                   ----
summary(charity$avhv)
boxplot(charity$avhv, main = "Boxplot of Average Home Values")
Q1avhv <- quantile(charity$avhv, 0.25)
Q3avhv <- quantile(charity$avhv, 0.75)
IQRavhv <- Q3avhv - Q1avhv
IQRavhv.1.5 <- IQRavhv*1.5
maxUsualavhv <- Q3avhv + IQRavhv.1.5 # Max usual value is 343
maxUsualavhv
minUsualavhv <- Q1avhv - IQRavhv.1.5 # Min usual value is 7
minUsualavhv
sum(charity$avhv > 343)
#Truncate at max usual value
charity$avhv[charity$avhv > 343] <- 343
summary(charity$avhv)

#Outliers only at top

#----Median family income                                                                                 ----
summary(charity$incm)
boxplot(charity$incm, main = "Boxplot of Median Family Income")
Q1incm <- quantile(charity$incm, 0.25)
Q3incm <- quantile(charity$incm, 0.75)
IQRincm <- Q3incm - Q1incm
IQRincm.1.5 <- IQRincm*1.5
maxUsualincm <- Q3incm + IQRincm.1.5 
minUsualincm <- Q1incm - IQRincm.1.5 
maxUsualincm
minUsualincm
sum(charity$incm > 94.5)
#Truncate at max usual value
charity$incm[charity$incm > 94.5] <- 94.5
summary(charity$incm)
#Outliers only at top end
#Max usual value is 94.5
#Min usual value is -13.5

#----Average family income                                                                                -----
summary(charity$inca)
boxplot(charity$inca, main = "Boxplot of Average Family Income")
Q1inca <- quantile(charity$inca, 0.25)
Q3inca <- quantile(charity$inca, 0.75)
IQRinca <- Q3inca - Q1inca
IQRinca.1.5 <- IQRinca*1.5
maxUsualinca <- Q3inca + IQRinca.1.5 
maxUsualinca
minUsualinca <- Q1inca - IQRinca.1.5 
minUsualinca
sum(charity$inca > 110)
charity$inca[charity$inca > 110] <- 110
summary(charity$inca)
#Max usual value is 110
#Min usual value is -2

#----Percentage of low income neighbors                                                                   ----
summary(charity$plow)
boxplot(charity$plow, main = "Boxplot of Percentage of Low-Income Neighbors")
Q1plow <- quantile(charity$plow, 0.25)
Q3plow <- quantile(charity$plow, 0.75)
IQRplow <- Q3plow - Q1plow
IQRplow.1.5 <- IQRplow*1.5
maxUsualplow <- Q3plow + IQRplow.1.5 
maxUsualplow
minUsualplow <- Q1plow - IQRplow.1.5
minUsualplow
sum(charity$plow > 46.5)
charity$plow[charity$plow > 46.5] <- 46.5
summary(charity$plow)
#Max usual value is 46.5
#Min usual value is -21.5


#----Number of lifetime promotions received                                                               ----
summary(charity$npro)
boxplot(charity$npro, main = "Boxplot of No. Lifetime Promotions Received")
Q1npro <- quantile(charity$npro, 0.25)
Q3npro <- quantile(charity$npro, 0.75)
IQRnpro <- Q3npro - Q1npro
IQRnpro.1.5 <- IQRnpro*1.5
maxUsualnpro <- Q3npro + IQRnpro.1.5 # Max usual value is 151
maxUsualnpro
minUsualnpro <- Q1npro - IQRnpro.1.5 # Min usual value is -33
minUsualnpro
sum(charity$npro > 151)
charity$npro[charity$npro > 151] <- 151
summary(charity$npro)

#----Dollar amount of lifetime gifts to date                                                              ----
summary(charity$tgif)
boxplot(charity$tgif, main = "Boxplot of Lifetime Gifts to Date")
Q1tgif <- quantile(charity$tgif, 0.25)
Q3tgif <- quantile(charity$tgif, 0.75)
IQRtgif <- Q3tgif - Q1tgif
IQRtgif.1.5 <- IQRtgif*1.5
maxUsualtgif <- Q3tgif + IQRtgif.1.5 
maxUsualtgif
minUsualtgif <- Q1tgif - IQRtgif.1.5 
minUsualtgif
sum(charity$tgif > 248)
charity$tgif[charity$tgif > 248] <- 248
summary(charity$tgif)
#Max usual value is 248
#Min usual value is -48

#----Dollar amount of largest gift to date                                                                ----
summary(charity$lgif)
boxplot(charity$lgif, main = "Boxplot of Amount of Largest Gift to Date")
Q1lgif <- quantile(charity$lgif, 0.25)
Q3lgif <- quantile(charity$lgif, 0.75)
IQRlgif <- Q3lgif - Q1lgif
IQRlgif.1.5 <- IQRlgif*1.5
maxUsuallgif <- Q3lgif + IQRlgif.1.5 
maxUsuallgif
minUsuallgif <- Q1lgif - IQRlgif.1.5 
minUsuallgif
sum(charity$lgif > 47.5)
charity$lgif[charity$lgif > 47.5] <- 47.5
summary(charity$lgif)
#Max usual value is 47.5
#Min usual value is -12.5

#----Dollar amount of most recent gift                                                                    ----
summary(charity$rgif)
boxplot(charity$rgif, main = "Boxplot of Dollar Amount of Most Recent Gift")
Q1rgif <- quantile(charity$rgif, 0.25)
Q3rgif <- quantile(charity$rgif, 0.75)
IQRrgif <- Q3rgif - Q1rgif
IQRrgif.1.5 <- IQRrgif*1.5
maxUsualrgif <- Q3rgif + IQRrgif.1.5 # Max usual value is 39.5
maxUsualrgif
minUsualrgif <- Q1rgif - IQRrgif.1.5 # Min usual value is -12.5
minUsualrgif
sum(charity$rgif > 39.5)
charity$rgif[charity$rgif > 39.5] <- 39.5
summary(charity$rgif)

#----Number of months since last donation                                                                 ----
summary(charity$tdon)
boxplot(charity$tdon, main = "Boxplot of No. of Months Since Last Donation")
Q1tdon <- quantile(charity$tdon, 0.25)
Q3tdon <- quantile(charity$tdon, 0.75)
IQRtdon <- Q3tdon - Q1tdon
IQRtdon.1.5 <- IQRtdon*1.5
maxUsualtdon <- Q3tdon + IQRtdon.1.5 
maxUsualtdon
minUsualtdon <- Q1tdon - IQRtdon.1.5
minUsualtdon
sum(charity$tdon > 32.5)
charity$tdon[charity$tdon > 32.5] <- 32.5
summary(charity$tdon)
#Max usual value is 32.5
#Min usual value is 4.5

#----Number of months between first and second gift                                                       ----
summary(charity$tlag)
boxplot(charity$tlag, main = "Boxplot of No. of Months 
        Between First and Second Donation")
Q1tlag <- quantile(charity$tlag, 0.25)
Q3tlag <- quantile(charity$tlag, 0.75)
IQRtlag <- Q3tlag - Q1tlag
IQRtlag.1.5 <- IQRtlag*1.5
maxUsualtlag <- Q3tlag + IQRtlag.1.5 
maxUsualtlag
minUsualtlag <- Q1tlag - IQRtlag.1.5 
minUsualtlag
sum(charity$tlag > 11.5)
charity$tlag[charity$tlag > 11.5] <- 11.5
summary(charity$tlag)
#Max usual value is 11.5
#Min usual value is -0.5

#----Average dollar amount of gifts to date                                                               ----
summary(charity$agif)
boxplot(charity$agif, main = "Boxplot of Average Gift Amount")
Q1agif <- quantile(charity$agif, 0.25)
Q3agif <- quantile(charity$agif, 0.75)
IQRagif <- Q3agif - Q1agif
IQRagif.1.5 <- IQRagif*1.5
maxUsualagif <- Q3agif + IQRagif.1.5 
maxUsualagif
minUsualagif <- Q1agif - IQRagif.1.5 
minUsualagif
sum(charity$agif > 26.545)
charity$agif[charity$agif > 26.545] <- 26.545
summary(charity$agif)
#Max usual value is 26.55
#Min usual value is

#----View predictor variable histograms                                                                   ----
par(mfrow=c(4,3))
hist(charity$avhv) 
hist(charity$incm)
hist(charity$inca) 
hist(charity$plow)
hist(charity$npro)
hist(charity$tgif) 
hist(charity$lgif) 
hist(charity$rgif) 
hist(charity$tdon)
hist(charity$tlag) 
hist(charity$agif) 

#----Trans. vars. with dep: normality assumption                                                          ----
charity$incm <- log(charity$incm)
charity$tgif <- log(charity$tgif)

par(mfrow=c(1,2))
hist(charity$tgif)
hist(charity$incm)
#----Corr. among variables that serve as direct/indirect measurements of potential donor's wealth         ----
library(PerformanceAnalytics)
wealth.vars <- data.frame(cbind(charity$hinc, charity$wrat, charity$avhv,
                                charity$incm, charity$inca, charity$plow))
colnames(wealth.vars) <- c("hinc", "wrat", "avhv", "incm", "inca", "plow")
cor(wealth.vars)
WealthCors <- chart.Correlation(wealth.vars)


#----Corr. among the variables that describe potential donor's interactions with charity                  ----
interaction.vars <- data.frame(cbind(charity$npro, charity$tgif,
                                     charity$lgif, charity$rgif, 
                                     charity$tdon, charity$tlag,
                                     charity$agif))
colnames(interaction.vars) <- c("npro", "tgif", "lgif", "rgif", 
                                "tdon", "tlag", "agif")
cor(interaction.vars)
IntCors <- chart.Correlation(interaction.vars)

#----Divide into training, validation, and testing sets                                                   ----

#Training set preparation
data.train <- charity[charity$part == "train",]
dim(data.train)

#Separate predictor variables from response variables
x.train <- data.train[,2:21]

#Create a vector to hold the DONR values, then check length.
c.train <- data.train[,22]
n.train.c <- length(c.train)

#Response variable showing donation amounts for obs where DONR=1
y.train <- data.train[c.train==1,23]
#Length of vector y.train to show proportion of donors in the set.
n.train.y <- length(y.train)
#1995/3984 = 0.50

#Validation set preparation. Repeat as with training set.
data.valid <- charity[charity$part=="valid",]
x.valid <- data.valid[,2:21]
c.valid <- data.valid[,22] 
n.valid.c <- length(c.valid) 
y.valid <- data.valid[c.valid==1,23] 
n.valid.y <- length(y.valid) 

#Test Set
data.test <- charity[charity$part=="test",]
n.test <- dim(data.test)[1] 
#2007
x.test <- data.test[,2:21]

#Standardize the training dataset predictor variables
#Find mean and stdev of each predictor variable
x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
#Standardize: zero mean and unit standard deviation-all means are essentially zero all sd one
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) 
apply(x.train.std, 2, mean) 
apply(x.train.std, 2, sd) 
data.train.std.c <- data.frame(x.train.std, donr=c.train) 
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) 

#Standardize validation set using training mean and sd
x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd)
#Classify donr
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid)
#Predict damt when donr=1
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid)

#Standardize test set using training mean and sd
x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) 
data.test.std <- data.frame(x.test.std)

#----EDA on training dataset                                                                              ----
#Frequency tables for donr and categorical variables
#Donr by region
sum(data.train$reg1==1)
table(data.train$reg1, data.train$donr)

sum(data.train$reg2==1)
table(data.train$reg2, data.train$donr)

sum(data.train$reg3==1)
table(data.train$reg3, data.train$donr)

sum(data.train$reg4==1)
table(data.train$reg4, data.train$donr)
#Region 5 remainder is 277 (14% of donors)


#Donr by home ownership
#1995 donors, 1947 are homeowners (97%) and of homeowners (3519) 55% are donors (1947)
table(data.train$home, data.train$donr)

#Donr by number of children says that 60% of donors have 0 children 
table(data.train$chld, data.train$donr)

#Donr by household income category
table(data.train$hinc, data.train$donr)

#Donr by gender- approx 40%= male 60%= female.
table(data.train$genf, data.train$donr)

#Donr by wealth rating
table(data.train$wrat, data.train$donr)

#Donr by number of months since last donation
cor(data.train$tlag, data.train$donr)


#####   Part 2: Classification model for DONR                                                             #####
# Load required libraries.
library(MASS)
library(ISLR)
library(class)

#----Begin with logistic regression                                                                       ----
#Model as entered in sample code AIC 2181.7
attach(charity)
model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc 
                  + I(hinc^2) + genf + wrat + avhv + incm + inca + plow + npro + tgif 
                  + lgif + rgif + tdon + tlag + agif, data.train.std.c, family=binomial("logit"))
summary(model.log1)

#Include only variables listed as statistically significant 
#AIC: 2170
#Error: 0.1199
#Correct predictions 1791/1995 donors
model.log2 <- glm(donr ~ reg1 + reg2 + home + chld + I(hinc^2) + wrat
                  + incm + tgif + tdon + tlag, data.train.std.c,
                  family=binomial("logit"))
summary(model.log2)

#Create predictions for training set based on model and examine confusion matrix
glm.probs.2 <- predict(model.log2, type = "response")
glm.pred.2 = rep("0", 3984)
glm.pred.2[glm.probs.2 > .5] = "1"
table(glm.pred.2, c.train)
LogisticError.train.lm2 <- mean(glm.pred.2 != c.train)
LogisticError.train.lm2

#Add interaction term
#AIC: 2151.3
#Error: 0.1184
#Correct predictions 1795/1995 donors
model.log3 <- glm(donr ~ reg1 + reg2 + home*chld + I(hinc^2) + wrat
                  + incm * tgif + tdon + tlag, data.train.std.c,
                  family=binomial("logit"))
summary(model.log3)

#Create predictions for training set and examine confusion matrix
glm.probs.3 <- predict(model.log3, type = "response")
glm.pred.3 = rep("0", 3984)
glm.pred.3[glm.probs.3 > .5] = "1"
table(glm.pred.3, c.train)
LogisticError.train.lm3 <- mean(glm.pred.3 != c.train)
LogisticError.train.lm3

#Model validation and testing.
#Run model.log2 on validation set
#AIC: 1219.8
#Error: 0.1258
#Correct predictions 887/999 donors
model.log2.valid <- glm(donr ~ reg1 + reg2 + home + chld + I(hinc^2) + wrat
                        + incm + plow + npro + tdon + tlag, data.valid.std.c,
                        family=binomial("logit"))
summary(model.log2.valid)

#Create predictions for validation set based on model and examine confusion matrix
glm.probs.valid2 <- predict(model.log2.valid, type = "response")
glm.pred.valid2 = rep("0", 2018)
glm.pred.valid2[glm.probs.valid2 > .5] = "1"
table(glm.pred.valid2, c.valid)
LogisticError.lm2 <- mean(glm.pred.valid2 != c.valid)
LogisticError.lm2

#Run model.log3 on validation set
#AIC: 1216.2
#Error: 0.1253
#Correct predictions 891/999 donors
model.log3.valid <- glm(donr ~ reg1 + reg2 + home*chld + hinc + I(hinc^2) +
                          wrat + incm*plow + npro + tdon + tlag,
                        data.valid.std.c, family = binomial("logit"))
summary(model.log3.valid)

#Create predictions for validation set based on model and examine confusion matrix
glm.probs.valid3 <- predict(model.log3.valid, type = "response")
glm.pred.valid3 = rep("0", 2018)
glm.pred.valid3[glm.probs.valid3 > .5] = "1"
table(glm.pred.valid3, c.valid)
LogisticError.lm3 <- mean(glm.pred.valid3 != c.valid)
LogisticError.lm3

#----Logistic regression GAM                                                                              ----
library(mgcv)
#All variables in model and no splines or transformations 
#AIC: 2789.143 
#Error: 0.1536145
#Correct predictions: 1726/1995 donors
gam.model1 <- gam(I(donr == 1) ~ reg1 + reg2 + reg3 + reg4 + home
                  + chld + hinc + genf + wrat + avhv + incm + inca + plow + npro
                  + tgif + lgif + rgif + tdon + tlag + agif, family = binomial,
                  data = data.train.std.c)
summary(gam.model1)

library(splines)
#AIC: 2561.171
#Error: 0.4952
#Correct predictions 1084/1995 donors
gam.model2 <- gam(I(donr==1) ~ reg2 + home*chld + I(wrat^2) + avhv + incm +
                    npro + tgif + I(tdon^2) + bs(tlag, knots = c(2, 7, 11)),
                  family = binomial, data = data.train.std.c)
summary(gam.model2)

#Confusion matrix for fitted values of gam.model1 and c.train
gam.preds.1 <- rep("0", 3984)
gam.preds.1[gam.model1$fitted.values > .5] = "1"
table(gam.preds.1, c.train)
mean(gam.preds.1 != c.train)

#Confusion matrix for fitted values of gam.model2 and c.train
gam.preds.2 <- rep("0", 3984)
gam.preds.2[gam.model2$fitted.values > .5] = "1"
table(gam.preds.2, c.train)
mean(gam.preds.2 != c.train)


#Run gam.model1 on validation set
#AIC: 1491.996
#Error: 0.1640
#Correct predictions 848/999 donors
gam.model1.valid <- gam(I(donr == 1) ~ reg1 + reg2 + reg3 + reg4 + home
                        + chld + hinc + genf + wrat + avhv + incm + inca
                        + plow + npro + tgif + lgif + rgif + tdon + tlag
                        + agif, family = binomial, data = data.valid.std.c)
summary(gam.model1.valid)

#Confusion matrix for fitted values and c.valid
gam.preds.valid.1 <- rep("0", 2018)
gam.preds.valid.1[gam.model1.valid$fitted.values > .5] = "1"
table(gam.preds.valid.1, c.valid)
gamError <- mean(gam.preds.valid.1 != c.valid)
gamError

#Run gam.model2 on validation set
#AIC: 1209.219
#Error: 0.1214
#Correct predictions 895/999 donors
gam.model2.valid <- gam(I(donr==1) ~ reg1 + reg2 + home + chld + I(hinc^2) +
                          wrat + plow + tdon + ns(tlag) + npro*tgif + avhv*incm,
                        family = binomial, data = data.valid.std.c)
summary(gam.model2.valid)

#Confusion matrix for fitted values and c.valid
gam.preds.valid.2 <- rep("0", 2018)
gam.preds.valid.2[gam.model2.valid$fitted.values > .5] = "1"
table(gam.preds.valid.2, c.valid)
gamError2 <- mean(gam.preds.valid.2 != c.valid)
gamError2

#----LDA                                                                                                  ----
library(MASS)
#Begin with all variables in model.
#Error: 0.1566
#Correct predictions 1766/1995 donors
lda.fit1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc 
                + genf + wrat + avhv + incm + inca + plow + npro
                + tgif + lgif + rgif + tdon + tlag + agif,
                data = data.train.std.c)
lda.fit1

#Reduced variable set
#Error: 0.1305
#Correct predictions 1831/1995 donors
lda.fit2 <- lda(donr ~ reg1 + reg2 + home*chld + hinc + I(hinc^2) +
                  wrat + incm*plow + npro + tdon + tlag, data = data.train.std.c)
lda.fit2

#Create predictions based on lda.fit1 and examine confusion matrix
lda.fit1.preds <- predict(lda.fit1, data.train.std.c)
lda.fit1.preds.class <- lda.fit1.preds$class
table(lda.fit1.preds.class, c.train)
mean(lda.fit1.preds.class != c.train)
lda.fit1.preds.posterior <- lda.fit1$posterior[,2]

#Create predictions based on lda.fit2 and examine confusion matrix
lda.fit2.preds <- predict(lda.fit2, data.train.std.c)
lda.fit2.preds.class <- lda.fit2.preds$class
table(lda.fit2.preds.class, c.train)
mean(lda.fit2.preds.class != c.train)
lda.fit2.preds.posterior <- lda.fit2$posterior[,2]


#Run lda.fit1 on validation set
#Error: 0.1699
#Correct predictions 872/999 donors
lda.fit1.valid <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc 
                      + genf + wrat + avhv + incm + inca + plow + npro
                      + tgif + lgif + rgif + tdon + tlag + agif,
                      data = data.valid.std.c)

#Create predictions based on lda.fit1 and examine confusion matrix.
lda.fit1.valid.preds <- predict(lda.fit1.valid, data.valid.std.c)
lda.fit1.valid.preds.class <- lda.fit1.valid.preds$class
table(lda.fit1.valid.preds.class, c.valid)
ldaError <- mean(lda.fit1.valid.preds.class != c.valid)
ldaError
lda.fit1.valid.preds.post <- lda.fit1.valid.preds$posterior

#Run lda.fit2 on validation set.
#Error: 0.1372
#Correct predictions 907/999 donors
lda.fit2.valid <- lda(donr ~ reg1 + reg2 + home*chld + I(hinc^2) + wrat + incm*plow +
                        npro + tdon + tlag, data = data.valid.std.c)

#Create predictions based on lda.fit2 and examine confusion matrix.
lda.fit2.valid.preds <- predict(lda.fit2.valid, data.valid.std.c)
lda.fit2.valid.preds.class <- lda.fit2.valid.preds$class
table(lda.fit2.valid.preds.class, c.valid)
ldaError2 <- mean(lda.fit2.valid.preds.class != c.valid)
ldaError2
lda.fit2.valid.preds.post <- lda.fit2.valid.preds$posterior

#----QDA                                                                                                  ----
#All variables in model 
#Error: 0.1463
#Predictions for 1825/1995 donors
qda.fit1 <- qda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc 
                + genf + wrat + avhv + incm + inca + plow + npro
                + tgif + lgif + rgif + tdon + tlag + agif,
                data = data.train.std.c)
qda.fit1

#Reduced variable set
#Error: 0.2073
#Correct predictions 1922/1995 donors
qda.fit2 <- qda(donr ~ reg1 + reg2 + home*chld + I(hinc^2) + wrat + incm*plow +
                  npro + tdon + tlag, data = data.train.std.c)
qda.fit2

#Create predictions based on qda.fit1 and examine confusion matrix
qda.fit1.preds <- predict(qda.fit1, data.train.std.c)
qda.fit1.preds.class <- qda.fit1.preds$class
table(qda.fit1.preds.class, c.train)
mean(qda.fit1.preds.class != c.train)
qda.fit1.preds.posterior <- qda.fit1$posterior[,2]

#Create predictions based on qda.fit2 and examine confusion matrix
qda.fit2.preds <- predict(qda.fit2, data.train.std.c)
qda.fit2.preds.class <- qda.fit2.preds$class
table(qda.fit2.preds.class, c.train)
mean(qda.fit2.preds.class != c.train)
qda.fit2.preds.posterior <- qda.fit2$posterior[,2]


#Run qda.fit1 on validation set.
#Error: 0.1427
#Correct predictions 947/999 donors
qda.fit1.valid <- qda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc 
                      + genf + wrat + avhv + incm + inca + plow + npro
                      + tgif + lgif + rgif + tdon + tlag + agif,
                      data = data.valid.std.c)

#Create predictions based on qda.fit1 and examine confusion matrix
qda.fit1.valid.preds <- predict(qda.fit1.valid, data.valid.std.c)
qda.fit1.valid.preds.class <- qda.fit1.valid.preds$class
table(qda.fit1.valid.preds.class, c.valid)
qdaError1 <- mean(qda.fit1.valid.preds.class != c.valid)
qdaError1
qda.fit1.valid.preds.post <- qda.fit1.valid.preds$posterior

#Run qda.fit2 on validation set
#Error: 0.2244 - Higher than training
#Correct predictions 964/1995 donors
qda.fit2.valid <- qda(donr ~ reg1 + reg2 + home*chld + I(hinc^2) + wrat + incm*plow +
                        npro + tdon + tlag, data = data.train.std.c)

#Create predictions based on qda.fit2 and examine confusion matrix
qda.fit2.valid.preds <- predict(qda.fit2.valid, data.valid.std.c)
qda.fit2.valid.preds.class <- qda.fit2.valid.preds$class
table(qda.fit2.valid.preds.class, c.valid)
qdaError2 <- mean(qda.fit2.valid.preds.class != c.valid)
qdaError2
qda.fit2.valid.preds.post <- qda.fit2.valid.preds$posterior

#----K-Nearest Neighbors                                                                                  ----
library(class)
train.charity.X <- cbind(data.train.std.c$reg1, data.train.std.c$reg2,
                         data.train.std.c$reg3, data.train.std.c$reg4,
                         data.train.std.c$home, data.train.std.c$chld,
                         data.train.std.c$hinc, data.train.std.c$genf, 
                         data.train.std.c$wrat, data.train.std.c$avhv, 
                         data.train.std.c$incm, data.train.std.c$inca, 
                         data.train.std.c$plow, data.train.std.c$npro, 
                         data.train.std.c$tgif, data.train.std.c$lgif, 
                         data.train.std.c$rgif, data.train.std.c$tdon, 
                         data.train.std.c$tlag, data.train.std.c$agif)
test.charity.X <- cbind(data.valid.std.c$reg1, data.valid.std.c$reg2, 
                        data.valid.std.c$reg3, data.valid.std.c$reg4, 
                        data.valid.std.c$home, data.valid.std.c$chld, 
                        data.valid.std.c$hinc, data.valid.std.c$genf, 
                        data.valid.std.c$wrat, data.valid.std.c$avhv, 
                        data.valid.std.c$incm, data.valid.std.c$inca, 
                        data.valid.std.c$plow, data.valid.std.c$npro, 
                        data.valid.std.c$tgif, data.valid.std.c$lgif, 
                        data.valid.std.c$rgif, data.valid.std.c$tdon,
                        data.valid.std.c$tlag, data.valid.std.c$agif)
dim(train.charity.X)
dim(test.charity.X)
train.Donr = data.train.std.c$donr
knn.pred=knn(train.charity.X,test.charity.X,train.Donr,k=1)
#Prediction accuracy
#Error: 0.2215
table(knn.pred, c.valid)
(725 + 846)/2018 #Correctly classified
(294 + 153)/2018 #Incorrectly classified
knnError <- mean(knn.pred != c.valid)
knnError

#Increase the number of nearest neighbors used
knn.pred2=knn(train.charity.X,test.charity.X,train.Donr,k=3)
#Check prediction accuracy.
#Error 0.2041
table(knn.pred2, c.valid)
(713 + 893)/2018 #Correctly classified
(306 + 106)/2018 #Incorrectly classified
knnError2 <- mean(knn.pred2 != c.valid)
knnError2

knn.pred3=knn(train.charity.X,test.charity.X,train.Donr,k=5)
#Prediction accuracy
#Error 0.1967
table(knn.pred3, c.valid)
(708 + 913)/2018 #Correctly classified
(311 + 86)/2018 #Incorrectly classified
knnError3 <- mean(knn.pred3 != c.valid)
knnError3

#----Decision Tree                                                                                        ----
library(tree)

#Datasets with donr coded as factor variable
tree.data.train = data.train.std.c
tree.data.train$donr = as.factor(tree.data.train$donr)
tree.data.valid = data.valid.std.c
tree.data.valid$donr = as.factor(tree.data.valid$donr)
#Fit tree and test
tree.Donr = tree(donr ~., tree.data.train)
plot(tree.Donr)
text(tree.Donr, pretty=0)
tree.pred = predict(tree.Donr, tree.data.valid, type = "class")
table(tree.pred, c.valid)
(783 + 929)/2018 #Correctly classified
(236 + 70)/2018 #Incorrectly classified
#Error: 0.1516
treeError <- mean(tree.pred != c.valid)
treeError

#Cross-validation to prune tree
cv.tree.Donr <- cv.tree(tree.Donr, FUN=prune.misclass)
plot(cv.tree.Donr$size, cv.tree.Donr$dev, type = "b")
plot(cv.tree.Donr$k, cv.tree.Donr$dev, type = "b")
#Most significant reductions by five lower at 9 and lowest at 15
prune.tree = prune.misclass(tree.Donr, best=5)
tree.pred.2 = predict(prune.tree, tree.data.valid, type="class")
table(tree.pred.2, c.valid)
#Error: 0.1838
treeError2 <- mean(tree.pred.2 != c.valid)
treeError2

#Error: 0.154
prune.tree = prune.misclass(tree.Donr, best=9)
tree.pred.3 = predict(prune.tree, tree.data.valid, type="class")
table(tree.pred.3, c.valid)
treeError3 <- mean(tree.pred.3 != c.valid)
treeError3

prune.tree = prune.misclass(tree.Donr, best=15)
tree.pred.4 = predict(prune.tree, tree.data.valid, type="class")
table(tree.pred.4, c.valid)
#Error: 0.1516
treeError4 <- mean(tree.pred.4 != c.valid)
treeError4

#----Bag and Random Forest                                                                                ----
library(randomForest)
set.seed(1)
bag.Donr = randomForest(donr ~ ., data=tree.data.train, mtry=20, 
                        importance=TRUE, type="classification")
importance(bag.Donr)
set.seed(1)
bag.pred = predict(bag.Donr, newdata=tree.data.valid)
#Correct predictions 903/999 donors
table(bag.pred, c.valid)
#Error: 0.1105
bagError = mean(bag.pred != c.valid)
bagError

#Boosting
#Error 0.107
#Correct predictions 927/999 donors

library(gbm)
set.seed(1)
boost.Donr = gbm(donr ~ ., data = data.train.std.c, distribution = "bernoulli",
                 n.trees = 5000, interaction.depth = 4)
set.seed(1)
boost.probs = predict.gbm(boost.Donr, newdata = data.valid.std.c,
                          n.trees = 5000, type = "response")
boost.pred = rep("0", 2018)
boost.pred[boost.probs > .5] = "1"
table(boost.pred , c.valid)
boostError <- mean(boost.pred != c.valid)
boostError

#Reduced dataset to improve output
set.seed(1)
boost.Donr2 = gbm(donr ~ reg1 + reg2 + home*chld + I(hinc^2) + wrat
                  + incm * tgif + tdon + tlag, data = data.train.std.c,
                  distribution = "bernoulli", n.trees = 5000,
                  interaction.depth = 4)
set.seed(1)
boost.probs2 = predict.gbm(boost.Donr2, newdata = data.valid.std.c,
                           n.trees = 5000, type = "response")
boost.pred2 = rep("0", 2018)
boost.pred2[boost.probs2 > .5] = "1"
table(boost.pred2 , c.valid)
boostError2 <- mean(boost.pred2 != c.valid)
boostError2

#----SVC                                                                                                  ----
#Error: 0.1645
#Correct predictions 865/999 donors
library(e1071)
set.seed(1)
# Use cross-validation to select the best parameters for the SVC.
tune.out = tune(svm, donr ~., data = tree.data.train, kernel = "linear",
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10)))
summary(tune.out)
# Best cost parameter is 1.
set.seed(1)
svm.fit1 = svm(donr ~., data = tree.data.train, kernel = "linear",
               cost = 1, scale = FALSE)
#----Predictions on Best Model                                                                            ----
bestmod = tune.out$best.model
summary(bestmod)
set.seed(1)
svm.pred = predict(bestmod, tree.data.valid)
table(svm.pred, c.valid)
#Error: 0.1625
svmError <- mean(svm.pred != c.valid)
svmError

#Error table for best models
Error <- c(LogisticError.lm3, gamError2, ldaError2, qdaError1,
           knnError3, treeError, bagError, boostError2, svmError)
Model = c("Logistic", "GAM", "LDA", "QDA", "KNN", "Tree", "Bag", "Boost", "SVM")
ErrorTable <- as.data.frame(cbind(Model, Error))
View(ErrorTable)

#Calculate ordered profit function for appliable models and set cutoffs
profit.logistic = cumsum(14.5 * c.valid[order(glm.probs.valid3,
                                              decreasing = T)] -2)
par(mfrow=c(3,3))
plot(profit.logistic, col = "forestgreen", main = "Logistic Model Profit")
n.mail.logistic = which.max(profit.logistic)

profit.gam = cumsum(14.5 * c.valid[order(gam.model2.valid$fitted.values,
                                         decreasing = T)] -2)
plot(profit.gam, col = "lightblue4", main = "GAM Model Profit")
n.mail.gam = which.max(profit.gam)

profit.lda = cumsum(14.5 * c.valid[order(lda.fit2.valid.preds.post[,2],
                                         decreasing = T)] -2)
plot(profit.lda, col = "purple", main = "LDA Model Profit")
n.mail.lda = which.max(profit.lda)

profit.qda = cumsum(14.5 * c.valid[order(qda.fit1.valid.preds.post[,2],
                                         decreasing = T)] -2)
plot(profit.qda, col = "lawngreen", main = "QDA Model Profit")
n.mail.qda = which.max(profit.qda)

profit.boost = cumsum(14.5 * c.valid[order(boost.probs2, decreasing = T)] - 2)
n.mail.boost = which.max(profit.boost)

#Set cutoffs based on mailing numbers
cutoff.logistic <- sort(glm.probs.valid3, decreasing = T)[n.mail.logistic + 1]
chat.valid.log <- ifelse(glm.probs.valid3 > cutoff.logistic, 1, 0)
table(chat.valid.log, c.valid)
#Sum of predicted donors: 381 + 993 = 1374
#Profit: (14.5 * 993) - (2 * 1374) = $11,650.5
LogisticDonors <- (381 + 993)
LogisticProfit <- (14.5 * 993) - (2 * 1374)

cutoff.gam <- sort(gam.model2.valid$fitted.values, decreasing = T)[n.mail.gam + 1]
chat.valid.gam <- ifelse(gam.model2.valid$fitted.values > cutoff.gam, 1, 0)
table(chat.valid.gam, c.valid)
#Sum of predicted donors: 995 + 391
#Profit: (14.5 * 995) - (2 * 1386) = $11,655.5
gamDonors <- (995 + 391)
gamProfit <- (14.5 * 995) - (2 * 1386)

cutoff.lda <- sort(lda.fit2.valid.preds.post[,2], decreasing = T)[n.mail.lda + 1]
chat.valid.lda <- ifelse(lda.fit2.valid.preds.post[,2] > cutoff.lda, 1, 0)
table(chat.valid.lda, c.valid)
#Sum of predicted donors: 341 + 982 = 1323 
#Profit: (14.5 * 982) - (2 * 1323) = $11,593
ldaDonors <- (341 + 982)
ldaProfit <- (14.5 * 982) - (2 * 1323)

cutoff.qda <- sort(qda.fit1.valid.preds.post[,2], decreasing = T)[n.mail.qda + 1]
chat.valid.qda <- ifelse(qda.fit1.valid.preds.post[,2] > cutoff.qda, 1, 0)
table(chat.valid.qda, c.valid)
#Sum of predicted donors: 353 + 977 = 1330 
#Profit: (14.5 * 977) - (2 * 1330) = $11,506.5
qdaDonors <- (353 + 977)
qdaProfit <- (14.5 * 977) - (2 * 1330)

cutoff.boost <- sort(boost.probs2, decreasing = T)[n.mail.boost + 1]
chat.valid.boost <- ifelse(boost.probs2 > cutoff.boost, 1, 0)
table(chat.valid.boost, c.valid)
#Sum of predicted donors: 255 + 988 = 1243 
#Profit: (14.5 * 988) - (2*1243) = $11,840
boostDonors <- 255 + 988
boostProfit <- (14.5 * 988) - (2*1243)

table(knn.pred3, c.valid)
#Sum of predicted donors: 311 + 913 = 1224
#Profit: (14.5 * 913) - (2 * 1224) = $10,790.5
knnDonors <- 311 + 913
knnProfit <- (14.5 * 913) - (2 * 1224)

table(tree.pred, c.valid)
#Sum of predicted donors: 236 + 929 = 1165 
#Profit: (14.5 * 929) - (2 * 1165) = $11,140.5
treeDonors <- (236 + 929)
treeProfit <- (14.5 * 929) - (2 * 1165)

table(bag.pred, c.valid)
#Sum of predicted donors: 131 + 907 = 1038 
#Profit: (14.5 * 907) - (2 * 1038) = $11,077.5
bagDonors <- 131 + 907
bagProfit <- (14.5 * 907) - (2 * 1038)

table(svm.pred, c.valid)
#Sum of predicted donors: 193 + 864 = 1057
#Profit: (14.5 * 864) - (2 * 1057) = $10,414
svmDonors <- 193 + 864
svmProfit <- (14.5 * 864) - (2 * 1057)

#Compare profits
Donors <- c(LogisticDonors, gamDonors, ldaDonors, qdaDonors, knnDonors,
            treeDonors, bagDonors, boostDonors, svmDonors)
Profits <- c(LogisticProfit, gamProfit, ldaProfit, qdaProfit, knnProfit,
             treeProfit, bagProfit, boostProfit, svmProfit)
ProfitTable <- as.data.frame(cbind(Model, Donors, Profits))
View(ProfitTable)
which.max(Profits)

#The boosted model has the highest predicted profit
#Predictions for test data set
set.seed(1)
post.test = predict.gbm(boost.Donr2, newdata = data.test.std,
                        n.trees = 5000, type = "response")

#Oversampling adjustment
n.mail.boost = which.max(profit.boost)
#Set typical response rate
tr.rate <- .1 
#Set validation response rate
vr.rate <- .5 
#Make adjustment
n.valid.c <- length(c.valid)
adj.test.1 <- (n.mail.boost/n.valid.c)/(vr.rate/tr.rate)
adj.test.0 <- ((n.valid.c-n.mail.boost)/n.valid.c)/((1-vr.rate)/(1-tr.rate))
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # Scale
#Calculate mailings for test set
n.mail.test <- round(n.test*adj.test, 0) # Number of mailings is 304.
n.mail.test
#Set cutoff
cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)

#####   Part 3: Prediction Model for DAMT                                                                 #####
library(leaps)
#----Linear Regression Model                                                                              ----
#All variables
model.lin1 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                   avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                 data.train.std.y)
summary(model.lin1)

#Use regsubsets() to identify best subset of predictor variables
regfit.full <- regsubsets(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc +
                            wrat + genf + avhv + incm + inca + plow + npro + tgif +
                            lgif + rgif + tdon + tlag + agif, data.train.std.y, nvmax = 20)
reg.summary = summary(regfit.full)
names(reg.summary)
reg.summary$rsq
reg.summary$adjr2
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

#Model with lowest Cp 
coef(regfit.full, 16)
model.lin2 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf +
                   incm + inca + plow + tgif + lgif + rgif + tdon + agif,
                 data.train.std.y)
summary(model.lin2)

#Run model on validation set
model.lin2.valid <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf +
                         incm + inca + plow + tgif + lgif + rgif + tdon + agif,
                       data.valid.std.y)
summary(model.lin2.valid)

#Make predictions for validation set based on model
pred.valid.lin2 <- predict(model.lin2.valid, data.valid.std.y)
MPE2 <- mean((y.valid - pred.valid.lin2)^2)
StandardError2 <- sd((y.valid - pred.valid.lin2)^2)/sqrt(n.valid.y)
MPE2
StandardError2

#Refine linear model
#Include interaction terms and non-linear transformations
model.lin3 <- lm(damt ~ reg1 + reg2*home + reg3 + reg4 + chld + genf + hinc +
                   incm * inca + I(plow^2) + tgif * lgif + rgif * agif + tdon,
                 data.train.std.y)

#Run model on validation set
model.lin3.valid <- lm(damt ~ reg1 + reg2*home + reg3 + reg4 + chld + genf + hinc +
                         incm * inca + I(plow^2) + tgif * lgif + rgif * agif + tdon,
                       data.valid.std.y)
summary(model.lin3.valid)

#Make predictions for validation set based on model
pred.valid.lin3 <- predict(model.lin3, data.valid.std.y)
MPE3 <- mean((y.valid - pred.valid.lin3)^2)
StandardError3 <- sd((y.valid - pred.valid.lin3)^2)/sqrt(n.valid.y)
MPE3
StandardError3

#----Best Subset with K-Fold Cross-Validation                                                             ----
set.seed(1)
regfit.best <- regsubsets(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc +
                            wrat + genf + avhv + incm + inca + plow + npro + tgif +
                            lgif + rgif + tdon + tlag + agif, data.train.std.y, nvmax = 20)

set.seed(1)
test.mat = model.matrix(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc +
                          wrat + genf + avhv + incm + inca + plow + npro + tgif +
                          lgif + rgif + tdon + tlag + agif, data.valid.std.y)

val.errors <- rep(NA,20)
for (i in 1:20) {
  coefi = coef(regfit.best, id=i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean((y.valid - pred)^2)}
val.errors
which.min(val.errors)
coef(regfit.best,14)

model.lin4 <- lm(damt ~ reg2 + reg3 + reg4 + home + chld + hinc + incm
                 + inca + plow + tgif + lgif + rgif + agif + tdon + agif, 
                 data.train.std.y)
summary(model.lin4)

#Run model on validation set
model.lin4.valid <- lm(damt ~ reg2 + reg3 + reg4 + home + chld + hinc + incm
                       + inca + plow + tgif + lgif + rgif + agif + tdon + agif, 
                       data.valid.std.y)
summary(model.lin4.valid)

#Make predictions for validation set based on model
pred.valid.lin4 <- predict(model.lin4, data.valid.std.y)
MPE4 <- mean((y.valid - pred.valid.lin4)^2)
StandardError4 <- sd((y.valid - pred.valid.lin4)^2)/sqrt(n.valid.y)
MPE4
StandardError4

#----Principal components regression                                                                      ----
library(pls)
set.seed(1)
pcr.fit = pcr(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc +
                wrat + genf + avhv + incm + inca + plow + npro + tgif +
                lgif + rgif + tdon + tlag + agif, data = data.train.std.y,
              scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP", type = "b")

set.seed(1)
pcr.pred = predict(pcr.fit, data.valid.std.y, ncomp=5)
MPE5 <- mean((y.valid - pcr.pred)^2)
StandardError5 <- sd((y.valid - pcr.pred)^2)/sqrt(n.valid.y)
MPE5
StandardError5
#20 components
set.seed(1)
pcr.pred2 = predict(pcr.fit, data.valid.std.y, ncomp=20)
MPE6 <- mean((y.valid - pcr.pred2)^2)
MPE6
StandardError6 <- sd((y.valid - pcr.pred2)^2)/sqrt(n.valid.y)
StandardError6

#----Partial Least Squares                                                                                ----
set.seed(1)
pls.fit = plsr(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc +
                 wrat + genf + avhv + incm + inca + plow + npro + tgif +
                 lgif + rgif + tdon + tlag + agif, data = data.train.std.y,
               scale = TRUE, validation = "CV") 
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP", type = "b")
#There is an 'elbow' in the graph at 3, with minimal reduction after.
#The elbow suggests three components may be sufficient.
set.seed(1)
pls.pred = predict(pls.fit, data.valid.std.y, ncomp=3)
MPE7 <- mean((y.valid - pls.pred)^2)
StandardError7 <- sd((y.valid - pls.pred)^2)/sqrt(n.valid.y)
MPE7
StandardError7

#----Ridge Regression                                                                                     ----
par(mfrow=c(1,1))

library(glmnet)
set.seed(1)
grid = 10^seq(10, -2, length=100)
mat.train <- data.matrix(data.train.std.y)
mat.train <- mat.train[,-21]
#Remove damt
ridge.mod = glmnet(mat.train, y.train, alpha=0, lambda=grid,
                   thresh=1e-12)
#Use cross-validation to choose lambda
set.seed(1)
cv.out = cv.glmnet(mat.train, y.train, alpha=0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
#Make predictions and view errors
mat.valid = as.matrix(data.valid.std.y)
mat.valid <- mat.valid[,-21]
set.seed(1)
ridge.pred = predict(ridge.mod, s=bestlam, newx=mat.valid)
MPE8 <- mean((y.valid - ridge.pred)^2)
StandardError8 <- sd((y.valid - ridge.pred)^2)/sqrt(n.valid.y)
MPE8
StandardError8

#----Lasso                                                                                                ----
set.seed(1)
#Use matrices and lambda grid from ridge regression
lasso.mod = glmnet(mat.train, y.train, alpha=1, lambda=grid)
#Use cross-validation to select lambda
set.seed(1)
cv.out.lasso = cv.glmnet(mat.train, y.train, alpha=1)
plot(cv.out.lasso)
bestlamlasso = cv.out.lasso$lambda.min
bestlamlasso
set.seed(1)
lasso.pred = predict(lasso.mod, s=bestlamlasso, newx=mat.valid)
MPE9 <- mean((y.valid - lasso.pred)^2)
StandardError9 <- sd((y.valid - lasso.pred)^2)/sqrt(n.valid.y)
MPE9
StandardError9

#----Decision Tree                                                                                        ----
set.seed(1)
tree.Damt = tree(damt ~., data.train.std.y)
plot(tree.Damt)
text(tree.Damt, pretty=0)
summary(tree.Damt)
set.seed(1)
tree.pred = predict(tree.Damt, data.valid.std.y)
MPE10 <- mean((y.valid - tree.pred)^2)
StandardError10 <- sd((y.valid - tree.pred)^2)/sqrt(n.valid.y)
MPE10
StandardError10

#Cross-validation to prune tree
cv.Damt = cv.tree(tree.Damt)
plot(cv.Damt$size, cv.Damt$dev, type = "b")
set.seed(1)
prune.Damt = prune.tree(tree.Damt, best = 7)
plot(prune.Damt)
text(prune.Damt, pretty = 0)
set.seed(1)
tree.pred2 <- predict(prune.Damt, data.valid.std.y)
MPE11 <- mean((y.valid - tree.pred2)^2)
StandardError11 <- sd((y.valid - tree.pred2)^2)/sqrt(n.valid.y)
MPE11
StandardError11

#----Bagging                                                                                              ----
set.seed(1)
bag.Damt = randomForest(damt ~., data = data.train.std.y, mtry=20,
                        importance=TRUE)
bag.Damt
set.seed(1)
pred.bag.Damt <- predict(bag.Damt, newdata=data.valid.std.y)
MPE12 <- mean((y.valid - pred.bag.Damt)^2)
StandardError12 <- sd((y.valid - pred.bag.Damt)^2/sqrt(n.valid.y))
MPE12
StandardError12

#----Random Forest                                                                                        ----
set.seed(1)
rf.Damt = randomForest(damt ~., data=data.train.std.y, importance=TRUE)
rf.Damt
set.seed(1)
pred.RF.Damt <- predict(rf.Damt, newdata = data.valid.std.y)
MPE13 <- mean((y.valid - pred.RF.Damt)^2)
StandardError13 <- sd((y.valid - pred.RF.Damt)^2/sqrt(n.valid.y))
MPE13
StandardError13

#----Boosting                                                                                             ----
set.seed(1)
boost.Damt = gbm(damt ~., data = data.train.std.y, distribution = "gaussian",
                 n.trees=5000, interaction.depth=4)
summary(boost.Damt)
set.seed(1)
pred.boost.Damt <- predict(boost.Damt, newdata = data.valid.std.y, n.trees = 5000)
MPE14 <- mean((y.valid - pred.boost.Damt)^2)
StandardError14 <- sd((y.valid - pred.boost.Damt)^2/sqrt(n.valid.y))
#Error: 1.5444
MPE14
StandardError14

#Improve boosted model by running on reduced dataset by cross validation for 14 variables
set.seed(1)
boost.Damt2 <- gbm(damt ~ reg2 + reg3 + reg4 + home + chld + hinc + incm
                   + inca + plow + tgif + lgif + rgif + agif + tdon,
                   data = data.train.std.y, distribution = "gaussian",
                   n.trees = 5000, interaction.depth=4)
summary(boost.Damt2)
set.seed(1)
pred.boost.Damt2 <- predict(boost.Damt2, newdata = data.valid.std.y,
                            n.trees = 5000)
MPE15 <- mean((y.valid - pred.boost.Damt2)^2)
StandardError15 <- sd((y.valid - pred.boost.Damt2)^2/sqrt(n.valid.y))
#Error: 1.6430
MPE15
StandardError15

#Improve boosted model by running on dataset from OLS model (MPE not improved)
set.seed(1)
boost.Damt3 <- gbm(damt ~ reg1 + reg2*home + reg3 + reg4 + chld + genf + hinc +
                     incm * inca + I(plow^2) + tgif * lgif + rgif * agif + tdon,
                   data = data.train.std.y, distribution = "gaussian",
                   n.trees = 5000, interaction.depth = 4)
summary(boost.Damt3)
set.seed(1)
pred.boost.Damt3 <- predict(boost.Damt3, newdata = data.valid.std.y,
                            n.trees = 5000)
MPE16 <- mean((y.valid - pred.boost.Damt3)^2)
StandardError16 <- sd((y.valid - pred.boost.Damt3)^2)/sqrt(n.valid.y)
#Error: 1.6386
MPE16
StandardError16

#----Compare MPE                                                                                          ----
Model2 <- c("OLS", "CV Best Subsets", "PCR", "PLS", "Ridge", "Lasso", "Tree",
            "Bag", "Random Forest", "Boost")
MPEvals <- c(MPE3, MPE4, MPE6, MPE7, MPE8, MPE9, MPE10, MPE12, MPE13, MPE14)
StdErrVals <- c(StandardError3, StandardError4, StandardError6,
                StandardError7, StandardError8, StandardError9,
                StandardError10, StandardError12, StandardError13,
                StandardError14)
DamtErrors <- as.data.frame(cbind(Model2, MPEvals, StdErrVals))
View(DamtErrors)
which.min(MPEvals)

#Create predictions for test set
set.seed(1)
yhat.test <- predict(boost.Damt, data.test.std, n.trees = 5000)


#####Final results                                                                                        #####

#Both lengths of "hat" vectors are 2007
length(chat.test)
length(yhat.test)
#Check content of "hat" vectors is plausible for goals
chat.test[1:10] 
yhat.test[1:10]
#Summary of final predictions
table(chat.test)
summary(yhat.test)
#Create dataframe with one column for the classification prediction and another for the dollar amount prediction
hat.data.frame <- data.frame(chat = chat.test, yhat = yhat.test)
View(hat.data.frame)
#Check expected profit from mailing
PredictedDonors <- subset(hat.data.frame, chat==1)
FinalProfit <- sum(PredictedDonors$yhat)
FinalProfit
#Check values
sum(is.na(hat.data.frame))

#Change in return rate
(0.15 - 0.10)/0.10

#CSV
write.csv(hat.data.frame, "CourseProject_Perkins.csv", row.names = FALSE)
