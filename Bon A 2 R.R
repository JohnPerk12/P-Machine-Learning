#--------------------------------------------------------------------------
#Load the Data
#--------------------------------------------------------------------------
library(caret)

for(package in c("caret",
                 "reshape",
                 "ggplot2",
                 "knitr")) {
  if(!require(package, character.only=TRUE)) {
    install.packages(package, repos="http://cran.us.r-project.org")
    library(package, character.only=TRUE)
  }
}

rm(package)

data(GermanCredit)
cred <- GermanCredit

#--------------------------------------------------------------------------
#Summary Statistics
#--------------------------------------------------------------------------
str(cred)

#--------------------------------------------------------------------------
#Part 2 Build a regression model to predict "Amount"
#--------------------------------------------------------------------------
print(paste("length(training set):", nrow(cred)*0.632))
print(paste("length(test set):", nrow(cred)*(1-0.632)))

df_coeff <- data.frame()
df_gof.train <- data.frame()
df_gof.test <- data.frame()

iter <- 1000

for (i in 1:iter) {
  train <- 0.632
  randraw <- runif(nrow(GermanCredit))
  
  GCred_train <- GermanCredit[randraw <= train, ]
  GCred_test <- GermanCredit[randraw > train, ]
  
  amt_train <- GCred_train$Amount
  amt_test <- GCred_test$Amount
  
  objControl <- trainControl(method="cv", number=3, 
                             returnResamp="none", allowParallel=TRUE, verboseIter=TRUE)
  f_lms <- train(Amount ~ .,
                   data=GCred_train,
                   method="lm", trControl=objControl)
  
  summary(f_lms)
  
  df_temp <- as.data.frame(t(coef(fit_lms$finalModel)))
  df_coeff <- rbind(df_coeff, df_temp) 
  
  pred_lms <- predict(f_lms, GCred_train)
  
  k <- 61 - 13
  n <- nrow(GCred_train)
  resid <- amt_train - pred_lms
  sst <- sum((amt_train - mean(amt_train))^2)
  sse <- sum(resid^2)
  ssr <- sst - sse
  mae <- mean(abs(resid))
  mse <- mean((resid)^2)
  rmse <- sqrt(mse)
  r2 <- ssr / sst
  adjr2 <- 1 - (sse/(n-k-1)) / (sst/(n-1))
  
  stats <- c(mae, mse, rmse, r2, adjr2)
  df_temp <- as.data.frame(t(stats))
  names(df_temp) <- c("mae", "mse", "rmse", "r2", "adjr2")
  rm(k, n, resid, sst, sse, ssr, mae, mse, r2, adjr2)
  
  df_gof.train <- rbind(df_gof.train, df_temp)
  
  pred_lms <- predict(f_lms, GCred_test)
  
  k <- 61 - 13
  n <- nrow(GCred_test)
  resid <- amt_test - pred_lms
  sst <- sum((amt_test - mean(amt_test))^2)
  sse <- sum(resid^2)
  ssr <- sst - sse
  mae <- mean(abs(resid))
  mse <- mean((resid)^2)
  rmse <- sqrt(mse)
  r2 <- ssr / sst
  adjr2 <- 1 - (sse/(n-k-1)) / (sst/(n-1))

  stats <- c(mae, mse, rmse, r2, adjr2)
  df_temp <- as.data.frame(t(stats))
  names(df_temp) <- c("mae", "mse", "rmse", "r2", "adjr2")
  rm(k, n, resid, sst, sse, ssr, mae, mse, r2, adjr2)
  
  df_gof.test <- rbind(df_gof.test, df_temp)
}

#--------------------------------------------------------------------------
#Plot the distributions of all coefficients
#--------------------------------------------------------------------------
df_temp <- df_coeff[, c(2:8)]
df_temp[, "iter"] <- rownames(df_temp)
df_temp[, "iter"] <- factor(df_temp[, "iter"], levels=df_temp[, "iter"])
df_temp <- melt(df_temp,  id.vars="iter", variable_name="coeff")

ggplot(df_temp, aes(x=iter, y=value, group=coeff)) + 
  geom_line(aes(color=factor(coeff))) +
  labs(list(x="Iterations", y="Estimate")) + 
  scale_x_discrete(breaks=round(seq(0, iter, by=iter/5),1)) + 
  guides(col=guide_legend(title="Coefficient", ncol=2)) +
  theme(legend.position="bottom")

dev.off()

#--------------------------------------------------------------------------
#Plot Holdout R-Squared
#--------------------------------------------------------------------------
df_temp <- df_gof.train["r2"]
names(df_temp) <- c("R2_train")
df_temp["R2_test"] <- df_gof.test["r2"]
df_temp[, "iter"] <- rownames(df_temp)
df_temp[, "iter"] <- factor(df_temp[, "iter"], levels=df_temp[, "iter"])
df_temp <- melt(df_temp,  id.vars="iter", variable_name="r2")

ggplot(df_temp, aes(x=iter, y=value, group=r2)) + 
  geom_line(aes(color=factor(r2))) +
  labs(list(x="Iteration", y="R-Squared value")) + 
  scale_x_discrete(breaks=round(seq(0, iter, by=iter/5),1)) + 
  guides(col=guide_legend(title="", ncol=2)) +
  theme(legend.position="bottom")

dev.off()

#--------------------------------------------------------------------------
#Plot Adjusted Holdout R-Squared
#--------------------------------------------------------------------------
df_temp <- df_gof.train["adjr2"]
names(df_temp) <- c("Adj R2_train")
df_temp["Adj R2_test"] <- df_gof.test["adjr2"]
df_temp[, "iter"] <- rownames(df_temp)
df_temp[, "iter"] <- factor(df_temp[, "iter"], levels=df_temp[, "iter"])
df_temp <- melt(df_temp,  id.vars="iter", variable_name="r2")

ggplot(df_temp, aes(x=iter, y=value, group=r2)) + 
  geom_line(aes(color=factor(r2))) +
  labs(list(x="Iteration", y="Adjusted R-Squared Value")) + 
  scale_x_discrete(breaks=round(seq(0, iter, by=iter/5),1)) + 
  guides(col=guide_legend(title="", ncol=2)) +
  theme(legend.position="bottom")

dev.off()

#--------------------------------------------------------------------------
#Plot % Fall in Training set vs. the test set
#--------------------------------------------------------------------------
df_temp <- df_gof.train["r2"]
names(df_temp) <- c("R2_train")
df_temp["R2_test"] <- df_gof.test["r2"]
df_temp["Delta"] <- 1 - (df_temp["R2_test"] / df_temp["R2_train"])
df_temp[, c("R2_test", "R2_train")] <- NULL
df_temp[, "iter"] <- rownames(df_temp)
df_temp[, "iter"] <- factor(df_temp[, "iter"], levels=df_temp[, "iter"])
df_temp <- melt(df_temp,  id.vars="iter", variable_name="delta")

ggplot(df_temp, aes(x=iter, y=value, group=delta)) + 
  geom_line(aes(color=factor(delta))) +
  labs(list(x="iteration", y="Percent Fall in R2 (train vs. test)")) + 
  scale_x_discrete(breaks=round(seq(0, iter, by=iter/5),1)) + 
  guides(col=guide_legend(title="", ncol=2)) +
  theme(legend.position="bottom")

dev.off()

#--------------------------------------------------------------------------
#Part 5-6 Compute mean beta of all coefficients using the 1000 iterations
#--------------------------------------------------------------------------
objc <- trainControl(method="cv", number=3, 
                           returnResamp="none", allowParallel=TRUE, verboseIter=TRUE)
f_lms <- train(Amount ~ .,
                 data=cred,
                 method="lm", trControl=objc)

dftemp1 <- as.data.frame(coef(f_lms$finalModel))

cols <- colnames(df_coeff[, !sapply(df_coeff, is.factor)])
dftemp2 <- df_coeff[, cols]
stats <- lapply(dftemp2, function(x) rbind(mean=mean(x),
                                            min=min(x),
                                            q25=quantile(x, na.rm=TRUE)[2],
                                            q75=quantile(x, na.rm=TRUE)[4],
                                            max=max(x)))
dftemp2 <- t(data.frame(stats))
rownames(dftemp2) <- cols

dftemp <- cbind(dftemp1, dftemp2)
colnames(dftemp)[1] <- "All_Data_Fit"
kable(round(dftemp, digits=2))
rm(cols, dftemp, dftemp1, dftemp2, stats)

#--------------------------------------------------------------------------




