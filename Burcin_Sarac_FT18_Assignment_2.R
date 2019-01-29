setwd("E:/dersler/Statistics 1/assignment 4")
library(tidyverse)
library(psych)
library(corrplot)
library(car)
library(glmnet)
library(randtests)
library(lmtest)

#Q1
usdata <- read.table("usdata")
str(usdata)
summary(usdata)

#Q2
numvar <- c("PRICE", "SQFT", "AGE", "FEATS")
boolvar <- c("NE", "COR")
usdata[,numvar] <- apply(usdata[,numvar], 2, function(x) as.numeric(x))
usdata[,boolvar] <- apply(usdata[,boolvar], 2, function(x) as.factor(x))
usnum <- usdata[numvar]
usfac <- usdata[boolvar]
str(usdata)

#Q3
round(t(describe(usdata[1:4])),2)

par(mfrow=c(2,2))
for(i in 1:3){
  hist(usdata[,i], main=names(usdata)[i], xlab = names(usdata)[i])
}
n <- nrow(usdata)
plot(table(usdata[,4])/n, type='h', xlim=range(usdata[,4])+c(-1,1), main=names(usdata)[4], ylab='Relative frequency')

for(i in 5:6){
  tbl= table(usdata[i])
  tbl = cbind(tbl,round(prop.table(tbl),2))
  colnames(tbl) <- c(names(usdata)[i], "prob")
  print(tbl)
}

par(mfrow=c(1,1))
barplot(sapply(usfac,table)/n, col=4:8)
legend('topright', fil=4:8, legend=c('No','Yes'), ncol=2)

#Q4
pairs(usnum)
par(mfrow=c(1,1))
corrplot(cor(usnum), method = "number")

par(mfrow=c(1,3))
for(i in 2:4){
  plot(usnum[,i], usnum[,1], xlab=names(usnum)[i], ylab='Price')
  abline(lm(usnum[,1]~usnum[,i]))
}

par(mfrow=c(1,3))
for(i in 2:4){
  boxplot(usnum[,1]~ usnum[,i], xlab=names(usnum)[i], ylab='Price')
  abline(lm(usnum[,1]~usnum[,i]))
}

par(mfrow=c(1,2))
for(i in 1:2){
  boxplot(usnum[,1]~usfac[,i], xlab=names(usfac)[i], ylab='Price')
  abline(lm(usnum[,1]~usfac[,i]))
}

#Q5
model <- lm(PRICE~.,usdata)
summary(model)
anova(model)

#Q6
mnull <- lm(PRICE~1,usdata)
step(mnull, scope=list(lower=mnull,upper=model), direction='forward')
step(mnull, scope=list(lower=mnull,upper=model), direction='both')

#Q7
modelf <- lm(PRICE~SQFT+FEATS, usdata)
summary(modelf)

modelf2 <- lm(PRICE~SQFT+FEATS-1, usdata)
summary(modelf2)


#Q8
plot(modelf2, which = 2)
plot(modelf, which = 2)

Stud.residuals <- rstudent(modelf2)
yhat <- fitted(modelf2)
par(mfrow=c(1,2))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2)
ncvTest(modelf2)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(modelf2)~yhat.quantiles)
plot(rstudent(modelf2), type='l')
runs.test(modelf2$res)
dwtest(modelf2)
durbinWatsonTest(modelf2)

Stud.residuals <- rstudent(modelf)
yhat <- fitted(modelf)
par(mfrow=c(1,2))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2)

modelf3 <- lm(PRICE~SQFT-1, usdata)
summary(modelf3)
par(mfrow=c(1,1))
plot(modelf3, which = 2)
Stud.residuals <- rstudent(modelf3)
yhat <- fitted(modelf3)
par(mfrow=c(1,2))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2)

modelf4 <- lm(PRICE~SQFT, usdata)
summary(modelf4)
par(mfrow=c(1,1))
plot(modelf4, which = 2)
Stud.residuals <- rstudent(modelf4)
yhat <- fitted(modelf4)
par(mfrow=c(1,2))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2)

modell <- lm(log(PRICE)~SQFT+FEATS-1, usdata)
summary(modell)
par(mfrow=c(1,1))
plot(modell, which = 2)
Stud.residuals <- rstudent(modell)
yhat <- fitted(modell)
par(mfrow=c(1,2))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2)
ncvTest(modell)
residualPlots(modell)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(modell)~yhat.quantiles)
boxplot(rstudent(modell)~yhat.quantiles)

residualPlot(modell, type='rstudent')
residualPlots(modell, plot=F, type = "rstudent")
plot(rstudent(modell), type='l')

boxTidwell(PRICE~SQFT+FEATS,data=usdata)

modell2 <- lm(PRICE~SQFT+I(FEATS^(7/8))-1, usdata)
summary(modell2)
residualPlots(modell2, plot=F)
par(mfrow=c(1,1))
plot(modell2, which = 2)
Stud.residuals <- rstudent(modell2)
yhat <- fitted(modell2)
par(mfrow=c(1,2))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2)
ncvTest(modell2)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(modell2)~yhat.quantiles)
boxplot(rstudent(modell2)~yhat.quantiles)

residualPlot(modell2, type='rstudent')
residualPlots(modell2, plot=F, type = "rstudent")


plot(rstudent(modell2), type='l')
dwtest(modell2)
durbinWatsonTest(modell2)


round(vif(model),1)
round(vif(modell2),1)

#Q9
X <- model.matrix(model)[,-1]
lasso <- glmnet(X, usdata$PRICE)
plot(lasso, xvar = "lambda", label = T)
lasso1 <- cv.glmnet(X, usdata$PRICE, alpha = 1)
lasso1$lambda
lasso1$lambda.min
lasso1$lambda.1se
plot(lasso1)
coef(lasso1, s = "lambda.min")
coef(lasso1, s = "lambda.1se")
plot(lasso1$glmnet.fit, xvar = "lambda")
abline(v=log(c(lasso1$lambda.min, lasso1$lambda.1se)), lty =2)


