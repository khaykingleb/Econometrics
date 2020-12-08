#Part 1

library(tidyverse)
library(haven)
data <- read_dta('clothing.dta')

#model estimation
model <- lm(data = data, sales ~ hoursw + ssize)
diag_model <- fortify(model)

#saving residuals
resid <- diag_model$.resid

#histogram
qplot(resid)

#Q-Q plot
qqnorm(resid)

#Kolmogorov-Smirnov test
ks.test(resid, pnorm)

#Shapiro-Wilk test
shapiro.test(resid)

#standardized residuals plot
ggplot(data=diag_model, aes(x = 1:nrow(diag_model), y = .stdresid)) + geom_point()
stdres <- diag_model$.stdresid

#model without outliers estimation
model_res <- lm(data$sales ~ data$hoursw + data$ssize, subset = (stdres > -2 & stdres < 2))
  
#median regression
install.packages("quantreg")
library("quantreg")
model_m <- rq(data=data, sales~hoursw+ssize, tau = 0.5)

#comparison
summary(model)
summary(model_res)
summary(model_m)

#Part 2

data <- read_dta('Dougherty.dta')

#model estimation
model <- lm(EARNINGS ~ S + ASVABC, data = data)
plot(model)

#Box-Cox test
install.packages('MASS')
library(MASS)
bc <- boxcox(model)
bc <- boxcox(model, lambda = seq(-1, 1))

#optimal lambda
best.lam <- bc$x[which(bc$y==max(bc$y))]
best.lam

semilog_model <- lm(log(EARNINGS) ~ S + ASVABC, data = data)
plot(semilog_model)

##PE test
library(lmtest)
#linear vs semilog
model <- lm(EARNINGS ~ S + ASVABC, data = data)
semilog_model <- lm(log(EARNINGS) ~ S + ASVABC, data = data)
petest(model, semilog_model)

#linear vs log
model <- lm(EARNINGS ~ S + ASVABC, data = data)
log_model <- lm(log(EARNINGS) ~ log(S) + log(ASVABC), data = data)
petest(model, log_model)

##Test Bera-McAleer (linear vs semilog)
#models estimation
model <- lm(EARNINGS ~ S + ASVABC, data = data)
semilog_model <- lm(log(EARNINGS) ~ S + ASVABC, data = data)

#saving fitted values for semilog model
diag_semilogmodel <- fortify(semilog_model)
head(diag_semilogmodel)
y1 <- exp(diag_semilogmodel$.fitted)

#estimating auxiliary regression 
y1_model <- lm(y1 ~ data$S + data$ASVABC)

#saving residuals
res1 <- resid(y1_model)

#saving fitted values for linear model
diag_model <- fortify(model)
y2 <- exp(diag_model$.fitted)

#estimating auxiliary regression 
y2_model <- lm(y2 ~ data$S + data$ASVABC)

#saving residuals
res2 <- resid(y2_model)

#estimating auxiliary regression 
model_for_semilog <- lm(log(data$EARNINGS) ~ data$S + data$ASVABC + res1)
model_for_lin <- lm(data$EARNINGS ~ data$S + data$ASVABC + res2)

#look at significance of coefficients before res1 and res2
summary(model_for_semilog)
summary(model_for_lin)

#Zarembka test (semilog vs linear)

##geometric mean calculation
library(psych)
geometric.mean(data$EARNINGS)

##new variables
EARNINGSstar <- data$EARNINGS/16.3442
logEARNINGSstar <- log(EARNINGSstar)

#auxiliary model estimation
model_star <- lm(EARNINGSstar ~ data$S + data$ASVABC)

#saving RSS
library(qpcR)
RSS1 <- RSS(model_star)

model_logstar <- lm(logEARNINGSstar ~ data$S + data$ASVABC)
RSS2 <- RSS(model_logstar)

#chi-squared observable
chi2 <- 0.5 * 540 * abs(log(RSS2/RSS1))
chi2
