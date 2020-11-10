library(foreign)
data <- read.dta("Dougherty.dta")

library(tidyverse)

#regression estimation
model1 <- lm(data=data, S~ASVABC+SM+SF)

#checking model adequacy (F-stat) 
summary(model1)

##testing hypothesis that coefs before SM and SF 
#are equal to zero (b3=b4=0 against b3^2+b4^2>0)
model1_restricted <- lm(data=data, S~ASVABC)
anova(model1_restricted, model1)

##testing hypothesis that coefs before SM and SF 
#are equal (b3=b4 against b3/=b4)
install.packages("car")
library(car)
linearHypothesis(model1, "SM-SF=0")

#testing hypothesis about right weights choice 
model2 <- lm(data=data, S~ASVAB02+ASVAB03+ASVAB04+SM+SF)
linearHypothesis(model2, "ASVAB02-2*ASVAB03=0")
linearHypothesis(model2, "ASVAB02-2*ASVAB04=0")
