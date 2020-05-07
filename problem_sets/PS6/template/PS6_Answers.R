#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)
lapply(c(),  pkgTest)
library("nnet")
library("MASS")

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS6/template")

# Question 1

cholestrol <- read.csv("cholesterol.csv") # Importing cholestrol dataset

# 1a)
lm1 <- lm(cholCat ~ sex + fat, data = cholestrol) # Additive model for predicting cholestrol category based on sex and fat intake
lm1 # Equation: Yi = -0.130 + 0.189 * sex + 0.008 * fat 
summary(lm1) # Summary output
# The summary of the linear model shows that the global null hypothesis is rejected, as the individual null
# hypotheses for B0 = 0, B1 = 0, B2 = 0 are all rejected since the p-value is less than the alpha value 
# of 0.05. According to the linear model, males are more likely to have higher cholestrol since the 
# coefficient attached to the sex variable is 0.189. Moreover, higher fat intake contributes to higher
# cholestrol since the coefficient attached to the fat variable is 0.008. Therefore, an individual's 
# sex and fat intake are related to the cholestrol category of the individual.

# 2a) Based on the coefficient, increasing fat intake by 1 gram per day for women increases their
# odds of being in the high cholestrol group by 0.008.

# 2b) Based on the coefficient, increasing fat intake by 1 gram per day for men increases their odds of 
# being in the high cholestrol group by 0.189 + 0.008 = 0.197. 

# 2c) The estimated probability of a woman with a fat intake of 100 grams per day being in the high cholestrol
# group is-0.130 + 0.189 * 0 + 0.008 * 100 = 0.67.
-0.130 + 0.189 * 0 + 0.008 * 100

# 2d)
lm2 <- lm(cholCat ~ sex + fat + sex:fat, data = cholestrol) #  model including the interaction term
lm2 # Equation: Yi = -0.152 + 0.391 * sex + 0.008544 * fat + 0.002 * sex * fat
summary(lm2) # Summary output
# Based on the output, the p-value of the coefficient for the interaction term is 0.272. Since this is greater
# than 0.05, we fail to reject the null hypothesis that the coefficient is 0, meaning that there is no
# significant interaction between the two explanatory variables that the answers to 2a and 2b change
# drastically

# Question 2
# 1)
GDP <- read.csv("gdpChange.csv", stringsAsFactors = F) # Importing dataset
GDP$GDPWdiff <- as.factor(GDP$GDPWdiff) # Changing to factor variable
GDP$GDPWdiff <- relevel(GDP$GDPWdiff, ref = "no change")
multi_model1 <- multinom(GDPWdiff ~ REG + OIL, data = GDP) # Constructing unordered multinomial logit
summary(multi_model1) # Summary
exp(coef(multi_model1)[,c(1:3)]) # Exponentiate coefficients

# Interpretation


# 2)
multi_model2 <- polr(GDPWdiff ~ REG + OIL, data = GDP, Hess = T) # Constructing ordered multinomial logit
summary(multi_model2) # Summary
exp(cbind(OR = coef(multi_model2), confint(multi_model2))) # Odds ratios and CI

# Interpretation
The coefficient REG for countries with a GDP decrease is 3.972, while the coefficient is 5.865 for
countries with GDP growth. Therefore, democratic countries are more likely to have GDP
growth than undemocratic ones. On the other hand, coefficient OIL for countries with a GDP decrease is 119.578, while the
coefficient is 97.156 for countries with positive GDP growth. Therefore, countries that export on average
more than half of their exports in fuel are more likely to have a decrease in GDP

# Interpretation
For countries that are democratic, the odds of having positive GDP growth is 1.507 times greater than
for countries that are not democratic. For countries that export more than 50% of total export in oil, 
the odds of having positive GDP growth is 0.836 times greater than for countries that export less than 50%.






