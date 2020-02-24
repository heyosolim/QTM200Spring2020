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

setwd("/Users/jasminlim/Documents/GitHub/QTM200Spring2020/problem_sets/PS4/template")

# Question 1

# Installing required packages
install.packages("car")
library("car")
data(Prestige)
help(Prestige)

# a)

# New factor variable (dummy variable)
# Recoding professional workerse as 1, blue/white collar workers as 0
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

# b)

# Linear model with prestige as outcome and income, professional, and the interaction of the two as predictors
lm1 <- lm(prestige ~ income + professional + income:professional, data = Prestige)
summary(lm1)

# c)

# Prediction equation 
# 21.14 + 0.0032 * income + 37.78 * professional - 0.0023 * income * professional

# d)

# Interpret coefficient for income
0.0032 - 0.0023

# 0.0032 is the slope associated with income for blue collar/white collar group.
# 0.0032 - 0.0023 is the slope associated with income for the professional group. 
# A 1 dollar increase in income is associated with a 0.0032 % increase in prestige for blue collar/white
# collar and a 0.0009 % increase in prestige for professionals.

# e) 

# Interpret coefficient for professional
# 37.78 is the effect associated with the professional group when controlling for income.
# The professional group exhibits, on average, 37.78 units more prestige than blue/white collar workers.

# f)

# What is the effect of a $1,000 increase in income on prestige score for professional
#occupations? In other words, we are interested in the marginal effect of income when
#the variable professional takes the value of 1. Calculate the change in ^y associated
#with a $1,000 increase in income based on your answer for (c).
# prestige 21.14 + 0.0032 * income + 37.78 * professional - 0.0023 * income * professional

# f)

prestige_0 <- 21.1423 + 37.7813 # 58.9236 units of prestige with $0 income
prestige_1000 <- 21.1423 + 0.0032 * 1000 + 37.7813 - 0.0023 * 1000 # 59.8236 units of prestige with $1000 income
prestige_1000 - prestige_0 # difference between the two -> 0.9 unit change in prestige with $1000 increase


# g)

##What is the effect of changing one's occupations from non-professional to professional
#when her income is $6,000? We are interested in the marginal effect of professional
#jobs when the variable income takes the value of 6000. Calculate the change in ^y
#based on your answer for (c).

# 21.14 + 0.0032 * income + 37.78 * professional - 0.0023 * income * professional


prestige_prof <- 21.1423 + 0.0032 * 6000 + 37.7813 - 0.0023 * 6000 # 64.3236 units of prestige for professional with $6000 income
prestige_bc <- 21.14 + 0.0032 * 6000 # 40.34 units of prestige for blue/white collar workers with $6000 income
prestige_prof - prestige_bc # 23.9836 units change in prestige between professionals and blue/white collar workers

# g)
# professional
prof <- 21.14 + 37.78 + (0.0032 - 0.0023) * 6000 # 64.32

# blue/white collar
collar <- 21.14 + 0.0032 *  6000 # 40.34

prof - collar # 23.98 unit increase in prestige between different professional groups when income is $6000.


# Question 2

# a)

Ho: B1 = 0; Ha; B1 != 0

n <- 30 # sample size
ts <- (0.042 - 0) / 0.016 # calculating the test statistic given the R outputs
p_value <- 2 * pt(abs(ts), df = n - 3, lower.tail = F) # calculating the p-value
p_value # 0.014

# Since p-value is less than 0.05, we reject the null hypothesis that the B1 is 0; having yard signs
# in the precinct affects vote share

# Question 3

# b)

Ho: B2 = 0; Ha = B2 != 0

n2 <- 76 # sample size
ts2 <- (0.042 - 0) / 0.013 # calculating the test statistic
p_value2 <- 2 * pt(abs(ts2), df = n2 - 3, lower.tail = F) # calculating the p-value
p_value2 # 0.0019

# Since p-value is less than 0.05, we reject the null hypothesis that the B1 is 0; having yard signs in 
# neighboring precincts also affects vote share for Cuccinelli

# c)

# coefficient constant term
0.302  # When there are no yard signs in precicnts/neighboring precincts the vote share for Cuccinelli was on average
30.2%

# d)

# According to the R2 which equals 0.094, 9.4% of the variation in vote share can be explained by the 
# current model. Therefore, there are other 




