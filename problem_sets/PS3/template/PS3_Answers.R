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

setwd("/Users/jasminlim/Documents/GitHub/QTM200Spring2020/problem_sets/PS3/template")

# Question 1

incumbents <- read.csv("incumbents_subset.csv") # Importing dataset

# 1.

# Running linear regression of vote share of incumbent on diff in campaign spending
reg_model_q1 <- lm(voteshare ~ difflog, data = incumbents)
reg_model_q1
summary(reg_model_q1) # Summary statistics for the model

# Question doesn't specify units!!!!

# Using the estimated coefficients and the summary statistics from the linear regression model, 
# we can calculate the fitted model as yhat = 0.579 + 0.042x. The slope of the fitted model is 0.042
# and we can interpret the slope as such: when the difference in campaign spending between incumbents
# and challengers increases by 1%, the incumbent's vote share increases by 4.2%. The intercept of the 
# fitted model is 0.579 and we can interpret it as such: when the 
# difference in campaign spending between incumbents and challengers is 0. the incumbent's vote share will
# be approximately 57.9%

# 2.

# Plot of diff in campaign spending vs vote share of incumbent
pdf("plotq1.pdf")
plot(incumbents$difflog, incumbents$voteshare, xlab = "Difference in Campaign Spending (%)", ylab = "Incumbent's Vote Share (%)")
abline(reg_model_q1) # Adding regression line
dev.off()
cor(incumbents$difflog, incumbents$voteshare) # Correlation coefficient = 0.606

# 3. 
q1_resid <- reg_model_q1$residuals # Storing residuals as separate object

# 4. 
# yhat = 0.579 + 0.042x

# Question 2

# 1.

# Running linear regression of vote share of the presidential candidate of the incumbent's party on diff in campaign spending
reg_model_q2 <- lm(presvote ~ difflog, data = incumbents)
reg_model_q2
summary(reg_model_q2) # Summary statistics for the model

# Using the estimated coefficients and the summary statistics from the linear regression model, 
# we can calculate the fitted model as yhat = 0.508 + 0.024x. The slope of the fitted model is 0.024
# and we can interpret the slope as such: when the difference in campaign spending between incumbents
# and challengers increases by 1%, the vote share of the presidential candidate of the incumbent's party
# increases by 2.4%. The intercept of the fitted model is 0.508 and we can interpret it as such: when 
# the difference in campaign spending between incumbents and challengers is 0. the presidential
# candidate's vote share will be approximately 50.8%

# 2.

# Plot of diff in campaign spending vs vote share of the presidential candidate of the incumbent's party
pdf("plotq2.pdf")
plot(incumbents$difflog, incumbents$presvote, xlab = "Difference in Campaign Spending (%)", ylab = "Vote Share of Pres. Candidate of Incumbent's Party (%)")
abline(reg_model_q2) # Adding regression line
dev.off()
cor(incumbents$difflog, incumbents$presvote) # Correlation coefficient = 0.297

# 3. 
q2_resid <- reg_model_q2$residuals # Storing residuals as separate object

# 4. 
# yhat = 0.508 + 0.024x

# Question 3

# 1.

# Running linear regression of incumbent's vote share on vote share of the presidential candidate of the incumbent's party
reg_model_q3 <- lm(voteshare ~ presvote, data = incumbents)
reg_model_q3
summary(reg_model_q3) # Summary statistics for the model

# Using the estimated coefficients and the summary statistics from the linear regression model, 
# we can calculate the fitted model as yhat = 0.441 + 0.388x. The slope of the fitted model is 0.388
# and we can interpret the slope as such: when the vote share of the presidential candidate of the
# incumbent's party increases by 1%, the incumbent's vote share increases by 38.8%. The intercept 
# of the fitted model is 0.441 and we can interpret it as such: when vote share of the presidential candidate
# is zero, the incumbent's vote share will be approximately 44.1%.

# 2.

# Plot of vote share of the presidential candidate of the incumbent's party vs incumbent's vote share
pdf("plotq3.pdf")
plot(incumbents$voteshare, incumbents$presvote, xlab = "Vote Share of Pres. Candidate of Incumbent's Party (%)", ylab = "Incumbent's Vote Share (%)")
abline(reg_model_q3) # Adding regression line
dev.off()
cor(incumbents$voteshare, incumbents$presvote) # Correlation coefficient = 0.454

# 3. 
q3_resid <- reg_model_q3$residuals # Storing residuals as separate object

# 4. 
# yhat = 0.441 + 0.388x

# Question 4

# 1.

# Running linear regression of residuals from Q1 on residuals from Q2
reg_model_q4 <- lm(q1_resid ~ q2_resid, data = incumbents)
reg_model_q4
summary(reg_model_q4) # Summary statistics for the model

# 2.

# Plot of the two residuals
pdf("plotq4.pdf")
plot(q2_resid, q1_resid, xlab = "Residuals from Question 2", ylab = "Residuals from Question 1")
abline(reg_model_q4) # Adding regression line
dev.off()
cor(q2_resid, q1_resid) # Correlation coefficient = 0.361

# 3. 
# yhat = -4.86e-18 + 0.26x

# Question 5 

# 1. 

# Running linear regression of voteshare on difflog and presvote
reg_model_q5 <- lm(incumbents$voteshare ~ incumbents$difflog + incumbents$presvote)
reg_model_q5
summary(reg_model_q5) # Summary statistics of the model

# 2.
# yhat = 0.449 + 0.036x1 + 0.257x2

# 3.
# The residuals are the same because
  






