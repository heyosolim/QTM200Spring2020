#################################
# Lab 11: Dummary variables
#################################

## Goal:
# 1. Estimate linear regressions with multiple variables
# some being categorical
# 2. Correctly interpret coefficients


# Socio-economic data was collected for both smokers and non-smokers
# Each respondent reported their age, how much they smoked, and gender

# A model was fitted to the data using "amtWeekdays" as the response,
# and income and gender as predictors
# Note: A dummy variable Di was created with female=0 and male=1

# Consider the common-slope model:
#  Yi = β0+ βageXi + βgenderDi + εi

# load and/or download necessary packages
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}
lapply(c("openintro"),  pkgTest)

# load data
data(smoking)
View(smoking)

# (1) What is the fitted model for men?
ifelse(smoking$gender == "Male", 1, 0)
model <- lm(smoking$amtWeekdays ~ smoking$age + smoking$gender)
summary(model)

# y = 7.48 + 0.108Xi + 3.75Di

# (2) What is the fitted model for women?
# y = 7.48 + 0.108Xi

# (3) Based on the estimated coefficients, does it look like men or women exhibit higher levels of smoking at every age? Justify your answer.
# Men exhibit higher levels of smoking because it has a coefficient attached (male = leve)

# (4) The individuals in this study ranged in age from 16-97 years old. Do peope increase or decrease their volume of smoking as they get older? Justify your answer.