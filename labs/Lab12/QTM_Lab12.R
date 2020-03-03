#####################################################
## File: Lab12.R                                   ##
## Regression Diagnostics in R                     ##
#####################################################


# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/lab12/")

## Goals:
## 1. Regression diagnostics

library("faraway")
data("star")
help(star)
colnames(star)

# For an illustrative purpose, we drop two observations
star <- star[-c(11,20),]

# Change index and row numbers
star$index <- row.names(star) <- 1:nrow(star)


# The data look like this
plot(star$temp, star$light,
     xlab="Temperature", ylab="Light Intensity",
     type="n")
text(star$temp, star$light, labels=star$index)


# Bivariate model
model1 <- lm(light ~ temp, data=star)
summary(model1)


# Estimated line
abline(model1, col="firebrick1", lwd=2)


# Check the residuals. Do they look more or less random?
plot(residuals(model1) ~ fitted(model1), data=star)
abline(h=0)

# Use absolute residuals
plot(abs(residuals(model1)) ~ fitted(model1), data=star)
abline(h=0)


install.packages("car")
library("car") # a package with lots of diagnostic tools


# Normality of Residuals
qqPlot(model1)


# Outliers
outlierTest(model1)


# Cook's distance and influential observations
cooks.dist <- cooks.distance(model1)
plot(cooks.dist)
text(c(28,32), cooks.dist[c(28,32)], labels=c(28,32))


# Influential observations
influencePlot(model1,
              sub="Circle size is proportial to Cook's Distance")


# What if we drop 28 and 32?
model2 <- lm(light ~ temp, data=star[-c(28,32),])
summary(model2)


# Let's compare Models 1 and 2
plot(star$temp, star$light,
     xlab="Temperature", ylab="Light Intensity",
     type="n") # type="n" does not plot anything
text(star$temp, star$light, labels=star$index)

# Model 1
abline(model1, col="firebrick1", lwd=2)

# Model 2 (without 28 and 32)
abline(model2, col="maroon1", lwd=2)


# What if we include a dummy variable for 28 and 32?
star$outliers <- 0
star$outliers[c(28,32)] <- 1

model3 <- lm(light ~ temp + outliers, data=star)
summary(model3)


# Get the coefficients
coef(model3)

# What are these?
abline(a=coef(model3)[1], b=coef(model3)[2], 
       lwd=2, col="mediumorchid1")

abline(a=coef(model3) + coef(model3)[3], b=coef(model3)[2], 
       lwd=2, col="indianred1")


# Check the residuals from Model 3. Do they look more or less random?
plot(residuals(model3) ~ fitted(model3), data=star)
abline(h=0)

# Use absolute residuals
plot(abs(residuals(model3)) ~ fitted(model3), data=star)
abline(h=0)


# Quick diagnostics
plot(model3)



#### Group Work ####

# In his 1998 paper, "Women's Representation in National 
# Legislatures: Developed and Developing Countries," Richard 
# Matland seeks to describe the conditions influencing the 
# representation of women in national legislatures. With data 
# gathered from a sample of 40 countries, Matland ran an ordinary 
# least squares regression model to examine the statistical 
# significance of six commonly identified causal variables.

matland <- read.csv("matland.csv")
names(matland)

# Outcome variable:
# percent.women: the percentage of female legislators in the 
# national assembly in each country in 1995

# Independent Variables:
# system: an electoral system dummy variable (this equals 1 if a 
#         country has a proportional representation system, 0 if 
#         the system is majoritarian)
# right.vote: the percent of parliamentary seats held by right-wing 
#             parties
# women.ed: the percent of women with some college education
# women.work: women's labor force participation rate
# culture: a cultural factor score measuring women's standing 
#          within the country
# development: a measure of the level of development


# 1. Run a bivariate linear regression model that includes just 
#    one independent variable, women's labor force participation




# 2. Check the residuals. Is there a problem of non-constant variance?




# 3. Repeat 2 and 3 including right.vote, women.ed, culture, 
#    development, and system. Is there a problem of non-constant 
#    variance?