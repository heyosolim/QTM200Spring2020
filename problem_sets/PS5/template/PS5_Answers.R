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

lapply(c("faraway"),  pkgTest)
library(car)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS5/template")


#####################
# Problem 1
#####################

# load data
gamble <- (data=teengamb)

# run regression on gamble with specified predictors
model1 <- lm(gamble ~ sex + status + income + verbal, gamble)

# (a) Check the constant variance assumption for the errors by plotting the residuals versus 
# the fitted values.
pdf("plot1.pdf")
plot(model1)
abline(h=0,lty=2)
dev.off()


# plot(model1$fitted, model1$residuals)
# abline(h=0,lty=2)
# Based on the plot of residuals against fitted values, the constant variance assumption doesn't seem to 
# be met. The residuals fan out as the fitted values increase (especially the outliers after fitted value 
# of 50), and there is no random scatter around the x-axis.

# (b) Check the normality assumption with a Q-Q plot of the studentized residuals.
pdf("plot2.pdf")
qqnorm(rstudent(model1))
qqline(rstudent(model1))
dev.off()

# Based on the Q-Q plot of the studentized residuals, the normality assumption is satisfied for the most
# part except for the outliers that occur as the theoretical quantiles increase. 

# (c) Check for large leverage points by plotting the h values.
pdf("plot3.pdf")
plot(hatvalues(model1))
abline(h = 2 * 5 / 47) # Based on the thresholds and k (number of predictors) = 4
abline(h = 3 * 5 / 47)
dev.off()

# The four points with higher hat values above the drawn line have high leverage, meaning that they
# have the potential to influence the fitted model.

# (d) Check for outliers by running an outlierTest.
outlierTest(model1, row.names(gamble))

# Since the adjusted p-value for the largest studentized residual(?) is larger than 0.05,
# we would conclude that this model doesn't have any extreme residuals.

# (e) Check for influential points by creating a "Bubble plot" with the hat-values and studentized 
# residuals.

pdf("plot4.pdf")
plot(hatvalues(model1), rstudent(model1), type = "n")
cook <- sqrt(cooks.distance(model1))
points(hatvalues(model1), rstudent(model1), cex = 10 * cook / max(cook))
abline(h = c(-2, 0, 2), lty = 2)
abline(v = c(2, 3) * 5 / 47, lty = 2)
identify(hatvalues(model1), rstudent(model1), row.names(gamble))
dev.off()

# Based on the Bubble plot, there's an observation with a large studentized residual and a high Cook's 
# distance, but with a low leverage. There are few observations with higher hat values/leverage, but with
# a small studentized residual/Cook's distance. The bubble plot shows that we do not have extreme outliers,
# since no observation has both a large studentized residual and high leverage/hat value. 

# high leverage, large studentized residual --> high influence

