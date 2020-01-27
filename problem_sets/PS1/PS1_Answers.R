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

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS1")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
t90 <- qt((1 - 0.9) / 2, lower.tail = FALSE, df = 24) # t-score for 90% confidence interval with 24 degrees of freedom
n <- length(y) # sample size
sample_mean <- mean(y) # sample mean
sample_sd <- sd(y) # sample standard deviation
sample_se <- sample_sd / sqrt(n) # standard error
lower_90 <- sample_mean - (t90 * sample_se) # lower bound of 90% confidence interval
upper_90 <- sample_mean + (t90 * sample_se) # upper bound of 90% confidence interval
confint <- c(lower_90, upper_90)
confint # 90% confidence interval

#####################
# Problem 2
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
t.test(y, mu = 100) # Conducting one sample t-test 
# Since the p-value of 0.5569 is greater than the alpha of 0.05, we fail to reject the Ho.
# We conclude that the true national mean IQ in schools is not significantly different from 100.
# We are 95% confident that the true national mean IQ in schools is between 93.04 and 103.84, and since
# 100 is included in this interval, there is not sufficient evidence to conclude that the true mean is
# significantly different from 100.

#####################
# Problem 3
#####################

y <- c(1, 2, 1, 3, 4, 1, 1, 4, 2, 1, 3, 4, 3, 2, 1, 3, 4, 1, 2, 3, 1, 1, 2, 1, 1, 3, 4)
yNew <- y # Creating new variable for class year

# Converting the numbers to class year
yNew[y==1]<-"Freshman"
yNew[y==2]<-"Sophomore"
yNew[y==3]<-"Junior"
yNew[y==4]<-"Senior"
table(yNew) # Checking Recoding

expenditure <- read.table("expenditure.txt", header=T)

# Plot relationships between Y - X3 with scatterplots

# Plot Y vs region
boxplot(expenditure$Y ~ expenditure$Region, names = c("Northeast", "North Central", "South", "West"), main = "Expenditure on Public Education vs. Geographical Region", xlab = "Geographical Region", ylab = "Per Capita Expenditure (Dollars)")

# Plot Y vs. X1
plot(expenditure$X1, expenditure$Y, log(expenditure$Region), main = "Expenditure on Public Education vs. Personal Income", xlab = "Per Capita Personal Income (Dollars)", ylab = "Per Capita Expenditure (Dollars)")
