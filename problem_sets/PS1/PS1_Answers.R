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

install.packages("Rmisc")
library(Rmisc)

install.packages("ggplot2")
library(ggplot2)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS1")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
t90 <- qt((1 - 0.9) / 2, lower.tail = FALSE, df = 24) # T-score for 90% confidence interval with 24 degrees of freedom
n <- length(y) # Sample size
sample_mean <- mean(y) # Sample mean
sample_sd <- sd(y) # Sample standard deviation
sample_se <- sample_sd / sqrt(n) # Standard error
lower_90 <- sample_mean - (t90 * sample_se) # Lower bound of 90% confidence interval
upper_90 <- sample_mean + (t90 * sample_se) # Upper bound of 90% confidence interval
confint <- c(lower_90, upper_90)
confint # 90% confidence interval (93.96, 102.92)

t.test(y, conf.level = 0.9) # checking answer

#####################
# Problem 2
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
tstat <- (sample_mean - 100) / sample_se # Calculating test statistic 
pvalue <- pt(tstat, df = 24, lower.tail = F) # Calculating the p-value for a one-tailed test
pvalue # 0.7215; greater than the alpha value of 0.05

t.test(y, mu = 100, alternative = "greater") # Checking answer

# Since the p-value of 0.7215 is greater than the alpha value of 0.05, we fail to reject the Ho.
# Therefore, we cannot conclude that the true national mean IQ in schools is higher than 100.

#####################
# Problem 3
#####################

y <- c(1, 2, 1, 3, 4, 1, 1, 4, 2, 1, 3, 4, 3, 2, 1, 3, 4, 1, 2, 3, 1, 1, 2, 1, 1, 3, 4)
yNew <- y # Creating new variable for class year

# Converting the numbers to class year
yNew[y==1] <-"Freshman"
yNew[y==2] <-"Sophomore"
yNew[y==3] <-"Junior"
yNew[y==4] <-"Senior"
table(yNew) # Checking recoding

expenditure <- read.table("expenditure.txt", header=T)

# Expenditure vs. Residents under 18 years old
pdf("Graph1.pdf")
plot(expenditure$X2, expenditure$Y, cex.main = 1, main = "Expenditure on Public Education vs. Number of Underage Residents", xlab = "Number of Underage Residents (per 1000)", ylab = "Per Capita Expenditure (Dollars)")
dev.off()

# Expenditure vs. Number of urban residents
pdf("Graph2.pdf")
plot(expenditure$X3, expenditure$Y, cex.main = 1, main = "Expenditure on Public Education vs. Number of Urban Residents", xlab = "Number of Residents in Urban Areas (per 1000)", ylab = "Per Capita Expenditure (Dollars)")
dev.off()

# Expenditure vs. Region
pdf("Graph3.pdf")
boxplot(expenditure$Y ~ expenditure$Region, names = c("Northeast", "North Central", "South", "West"), main = "Expenditure on Public Education vs. Geographical Region", xlab = "Geographical Region", ylab = "Per Capita Expenditure (Dollars)")
dev.off()

expenditure$newRegion <- factor(NA, c("Northeast", "North Central", "South", "West"))
expenditure$newRegion[expenditure$Region == 1] <- "Northeast"
expenditure$newRegion[expenditure$Region == 2] <- "North Central"
expenditure$newRegion[expenditure$Region == 3] <- "South"
expenditure$newRegion[expenditure$Region == 4] <- "West"

summarySE(data = expenditure, measurevar = "Y", groupvars = "newRegion")

# Expenditure vs. Income
pdf("Graph4.pdf")
plot(expenditure$X1, expenditure$Y, main = "Expenditure on Public Education vs. Personal Income", xlab = "Per Capita Personal Income (Dollars)", ylab = "Per Capita Expenditure (Dollars)")
dev.off()

# Replotting Expenditure vs. Income color-coded by region
pdf("Graph5.pdf")
Graph5 <- ggplot(expenditure, aes(X1, Y,colour = newRegion)) + geom_point()
print(Graph5 + labs(colour = "Region", title = "Expenditure on Public Education vs. Personal Income by Region", x = "Per Capita Personal Income (Dollars)", y = "Per Capita Expenditure (Dollars)"))
dev.off()