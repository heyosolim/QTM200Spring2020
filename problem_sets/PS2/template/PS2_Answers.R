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
library(ggplot2)

setwd("/Users/jasminlim/Documents/GitHub/QTM200Spring2020/problem_sets/PS2/template")

# Question 1

# a)
bribe_study <- matrix(c(14, 6, 7, 7, 7, 1), byrow = T, nrow = 2) # Recreating the table of data
bribe_study <- cbind(bribe_study, rowSums(bribe_study)) # Adding the total for rows as another column
bribe_study <- rbind(bribe_study, colSums(bribe_study)) # Adding the total for cols as another row
rownames(bribe_study) <- c("Upper class", "Lower class", "Total") # Row names
colnames(bribe_study) <- c("Not stopped", "Bribe requested", "Stopped/given warning", "Total") # Col names
bribe_study # Checking table

expected <- matrix(data = NA, nrow= 2, ncol = 3) # Matrix for expected values

# Calculating expected values and assigning expected value to each cell
for (i in 1:2) 
  for (j in 1:3) 
    expected[i, j] <- (bribe_study[i, 4] * bribe_study[3, j]) / bribe_study[3, 4]
expected # Checking expected values

observed <- bribe_study[-3, -4] # To make arrays comfortable
chisq <- (observed - expected)^2 / expected # Calculating each cell's contribution to TS
sum(chisq) # Chi-sq TS is 3.79

Xsq <- chisq.test(observed) # Checking answer
Xsq

# b)
rows = nrow(observed) # Number of rows
cols = ncol(observed) # Number of columns
pchisq(3.79, df = (rows-1)*(cols-1), lower.tail = F) # p-value = 0.1503

# c)
adjusted_resid <- matrix(data = NA, nrow= 2, ncol = 3) #Matrix for adjusted residuals

# Calculating adjusted residuals and assigning adjusted residual to each cell
for (i in 1:2) 
  for (j in 1:3) 
    adjusted_resid[i, j] <- (observed[i,j] - expected[i,j]) / sqrt(expected[i,j] * (1 - bribe_study[i, 4] / bribe_study[3,4]) * (1 - bribe_study[3, j] / bribe_study[3,4]))
adjusted_resid
  
Xsq$stdres # Checking answer

# Question 2
women <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header = T) # Importing dataset - can use the URL
summary(women) # Inspecting data

mean.x <- mean(women$reserved)
mean.y <- mean(women$water)
a <- sum((women$reserved - mean.x)*(women$water - mean.y)) 
b <- sum((women$reserved - mean(women$reserved))^2)
beta <- a / b # slope
beta # slope = 9.25

alpha <- mean.y - (beta * mean.x) # intercept
alpha # intercept = 14.74


model <- lm(water ~ reserved, data = women) # Checking answer
summary(model) # Summary of the fitted model

14.74 + 9.25

# Question 3

# 1.
fruitfly <- read.csv("fruitfly.csv", header = T) # Importing dataset
summary(fruitfly) # Summary statistics

pdf("fruitfly.pdf") # Plot for distribution of fruitfly lifespan
hist(fruitfly$lifespan, xlab = "Lifespan (days)", main = NULL)
dev.off()


# 2.
pdf("thorax_lifespan.pdf") # Plot for lifespan vs. thorax
plot(fruitfly$thorax, fruitfly$lifespan, main = NULL, xlab = "Thorax (mm)", ylab = "Lifespan (days)") # Moderately linear
dev.off()
cor(fruitfly$thorax, fruitfly$lifespan) # Correlation coefficient = 0.64

# 3. 
# Regress lifespan on thorax
mean.x <- mean(fruitfly$thorax) # Mean of x
mean.y <- mean(fruitfly$lifespan) # Mean of y
a <- sum((fruitfly$thorax - mean.x)*(fruitfly$lifespan - mean.y)) 
b <- sum((fruitfly$thorax - mean(fruitfly$thorax))^2)
beta <- a / b # slope
beta # slope = 144.33

alpha <- mean.y - (beta * mean.x) # intercept
alpha # intercept = -61.05

model2 <- lm(lifespan ~ thorax, data = fruitfly) # checking answer
summary(model2)

# 4.

# Pearson correlation 
pear_cor <- (cov(fruitfly$thorax, fruitfly$lifespan)) / (sd(fruitfly$thorax) * sd(fruitfly$lifespan))
pear_cor

cor(fruitfly$thorax, fruitfly$lifespan) # Checking answer

t_stat <- (pear_cor * sqrt(125 - 2)) / sqrt(1 - pear_cor^2) # Calculating the t-test statistic
t_stat

p_value <- 2 * pt(t_stat, 125 - 2, lower.tail = FALSE) # Calculating the p-value
p_value 

cor.test(fruitfly$thorax, fruitfly$lifespan) # Checking answer

# 5.

slope <- 144.33 # Slope
se <- 15.77 # Standard error (from R output in summary)
sample_size <- nrow(fruitfly)
t90 <- qt((1 - 0.9) / 2, lower.tail = F, df = sample_size - 2) # t-score for 90% conf interval
lower_90 <- slope - t90 * se # Lower bound
upper_90 <- slope + t90 * se # Upper bound
c90 <- c(lower_90, upper_90)
c90 # (118.2, 170.5)

confint(model2, level = 0.9) # Checking answer

# 6.
new_fruitfly <- data.frame(thorax = 0.8) # Creating new data
predict(model2, new_fruitfly, se.fit = T) # Running prediction: 54.51 days
prediction <- predict(model2, new_fruitfly, interval="prediction", level=0.95) # Prediction intervals
prediction # (27.38, 81.45)
confidence <- predict(model2, new_fruitfly, interval="confidence", level=0.95) # Confidence intervals
confidence # (51.92, 56.91)

# 7.

# First plot attempt
new_df <- cbind(fruitfly, prediction, row.names = NULL)
pdf("conf_interval2.pdf")
ggplot(new_df, aes(thorax, lifespan)) + geom_point() + geom_line(aes(y=lwr), color = "red", linetype = "dashed")+ geom_line(aes(y=upr), color = "red", linetype = "dashed")+ geom_smooth(method=lm, se=TRUE)
dev.off()

# Second plot attempt
newww_fruitfly <- fruitfly 
newww_fruitfly$thorax <- 0.8
pred <- predict(lm(fruitfly$lifespan~fruitfly$thorax), newdata = newww_fruitfly, interval = "prediction", level = 0.95)
pred

pdf("conf_interval.pdf")
#ggplot(fruitfly, aes(x=thorax, y=lifespan)) + geom_point() + geom_smooth(method=lm, se=TRUE)
new_df2 <- cbind(fruitfly, pred, row.names = NULL) # New dataframe
ggplot(new_df2, aes(thorax, lifespan)) + geom_point() + geom_line(aes(y=lwr), color = "red", linetype = "dashed")+ geom_line(aes(y=upr), color = "red", linetype = "dashed")+ geom_smooth(method=lm, se=TRUE)
dev.off()


