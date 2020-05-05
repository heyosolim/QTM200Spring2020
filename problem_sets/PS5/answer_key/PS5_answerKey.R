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

lapply(c("faraway", "ggplot2", "car", "xtable"),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS5/answer_key")

#####################
# Problem A
#####################

# load data
gamble <- (data=teengamb)
# run regression on gamble with specified predictors
model1 <- lm(gamble ~ sex + status + income + verbal, data=gamble)

# open up plot of residuals and fitted values
pdf("constant_variance1.pdf")
plot(fitted(model1), resid(model1),
     ylab="Studentized Residuals", xlab="Fitted Values") 
abline(0, 0, lty=2)    
dev.off()

pdf("constant_variance2.pdf")
ggplot(model1, aes(.fitted, .resid)) + geom_point() + 
  stat_smooth(method="loess", col="red") +
  geom_hline(yintercept=0, col="black", linetype="dashed") + 
  labs(x="Fitted Values", y="Studentized Residuals") +
  theme_classic()
dev.off()

#####################
# Problem B
#####################

# create quantile comparison plot check normality of studentized residuals
pdf("QQplot.pdf")
qqPlot(model1, ylab="Studentized Residuals")
dev.off()

pdf("QQplot2.pdf")
ggplot(model1, aes(qqnorm(.stdresid)[[1]], .stdresid)) + 
  geom_point(na.rm = TRUE) +
  geom_abline(aes(qqline(.stdresid))) +
  labs(x="Theoretical Quantiles", y="Standardized Residuals") +
  theme_classic()
dev.off()

#####################
# Problem C
#####################

# observations of concern (h_i > 2*mean(h))
hat.concern <- hatvalues(model1) [which(hatvalues(model1) > 2 * mean(hatvalues(model1)))]
# plot h values
pdf("hat_values.pdf")
plot(hatvalues(model1), main="Figure 2")
# h values of concern
abline(h=2*mean(hatvalues(model1)), lty=2)
dev.off()

# To check for leverage points, we can look at the hat values of each observation (remember that hat values of concern are $h_i> 2\bar{h}$). Looking at figure 2 and table 1, there are at least 4 observations that may be exerting leverage on our linear estimation.

# \begin{center}
# Table 1
# 
# \begin{tabular}{rr}
# \hline
# Observation & Hat values\\ 
# \hline
# 31 & 0.24 \\ 
# 33 & 0.22 \\ 
# 35 & 0.31 \\ 
# 42 & 0.30 \\ 
# \hline
# \end{tabular}
# \end{center}

#####################
# Problem D
#####################
# Check for outliers

pdf("outlier_test.pdf")
outlierTest(model1)
dev.off()

#####################
# Problem E
#####################
# Check for influential points

# find Cook's distance for each observation
cook <- influence.measures(model1)[[1]][,8]
# observations w/ cook distance > 4 / (50 - 4 - 1)
cook.concern <- cook [which(cook > 4 / (50 - 4 - 1))]
# find cov.ratio
cov.ratio <-influence.measures(model1)[[1]][,7]
# observations w/ cov.ratio +/- 2 standard deviations from mean cov.ratio (~ 1.129)
cov.ratio.concern <- cov.ratio [which(cov.ratio > 1 + 2*sd(cov.ratio),
                                      cov.ratio < 1 - 2*sd(cov.ratio))]
# influence plot through car package
pdf("bubble_plot.pdf")
influencePlot(model1, scale=10, xlab="Hat-Values", ylab="Studentized Residuals")
dev.off()
