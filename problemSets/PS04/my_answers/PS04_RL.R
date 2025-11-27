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
lapply(c("car", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

install.packages("car")
library(car)
data(Prestige)
help(Prestige)
head(Prestige)

# Question 1
# (a) Create a new variable 'professional' by recoding the variable type so
# that professionals are coded as 1, and blue and white collar workers are
# coded as 0.
Prestige["professional"] <- NA
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

# (b) Run a linear model with prestige as an outcome and income, professional, 
# and the interaction of the two as predictors
model1 <- lm(prestige ~ income + professional + income:professional,
             data = Prestige)
summary(model1)
stargazer(model1,
          type = "latex",
          title = "Table of Coefficients",
          column.labels = "Model 1",
          covariate.labels = c("Income", "Professional", "Income:Professional"),
          dep.var.labels = "Prestige")

# (f) What is the eﬀect of a $1,000 increase in income on prestige score for 
# professional occupations? In other words, we are interested in the marginal 
# eﬀect of income when the variable professional takes the value of 1. 
# Calculate the change in ˆ y associated with a $1,000 increase in income based 
# on your answer for (c).
  # (c) y = 21.142 + 0.003income + 37.781prof - 0.002income*prof
income_0 <- 21.142 + 37.781
income_1000 <- 21.142 + 0.003*(1000) + 37.781*(1) - 0.002*(1000*1)
income_1000 - income_0

# (g) What is the eﬀect of changing one’s occupations from non-professional to 
# professional when her income is $6,000? We are interested in the marginal 
# eﬀect of professional jobs when the variable income takes the value of 6,000. 
# Calculate the change in ˆ y based on your answer for (c).
non_prof <- 21.142 + 0.003*(6000)
prof <- 21.142 + 0.003*(6000) + 37.781*(1) - 0.002*(6000*1)
prof - non_prof

# Question 2
# (a) Use the results from a linear regression to determine whether having these 
# yard signs in a precinct aﬀects vote share (e.g., conduct a hypothesis test 
# with α=.05).
beta1 <- 0.042
SE1 <- 0.016
n1 <- 30
# Number of variables
k1 <- 1
# Calculate t-statistic
TS1 <- (beta1 - 0)/SE1
# Calculate p-value for test statistic
p_value1 <- 2*pt(abs(TS1), n1-k1-1, lower.tail = F)

# (b) Use the results to determine whether being next to precincts with these 
# yard signs aﬀects vote share (e.g., conduct a hypothesis test with α=.05).
beta2 <- 0.042
SE2 <- 0.013
n2 <- 76
# Number of variables
k2 <- 1
# Calculate t-statistic
TS2 <- (beta2 - 0)/SE2
# Calculate p-value for test statistic
p_value2 <- 2*pt(abs(TS2), n2-k2-1, lower.tail = F)

# (d) Evaluate the model fit for this regression. What does this tell us about
# the importance of yard signs versus the other factors that are not modeled?
r_squared <- 0.094
n <- 131
# k = number of variables
k <- 2
# F statistic using the r_squared value
F_test <- (r_squared/k)/((1-r_squared)/(n-k-1))
# F test requires 2 degrees of freedom
df1 <- k
df2 <- n-k-1
# Calculate p-value for test statistic
F_pvalue <- pf(F_test, df1, df2, lower.tail=FALSE)
?pf
