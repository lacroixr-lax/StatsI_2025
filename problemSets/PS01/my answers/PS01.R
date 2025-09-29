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

lapply(c("stringr"),  pkgTest)

#####################
# Problem 1
#####################
# Find a 90% confidence interval for the average student IQ in the school.
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

z90 <- qnorm((1-.90)/2, lower.tail = FALSE)
n <- length(y)
y_mean <- mean(y)
y_sd <- sd(y)
lower_90 <- y_mean - (z90 * (y_sd/sqrt(n)))
upper_90 <- y_mean + (z90 * (y_sd/sqrt(n)))
confint90 <- c(lower_90, upper_90)
confint90

# With repeated sampling, 90% of confidence intervals will contain
# the population parameters 94.13, 102.74.

# Next, the school counselor was curious whether the average student IQ in her school
# is higher than the average IQ score (100) among all the schools in the country.
# Using the same sample, conduct the appropriate hypothesis test with α= 0.05

mo <- 100 # Hnull mean
df <- n - 1 # small sample, less than 30
t_test_by_hand <- function(y, mo = 100){
  t <- (y_mean - mo)/y_sd
  dat <- c(y_mean - mo, y_sd, t, pt(abs(t), df))
  names(dat) <- c("Diff in means", "Std Error", "t", "p-value")
  return(round(dat, 3))
}
t_test_by_hand(y)

# We did not find significant evidence to reject the null hypothesis, 
# as the p-value was found to be greater than our α= 0.05.

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)
summary(expenditure)
View(expenditure)
