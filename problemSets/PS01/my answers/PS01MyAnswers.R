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
install.packages("stargazer")
library(stargazer)

getwd()
setwd('/Users/rosalielacroix/Documents/GitHub/StatsI_2025/problemSets/PS01/my answers')


#####################
# Problem 1
#####################
# Find a 90% confidence interval for the average student IQ in the school.
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

n <- length(y)
y_mean <- mean(y)
y_sd <- sd(y)
# Use t-test as the sample size is less than 30
t90 <- qt((1-.90)/2, lower.tail = FALSE, df = n-1)
lower_90 <- y_mean - (t90 * (y_sd/sqrt(n)))
upper_90 <- y_mean + (t90 * (y_sd/sqrt(n)))
confint90 <- c(lower_90, upper_90)
confint90


confint90_correct <- t.test(y, conf.level = 0.90, alternative = "two.sided")
confint90_correct

# With repeated sampling, 90% of confidence intervals will contain
# the population parameters 93.96, 102.92.

# Next, the school counselor was curious whether the average student IQ in her school
# is higher than the average IQ score (100) among all the schools in the country.
# Using the same sample, conduct the appropriate hypothesis test with α= 0.05

# Establish the null hypothesis
hnull <- 100 
df <- n - 1 # Small sample, less than 30
# Standard error of y; use sd(y) determined earlier and named y_sd
y_se <- y_sd/sqrt(n)
# Use t-test as sample size is less than 30; use mean(y) determined 
# earlier and named y_mean
t <- (y_mean - hnull)/y_se
# Compile values into a data frame to help legibility
# Use pt() to determine the p-value
df <- c(y_mean - hnull, y_se, t, pt(abs(t), df))
names(df) <- c("Diff in means", "Std Error", "t", "p-value")
hyp_test <- round(df, 3)
hyp_test

# We did not find significant evidence to reject the null hypothesis, 
# as the p-value was found to be greater than our α = 0.05.

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)
summary(expenditure)
View(expenditure)

plot(expenditure$Y, expenditure$X1)

# Please plot the relationships among Y, X1, X2, and X3 ? What are the correlations
# among them (you just need to describe the graph and the relationships among them)?

# Y/X1
plot(expenditure$X1,
     expenditure$Y,
     xlab="Personal Income",
     ylab="Expenditure on Shelters/Housing Assistance",
     main="The Relationship between personal income and spending
     on shelters/housing assistance in states (per capita)")

  # The graph shows there is a weak to moderate, positive, linear relationship between 
  # per capita personal income and per capita expenditure on shelters/
  # housing assistance in states. The reason the relationship is being described as
  # weak to moderate, is that there is a grouping of states that show a stronger
  # positive, linear relationship towards the lower/lower-middle end of the two variables,
  # but as both variables increase, this relationship becomes much weaker.

# Y/X2
plot(expenditure$X2,
     expenditure$Y,
     xlab="Number of Residents per 100,000 that are ”Financially Insecure",
     ylab="Per Capita Expenditure on Shelters/Housing Assistance",
     main="The Relationship Between States' Number of Residents 
     that are Financially Insecure and Spending on Shelters/Housing Assistance")

  # The graph shows that there is little correlation between states' number of residents
  # per 100,000 that are "financially insecure" and the amount of money that state is 
  # spending on shelters/housing assistance. Though the general correlation appears minimal,
  # it is important to note that there is a weak, negative, linear correlation between 
  # states that have less than 300 residents per 100,000 and their expenditure on shelters/
  # housing assistance. There also appears to be a weak, positive, linear correlation for
  # states that have greater than 400 residents per 100,000 and their expenditure on 
  # shelters/housing assistance.

# Y/X3
plot(expenditure$X3,
     expenditure$Y,
     xlab="Number of People per thousand Residing in Urban Areas",
     ylab="Per Capita Expenditure on Shelters/Housing Assistance",
     main="Relationship between States' Per Capita Expenditure on Shelters/
     Housing Assistance and the Number of People Residing in Urban Areas")

  # The graph shows that there is a weak, positive, linear correlation between 
  # the per capita expenditure on shelters/housing assistance in a state and it's
  # number of people per thousand residing in urban areas.

# X1/X2
plot(expenditure$X1,
     expenditure$X2,
     xlab="Per Capita Personal Income",
     ylab="Number of Residents per 100,000 that are Financially Insecure",
     main="Relationship between Personal Income and the
      Number of Residents that are Financially Insecure in States")

  # The graph shows that there is a very weak relationship between states'
  # per capita personal income and the number of residents that are financially 
  # there. In states where the per capita personal income is lower, a faint positive
  # correlation can be seen that begins to spread out as per capita personal income
  # increases.

# X1/X3
plot(expenditure$X1,
     expenditure$X3,
     xlab="Per Capita Personal Income",
     ylab="Number of People per thousand Residing in Urban Areas",
     main="Relationship between Personal Income and Number of People Residing
      in Urban Areas in States")

  # The graph shows that there is a weak to moderate, positive, linear relationship
  # between states' per capita personal income and the number of people per 
  # thousand residing in rural areas. The correlation weakens when the per capita
  # personal income reaches approximately 2300. There is an outlier from the general
  # trend that appears on the higher end of per capita personal income but falls 
  # very low in the number of people residing in urban areas. 

# X2/X3
plot(expenditure$X2,
     expenditure$X3,
     xlab="Number of Residents per 100,000 that are Financially Insecure",
     ylab="Number of People per thousand Residing in Urban Areas",
     main="Relationship between Number of Residents that are Financially Insecure
     and Number of People Residing in Urban Areas in States")

  # The graph shows that there is no correlation between the number of residents
  # that are financially insecure and the number of people residing in urban areas
  # across states. 

# Plot the relationship between Y and region
pdf("y_region_PS01.pdf")
boxplot(Y~Region, expenditure,
     xlab="Region",
     ylab="Y",
     main="Y vs. Region",
     xaxt = "n")
axis(1, at=1:4, labels=c("Northeast","North Central", "South", "West"))
means <- tapply(expenditure$Y, expenditure$Region, mean)
points(means,col="red",pch=18)
dev.off()

# On average, which region has the highest average?
  # The highest average is the West. 

# Plot the relationship between Y and X1, add region
plot(expenditure$X1,
     expenditure$Y,
     xlab="Per Capita Personal Income",
     ylab="Per Capita Expenditure on Shelters/Housing Assistance",
     main="The Relationship between Personal Income and Spending on Shelters/
     Housing Assistance in States")

# The graph shows there is a weak to moderate, positive, linear relationship between 
# per capita personal income and per capita expenditure on shelters/
# housing assistance in states. The reason the relationship is being described as
# weak to moderate, is that there is a grouping of states that show a stronger
# positive, linear relationship towards the lower/lower-middle end of the two variables,
# but as both variables increase, this relationship becomes much weaker.

pdf(file="last_question_PS01.pdf")
plot(expenditure$X1,
     expenditure$Y,
     type = "n",
     xlab="Per Capita Personal Income",
     ylab="Per Capita Expenditure on Shelters/Housing Assistance",
     main="Relationship between Personal Income and Spending on Shelters/
     Housing Assistance in States")
points(x = expenditure$X1[expenditure$Region == "1"],
       y = expenditure$Y[expenditure$Region == "1"],
       pch = 16, col = "deepskyblue")
points(x = expenditure$X1[expenditure$Region == "2"],
       y = expenditure$Y[expenditure$Region == "2"],
       pch = 15, col = "yellowgreen")
points(x = expenditure$X1[expenditure$Region == "3"],
       y = expenditure$Y[expenditure$Region == "3"],
       pch = 17, col = "deeppink3")
points(x = expenditure$X1[expenditure$Region == "4"],
       y = expenditure$Y[expenditure$Region == "4"],
       pch = 18, col = "black")
legend(1050, 120,
       col = c("deepskyblue","yellowgreen","deeppink3","black"),
       pch = c(16, 15, 17, 18),
       legend = c("Northeast", "North Central", "South", "West"))
dev.off()

  # Credit https://intro2r.com/custom_plot.html for helping me make my plot
  # look aesthetically much nicer.

