#######
# PS02 My Answers
#######

getwd()
setwd("/Users/rosalielacroix/Documents/GitHub/StatsI_2025/problemSets/PS02/my_answers")

rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("readr", "ggplot2"),  pkgTest)

# Question 1
# (a) Calculate the χ2 test statistic by hand/manually.

# Establish null hypothesis:
  # The variables of class and solicitation of bribe are statistically
  # independent.

# Create a contingency table with the data
cont_table <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, ncol = 3, byrow = TRUE)
rownames(cont_table) <- c("Upper Class", "Lower Class")
colnames(cont_table) <- c("Not Stopped", "Bribe Requested", "Stopped/Given Warning")

# Chi Square by hand
chi_square_by_hand <- function(x) {
  if (!is.matrix(x)) {
    stop()
  } else {
    
    # Expected Frequencies
    fe_1 <- (sum(x[,1])/sum(x))*sum(x[1,])
    fe_2 <- (sum(x[,2])/sum(x))*sum(x[1,])
    fe_3 <- (sum(x[,3])/sum(x))*sum(x[1,])
    fe_4 <- (sum(x[,1])/sum(x))*sum(x[2,])
    fe_5 <- (sum(x[,2])/sum(x))*sum(x[2,])
    fe_6 <- (sum(x[,3])/sum(x))*sum(x[2,])
    
    # Observed Frequencies
    fo_1 <- x[1,1]
    fo_2 <- x[1,2]
    fo_3 <- x[1,3]
    fo_4 <- x[2,1]
    fo_5 <- x[2,2]
    fo_6 <- x[2,3]
    
    # (Observed - expected)^2
    fo_fe_sq1 <- (fo_1-fe_1)^2
    fo_fe_sq2 <- (fo_2-fe_2)^2
    fo_fe_sq3 <- (fo_3-fe_3)^2
    fo_fe_sq4 <- (fo_4-fe_4)^2
    fo_fe_sq5 <- (fo_5-fe_5)^2
    fo_fe_sq6 <- (fo_6-fe_6)^2
    
    # ((Observed - expected)^2)/expected
    chi_square1 <- fo_fe_sq1/fe_1
    chi_square2 <- fo_fe_sq2/fe_2
    chi_square3 <- fo_fe_sq3/fe_3
    chi_square4 <- fo_fe_sq4/fe_4
    chi_square5 <- fo_fe_sq5/fe_5
    chi_square6 <- fo_fe_sq6/fe_6
    
    # Chi-square statistic
    chi_square <- chi_square1 + chi_square2 + chi_square3 + chi_square4 +
      chi_square5 + chi_square6
  }
  print(chi_square)
}
chi_square <- chi_square_by_hand(cont_table)

# (b) Now calculate the p-value from the test statistic you just created (in R) 
# What do you conclude if α= 0.1?
p_value <- pchisq(chi_square, df=(2-1)*(3-1), lower.tail=FALSE)
p_value

  # P-value = 0.1502306. We conclude that there is not sufficient evidence to reject the null
  # hypothesis that the variables class and solicitation of bribe are 
  # statistically independent as our p-value is greater than the alpha of 0.1.

# (c) Calculate the standardized residuals for each cell and put them in 
# the table below.
chi_in_r <- chisq.test(cont_table)
chi_in_r$stdres

# (d) How might the standardized residuals help you interpret the results?
  # Standardized residuals tell us how far away each observed value is from our
  # expected value. This information gives us another level of evaluation when
  # determining the significance of our results. 
  # In a large sample size, the distribution of each of these values will follow
  # a normal distribution, meaning that any value that falls outside the
  # critical value of +/-1.96 will have a p-value of less than 0.05.
  # In this circumstance, none of the values are outside of this range reinforcing our
  # statement earlier that we cannot reject the null hypothesis that the 
  # variables of class and solicitation of bribe are statistically
  # independent. These residuals also give us insight on each level of the 
  # variable, allowing us to the note variation in the independence of the sample.

# Chi Square by Hand with function
# Finding the expected frequency
# Need to find for i in x, (row total/grand total)*column total
expected_ctm <- mapply(function(x, y, z) (sum(y)/sum(x))*sum(z), ctm, ctm[,1], ctm[1,])
expected_ctm

# expected <- function(x)
  # for (i in x)

(14-13.5)/sqrt(13.5*(1-(14/21))*(1-(14/27)))

# Question 2
df <- read.csv("/users/rosalielacroix/Downloads/women.csv")
df

# (a) State and null and alternative hypotheses.
  # The null hypothesis is that the reservation policy and number of new or 
  # repaired drinking water facilities in the villages are not statistically
  # related, meaning that the slope (beta) is equal to zero.
  # The alternative hypothesis is that the reservation policy and 
  # number of new or repaired drinking water facilities in the villages are
  # statistically related, meaning that the slope (beta) is not 
  # equal to zero.

# (b) Run a bivariate regression to test this hypothesis in R 
# (include your code!)
# Model: new or repaired drinking water facilities = b0 + b1 * res policy
regression <- lm(water ~ reserved, data = df)
summary(regression)
  # Interpretation:
    # Intercept (B0) = 14.738, expected water when reserved = 0
    # Slope (B1) = 9.252, average increase in water when reserved increases by 1

# t-test for slope of regression line
summary(regression)
  # slope/SE
9.252/3.948
  # t statistic = 2.343465

# check p-value
sprintf("%.20f",4.22e-10)
  # p-value = 4.22e-10

# (c) Interpret the coefficient estimate for reservation policy.
# Find coefficient estimate
round((cor(df[,6], df[,3], method = "pearson"))^2, 5)
  # or
summary(regression)
  # r^2 = 0.01688
# The coefficient estimate of 0.1299 indicates that there is a weak, positive
# relationship between the GP reserved for women and the number of new or 
# repaired drinking-water facilities in the village. Another way of putting 
# this finding is that 12.99% of the variation in the number of new or
# repaired drinking-water facilities can be attributed to whether or not the GP
# was reserved for women. 

# The coefficient estimate for reservation policy is 9.252. This value means
# that for every 1 unit increase in the reserved variable (x), the water variable
# (y) increases by 9.252. In the specific contexts of this study, the coefficient
# estimate indicates that when the Gram Panchayat (GP) goes from a policy of
# no reservation for female council heads to a policy reserving seats for female
# council heads, the number of new or repaired drinking-water facilities in the
# village increases by an average of 9.252 facilities. 

# A test statistic of 2.344
# and p-value of 4.22e-10 were found for this coefficient estimate. This test
# statistic is fairly large, indicating that it would be unlikely to find this
# coefficient if the null hypothesis that the reservation policy did not impact
# the number of new or repaired water facilities in a village. The p-value is
# also very small, which means that the coefficient results are statistically
# significant and we can reject the null hypothesis. 
