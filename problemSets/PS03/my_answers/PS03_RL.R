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
lapply(c("wbstats", "ggplot2", "tidyverse", "stargazer", "readr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")
View(inc.sub)

# Question 1: voteshare ~ difflog
# Regression
regression1 <- lm(voteshare ~ difflog, data = inc.sub)
regression1 

# Scatterplot
pdf("scatterplot_1.pdf")
scatter1 <-
  ggplot(inc.sub, aes(x = difflog, y = voteshare)) + 
  geom_point() +
  geom_smooth(method='lm',col="red")
scatter1
dev.off()

# Save Residuals in Separate Object
residuals1 <- regression1$residuals

# Prediction Equation
regression1
  # y = 0.5790 + 0.0417x
  # voteshare = 0.5790 + 0.0417difflog

# Table
stargazer(regression1, 
          type = "latex",
          title = "Table of Coefficients",
          column.labels = "Regression 1",
          covariate.labels = "Difflog",
          dep.var.labels = "Voteshare")

# Question 2: presvote ~ difflog
# Regression
regression2 <- lm(presvote ~ difflog, data = inc.sub)
regression2

# Scatterplot
pdf("scatterplot_2.pdf")
scatter2 <-
  ggplot(inc.sub, aes(x = difflog, y = presvote)) +
  geom_point() +
  geom_smooth(method = 'lm', col = "red")
scatter2
dev.off()

# Residuals
residuals2 <- regression2$residuals

# Prediction Equation
regression2
  # y = 0.5076 + 0.0238x
  # presvote = 0.5076 + 0.0238difflog

# Table
stargazer(regression2, 
          type = "latex",
          title = "Table of Coefficients",
          column.labels = "Regression 2",
          covariate.labels = "difflog",
          dep.var.labels = "presvote")

# Question 3: voteshare ~ presvote
# Regression
regression3 <- lm(voteshare ~ presvote, data = inc.sub)
regression3

# Scatterplot
pdf("scatterplot_3.pdf")
scatter3 <-
  ggplot(inc.sub, aes(x = presvote, y = voteshare)) +
  geom_point() +
  geom_smooth(method = 'lm', col = "red")
scatter3
dev.off()

# Prediction Equation
regression3
  # y = 0.4413 + 0.3880x
  # voteshare = 0.4413 + 0.3880presvote

# Table
stargazer(regression3, 
          type = "latex",
          title = "Table of Coefficients",
          column.labels = "Regression 3",
          covariate.labels = "presvote",
          dep.var.labels = "voteshare")

# Question 4: residuals1 ~ residuals2
# Regression
resid_regression <- lm(residuals1 ~ residuals2)
resid_regression

# Scatterplot
df1 <- as.data.frame(cbind(residuals1, residuals2))
pdf("scatterplot_4.pdf")
scatter4 <- 
  ggplot(df1, aes(x = residuals2, y = residuals1)) +
  geom_point() +
  geom_smooth(method = 'lm', col = "red")
scatter4
dev.off()

# Prediction equation
resid_regression
  # y = -1.942e-18 + 0.2569x
  # residuals1 = -1.942e-18 + 0.2569residuals2

# Table
stargazer(resid_regression, 
          type = "latex",
          title = "Table of Coefficients",
          column.labels = "Residual Regression",
          covariate.labels = "residuals2",
          dep.var.labels = "residuals1")

# Question 5: voteshare ~ difflog + presvote
# Regression
regression4 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
regression4

# Prediction Equation
regression4
  # y = 0.4486 + 0.0355(x1) + 0.2569(x2)
  # voteshare = 0.4486 + 0.0355difflog + 0.2569presvote

# Table
stargazer(regression4, 
          type = "latex",
          title = "Table of Coefficients",
          column.labels = "Regression 4",
          covariate.labels = c("difflog", "presvote"),
          dep.var.labels = "voteshare")

# What is it in this ouput that is identical to the output in Question 4? 
# Why do you think this is the case?
