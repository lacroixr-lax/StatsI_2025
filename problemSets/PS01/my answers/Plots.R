pdf(file="six_plots_PS01.pdf")
par(mfrow=c(2,3))
plot(expenditure$X1,
     expenditure$Y,
     xlab="X1",
     ylab="Y",
     main= "Y vs. X1")
plot(expenditure$X2,
     expenditure$Y,
     xlab="X2",
     ylab="Y",
     main="Y vs. X2")
plot(expenditure$X3,
     expenditure$Y,
     xlab="Y",
     ylab="X3",
     main="Y vs. X3")
plot(expenditure$X1,
     expenditure$X2,
     xlab="X1",
     ylab="X2",
     main="X1 vs. X2")
plot(expenditure$X1,
     expenditure$X3,
     xlab="X1",
     ylab="X3",
     main="X1 vs. X3")
plot(expenditure$X2,
     expenditure$X3,
     xlab="X2",
     ylab="X3",
     main="X2 vs. X3")
dev.off()

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

getwd()
setwd('/Users/rosalielacroix/Documents/GitHub/StatsI_2025/problemSets/PS01/my answers')
