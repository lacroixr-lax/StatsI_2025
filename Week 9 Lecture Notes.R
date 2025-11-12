#######################
# Lecture Notes Week 9
######################

# Example: Support for Iraq War
  # We want to estimate the effects of race, gender, age and party ID on
  # support for Bush starting was in Iraq

# (1) Load data
load("/Users/rosalielacroix/Documents/GitHub/StatsI_2025/datasets/anes.Rdata")

# Subset data to relevant variables
anes <- anes[complete.cases(anes$caseid), ]
reg_DF <- anes[,c("white", "female", "age", "partyid", "bushiraq")]

# Hypothesis test for Beta1
  # Hypothesis: Hnull: Betawhite = 0 vs. Halternative: Bwhite != 0
  # What do we need?
    # Coefficient estimate, test statistic, standard error, p-value

# (2) Estimate coefficients and standard errors
lm_by_hand <- function(inputDF, covariates, outcome){
  # create matrices
  X <- as.matrix(cbind(rep(1, dim(inputDF)[1]), inputDF[, covariates]))
  Y <- inputDF[, outcome]
  # calculate betas
  betas <- solve((t(X)%*%X)) %*% (t(X)%*%Y)
  rownames(betas)[1] <- "intercept"
  n <- dim(inputDF)[1]
  k <- ncol(X)
  # calculate SEs for betas
  # estimate sigma-squared
  sigma_squared <- sum((Y - X%*%betas)^2)/(nrow(X)-ncol(X))
  # create variance-covariance matrix for betas
  var_covar_mat <- sigma_squared*solve(t(X)%*%X)
  # standard errors for coefficient estimates
  SEs <- sqrt(diag(var_covar_mat))
  # get t-stat and p-values
  TS <- (betas - 0)/SEs
  p_values <- 2*pt(abs(TS), n-k, lower.tail = F)
}

# (3) Estimate regression with our function
reg_results <- lm_by_hand(reg_DF, c("white", "female", "age", "partyid"), 
                          "bushiraq")
reg_results

# (4) As well as built-in lm in R
auto_results <- lm(bushiraq ~ ., data = reg_DF)
summary(auto_results)

# Confidence Interval for Beta1
confint(auto_results, level = 0.95)
