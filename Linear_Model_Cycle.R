# Data used: http://archive.ics.uci.edu/ml/datasets/Communities+and+Crime
library(dplyr)
library(car)
library(readxl)
library(perturb)

#### Add in column names: ####

#### End Column name ####

# 1. Describe data (summary stats, scatter plots, histograms, box plots, distribution plots, etc)
# 2. Perform statistical analysis
#    test for significance and overall significance of the model
#    make individual inferences about each parameter in the model
#    Obtain simultaneous confidence intervals and/or prediction intervals where appropriate
# 3. Check the validity of the model
#    Test for lack of fit
#    Obtain residual plots and test to verify assumptions
#    Check for multicollinearity, outliers, and influential observations
#    Take appropriate remedial measures

#### 1. ####

# Graph out data

# if too many and want to check predictors against outcome variable...

# Check skew
# Looks like violent crimes per pop is postively skewed
# Make sure to check residual distribution


#### End Step 1. ####

#### Start Step 2. ####
# 2. Perform statistical analysis
#    test for significance and overall significance of the model
#    make individual inferences about each parameter in the model
#    Obtain simultaneous confidence intervals and/or prediction intervals where appropriate

# Add confidence intervals in here

# Check confidence interval

#### End Step 2. ####

#### Start Step 3. ####
# 3. Check the validity of the model
#    Test for lack of fit
#    Obtain residual plots and test to verify assumptions
#    Check for multicollinearity, outliers, and influential observations
#    Take appropriate remedial measures

# Lack of fit

# reduced <- crimes.lm
# full <- crimes_filter.lm

# Fail to reject that there is no lack of fit, and that the current model is adequate compared to the more complex model

# Residual plots and test to verify assumptions

# Check if multicollinearity exists

# Check model
