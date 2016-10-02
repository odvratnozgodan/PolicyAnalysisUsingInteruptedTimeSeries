###################################
# Load the necessary libraries
###################################

library(nlme)
library(car)


########################
# Read in the dataset
########################

# Two options: either use "import dataset" in RStudio, or use read.csv
data <- read.csv("antidepressants_youth.csv",header=T)
data$trend_sq <- data$trend^2
    
########################
# Initial Plot
########################

# Plot outcome variable versus time
plot(data$time,data$ad_perc,
     ylab="Antidepresants percent",
     ylim=c(0,2),
     xlab="Quarter",
     type="l",
     col="red",
     xaxt="n")

# Add x-axis quarter labels
axis(1, at=1:44, labels=data$quarter)

# Add in the points for the figure
points(data$time,data$ad_perc,
       col="red",
       pch=20)

# Label the upstream dam build
abline(v=15.5,lty=2)


#########################
# Create New Dataset
#########################

# Make a vector of the rows we want to include
include <- c(1:15,21:44)

# Duplicate these rows into a new dataset
data_pi <- data[include,]

# Correct the trend variable in the new dataset
data_pi$trend[16:39] <- data_pi$trend[16:39] - 5
data_pi$trend_sq <- data_pi$trend^2

########################
# Modeling
########################

# A preliminary OLS regression
model_ols <- lm(ad_perc ~ time + level + trend + trend_sq, data=data_pi)
summary(model_ols)


################################
# Assessing Autocorrelation
################################

# Durbin-watson test, 12 time periods
dwt(model_ols,max.lag=12,alternative="two.sided")

# Graph the residuals from the OLS regression to check for serially correlated errors
plot(data_pi$time,
     residuals(model_ols),
     type='o',
     pch=16,
     xlab='Time',
     ylab='OLS Residuals',
     col="red")
abline(h=0,lty=2)

# Plot ACF and PACF
# Set plotting to two records on one page
par(mfrow=c(1,2))

# Produce plots
acf(residuals(model_ols))
acf(residuals(model_ols),type='partial')
# Note decay in ACF, significant spike at 4 in PACF, model p=4

# Reset the plot
par(mfrow=c(1,1))


########################
# Modeling
########################

# Fit the GLS regression model
model_p5 <- gls(ad_perc ~ time + level + trend + trend_sq,
                data=data_pi,
                correlation=corARMA(p=5,form=~time),
                method="ML")
summary(model_p5)


########################
# Plot results
#########################

# Produce the plot, first plotting the raw data points
plot(data$time,data$ad_perc,
     ylim=c(0,2),
     ylab="Antidepresants percent",
     xlab="Quarter",
     pch=20,
     col="pink",
     xaxt="n")

# Add x axis with dates
axis(1, at=1:44, labels=data$quarter)

# Add line indicating upstream dam
abline(v=15.5,lty=2)

# Plot the first line segment
lines(data$time[1:15], fitted(model_p5)[1:15], col="red",lwd=2)

# Plot the second line segment
lines(data$time[21:44], fitted(model_p5)[16:39], col="red",lwd=2)

# And the counterfactual
segments(21, model_p5$coef[1]+model_p5$coef[2]*21,
         44, model_p5$coef[1]+model_p5$coef[2]*44,
         lty=2, lwd=2, col='red')

# Add a box to show phase-in period
rect(15.5,-500,20.5,5000 , border = NA, col= '#00000011')


##############################################
# Predict absolute and relative changes
##############################################

# NOTE When calculating changes for a time  series with phase-in period select the ordinal number from 
# the smaller dataset for the predicted value, and the ordinal number from the whole dataset for the counterfactual
# Eg. ordinal 23 for the PRED value calculation because we removed 5 timeframes for the phase in period,
# and 28 for the CFAC value caluculation


# Predicted value at 6 quarters after the change
pred <- fitted(model_p5)[23]

# Estimate the counterfactual at the same time point
cfac <- model_p5$coef[1] + model_p5$coef[2]*28

# Absolute change at 25 years
pred - cfac
# Relative change at 25 years
(pred - cfac) / cfac


# END



