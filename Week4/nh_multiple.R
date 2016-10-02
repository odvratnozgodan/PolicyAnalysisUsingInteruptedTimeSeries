###################################
# Load the necessary libraries
###################################

library(nlme)
library(car)


########################
# Read in the dataset
########################

# Two options: either use "import dataset" in RStudio, or use read.csv
data <- read.csv("nh_multiple.csv",header=T)
data$level_1 <- c(rep(0, 20), rep(1, 28))
data$trend_1 <- c(rep(0, 20), 1:28)
data$level_2 <- c(rep(0, 31), rep(1, 17))
data$trend_2 <- c(rep(0, 31), 1:17)
data$wild_point <- rep(0,48)
data$wild_point[20] <- 1

########################
# Initial Plot
########################

# Plot outcome variable versus time
plot(data$time,data$rxpp,
     ylab="Perscriptions per person",
     ylim=c(0,7),
     xlab="Month",
     type="l",
     col="red",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:48, labels=data$month)

# Add in the points for the figure
points(data$time,data$rxpp,
       col="red",
       pch=20)

# Label the policy changes
abline(v=20.5,lty=2)
abline(v=31.5,lty=2)


########################
# Modeling
########################

# A preliminary OLS regression
model_ols <- lm(rxpp ~ time + level_1 + trend_1 + level_2 + trend_2 + wild_point, data=data)
summary(model_ols)


################################
# Assessing Autocorrelation
################################

# Durbin-watson test, 12 time periods
dwt(model_ols,max.lag=12,alternative="two.sided")

# Graph the residuals from the OLS regression to check for serially correlated errors
plot(residuals(model_ols),
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
# Spike in PACF at 4, exp decay in ACF, model with p=4

# Reset plot window
par(mfrow=c(1,1))

########################
# Run the final model
########################

# Fit the GLS regression model
model_p0q0 <- gls(rxpp ~ time + level_1 + trend_1 + level_2 + trend_2 + wild_point,
                data=data,
                correlation=NULL,
                method="ML")
summary(model_p0q0)

model_simple <- gls(rxpp ~ time + level_1 + trend_1 + wild_point,
                  data=data,
                  correlation=NULL,
                  method="ML")
summary(model_simple)



########################
# Plot results
#########################

# Produce the plot, first plotting the raw data points
plot(data$time,data$rxpp,
     ylab="Perscriptions per person",
     ylim=c(0,7),
     xlab="Year",
     pch=20,
     col="pink",
     xaxt="n")

# Add x axis with dates
axis(1, at=1:48, labels=data$month)

# Add line indicating the policy changes
abline(v=20.5,lty=2)
abline(v=31.5,lty=2)

# Plot the first line segment
segments(1,
         model_p0q0$coef[1]+model_p0q0$coef[2],
         20,
         model_p0q0$coef[1]+model_p0q0$coef[2]*20,
         lty=1,
         lwd=2,
         col='red')


# Plot the second line segment
lines(data$time[21:31], fitted(model_p0q0)[21:31], col="red",lwd=2)

# Plot the third line segment
lines(data$time[32:48], fitted(model_p0q0)[32:48], col="red",lwd=2)

# And the first counterfactual
segments(21, model_p0q0$coef[1]+model_p0q0$coef[2]*21,
         31, model_p0q0$coef[1]+model_p0q0$coef[2]*31,
         lty=2, lwd=2, col='red')


# And the second counterfactual
segments(32, model_p0q0$coef[1] + model_p0q0$coef[2]*32 +
             model_p0q0$coef[3] + model_p0q0$coef[4]*12,
         48, model_p0q0$coef[1] + model_p0q0$coef[2]*48 +
             model_p0q0$coef[3] + model_p0q0$coef[4]*28,
         lty=2, lwd=2, col='red')


##############################################
# Predict absolute and relative changes
##############################################

# Predicted value at 6 quarters after the change
pred <- fitted(model_p0q0)[43]

# Estimate the counterfactual at the same time point
cfac <- model_p0q0$coef[1] + model_p0q0$coef[2]*43 +
    model_p0q0$coef[3] + model_p0q0$coef[4]*23

# Absolute change at 25 years
pred - cfac
# Relative change at 25 years
(pred - cfac) / cfac


# END