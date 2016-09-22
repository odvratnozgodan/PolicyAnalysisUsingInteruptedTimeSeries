###################################
# Load the necessary libraries
###################################

library(nlme)
library(car)


########################
# Read in the dataset
########################

# Two options: either use "import dataset" in RStudio, or use read.csv
data <- read.csv("antipsychotic_study_control.csv",header=T)
head(data)
data["time"] <- rep(1:length(data[data["state"]=="WV", 1]), 2)
data["level"] <- c(rep(0, 8), rep(1, 19 - 8), rep(0, 8), rep(1, 19 - 8))
data["trend"] <- c(rep(0, 8), 1:(19-8), rep(0, 8), 1:(19-8))
data["wvtime"] <- c(1:19, rep(0, 19))
data["wvlevel"] <- c(rep(0, 8), rep(1, 19-8), rep(0, 19))
data["wvtrend"] <- c(rep(0, 8), 1:(19-8), rep(0, 19))


########################
# Initial Plot
########################

# Plot the time series
plot(data$time[1:19],data$market_share[1:19],
     ylab="Market share",
     ylim=c(min(data$market_share)*0.8,max(data$market_share)*1.2),
     xlab="Year",
     type="l",
     col="red",
     xaxt="n")

# Add in control group
points(data$time[20:38],data$market_share[20:38],
       type='l',
       col="blue")

# Add x-axis year labels
axis(1, at=1:19, labels=data$yearqtr[1:19])

# Add in the points for the figure
points(data$time[1:19],data$market_share[1:19],
       col="red",
       pch=20)

points(data$time[20:38],data$market_share[20:38],
       col="blue",
       pch=20)

# Label the weather change
abline(v=8.5,lty=2)

# Add in a legend
legend(x=1, y=0.2, legend=c("WV","Control"), col=c("red","blue"),pch=20)


########################
# Modeling
########################

# A preliminary OLS regression
model_ols<-lm(market_share ~ time + state + wvtime + level + trend + wvlevel + wvtrend, data=data)
summary(model_ols)
confint(model_ols)



################################
# Assessing Autocorrelation
################################

# Durbin-watson test to 12 lags
dwt(model_ols,max.lag=8,alternative="two.sided")

# Graph the residuals from the OLS regression to check for serially correlated errors
plot(data$time[1:19],
     residuals(model_ols)[1:19],
     type='o',
     pch=16,
     xlab='Time',
     ylab='OLS Residuals',
     col="red")
abline(h=0,lty=2)

# Plot ACF and PACF
# Set plotting to two records on one page
par(mfrow=c(1,2))

# Produce Plots
acf(residuals(model_ols))
acf(residuals(model_ols),type='partial')
# Note decay in ACF, significant spike at 10 in PACF, model p=10

########################
# Run the final model
########################

# Fit the GLS regression model
model_p1 <- gls(market_share ~ time + state + wvtime + level + trend + wvlevel + wvtrend,
                 data=data,
                 correlation=corARMA(p=1,form=~time|state),
                 method="ML")
summary(model_p1)
confint(model_p1)




########################
# Diagnostic tests
########################

# Likelihood-ratio tests to check whether the parameters of the AR process for the errors are necessary and sufficient
model_p1q1 <- update(model_p1,correlation=corARMA(q=1,p=10,form=~time|nile))
anova(model_p1,model_p1q1)

model_p11 <- update(model_p1,correlation=corARMA(p=11,form=~time|nile))
anova(model_p1,model_p11)

# Put plotting back to one chart
par(mfrow=c(1,1))

# Residual plot
qqPlot(residuals(model_p1))



########################
# Plot results
#########################

par(mfrow=c(1,1))

# First plot the raw data points for the Nile
plot(data$time[1:19],data$market_share[1:19],
     ylim=c(min(data$market_share)*0.8,max(data$market_share)*1.2),
     ylab="Market share",
     xlab="Quarter",
     pch=20,
     col="lightblue",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:19, labels=data$year[1:19])
# Label the policy change
abline(v=8.5,lty=2)

# Add in the points for the control
points(data$time[20:38],data$market_share[20:38],
       col="pink",
       pch=20)

# Plot the first line segment for the intervention group
lines(data$time[1:8], fitted(model_p1)[1:8], col="blue",lwd=2)

# Add the second line segment for the intervention group
lines(data$time[9:19], fitted(model_p1)[9:19], col="blue",lwd=2)

# Add the counterfactual for the intervention group
segments(9, model_p1$coef[1] + model_p1$coef[2]*9 + model_p1$coef[3]+model_p1$coef[4]*9 + 
             model_p1$coef[5] + model_p1$coef[6],
         19, model_p1$coef[1] + model_p1$coef[2]*19 + model_p1$coef[3]+model_p1$coef[4]*19 + 
             model_p1$coef[5] + model_p1$coef[6]*6,
         lty=2,col='blue',lwd=2)

# Plot the first line segment for the control group
lines(data$time[20:27], fitted(model_p1)[20:27], col="red",lwd=2)

# Add the second line segment for the control
lines(data$time[28:38], fitted(model_p1)[28:38], col="red",lwd=2)

# Add the counterfactual for the control group
segments(1, model_p1$coef[1]+model_p1$coef[2],
         19,model_p1$coef[1]+model_p1$coef[2]*19,
         lty=2,col='red',lwd=2)

# Add in a legend
legend(x=3, y=0.3, legend=c("WV","Control"), col=c("blue","red"),pch=20)



##############################################
# Predict absolute and relative changes
##############################################

# Predicted value at 6 quarters after the change
pred <- fitted(model_p1)[14]

# Estimate the counterfactual at the same time point
cfac <- model_p1$coef[1] + model_p1$coef[2]*14 +
    model_p1$coef[3] + model_p1$coef[4]*14 +
    model_p1$coef[5] + model_p1$coef[6]*6

# Absolute change at 25 years
pred - cfac
# Relative change at 25 years
(pred - cfac) / cfac

# Predicted value at 2 years after the change
pred <- fitted(model_p1)[16]

# Estimate the counterfactual at the same time point
cfac <- model_p1$coef[1] + model_p1$coef[2]*16 +
    model_p1$coef[3] + model_p1$coef[4]*16 +
    model_p1$coef[5] + model_p1$coef[6]*8

# Absolute change at 25 years
pred - cfac
# Relative change at 25 years
(pred - cfac) / cfac










