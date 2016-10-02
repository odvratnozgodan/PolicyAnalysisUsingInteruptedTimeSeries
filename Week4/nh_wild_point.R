###################################
# Load the necessary libraries
###################################

library(nlme)
library(car)


########################
# Read in the dataset
########################

# Two options: either use "import dataset" in RStudio, or use read.csv
data <- read.csv("nh_wild_point.csv",header=T)

# Alternative method to create a new variable indicating the wild point
data$wild_point <- rep(0,31)
data$wild_point[20] <- 1


########################
# Preliminary Analysis
########################

# Fit the OLS regression model
model_ols <- lm(rxpp ~ time + level + trend + wild_point, data=data)
summary(model_ols)


########################
# Modeling
########################

# Fit the GLS regression model with p=10 as in Week 2
model_p0q0 <- gls(rxpp ~ time + level + trend,
                 data=data,
                 correlation=NULL,
                 method="ML")
summary(model_p0q0)


########################
# Plot results
#########################

# Produce the plot, first plotting the raw data points
plot(data$time,data$rxpp,
     ylim=c(0,7),
     ylab="Rx Per Person",
     xlab="Month",
     pch=20,
     col="pink",
     xaxt="n")

# Add x axis with dates
axis(1, at=1:31, labels=data$month)

# Add line indicating weather pattern change
abline(v=20.5,lty="dotted")

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

# And the counterfactual
segments(21,
         model_p0q0$coef[1]+model_p0q0$coef[2]*21,
         31,
         model_p0q0$coef[1]+model_p0q0$coef[2]*31,
         lty=2,
         lwd=2,
         col='red')

# END