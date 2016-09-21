########################
# Reading, setup and data cleaning
########################

data <- read.csv("antipsychotic_study.csv",header=T)
dim(data)

data["time"] <- 1:19
data["level"] <- c(rep(0, 8), rep(1, 11))
data["trend"] <- c(rep(0, 8), 1:11)
ceiling(max(data$marketshare))

########################
# Ploting
########################

plot(data$time,data$marketshare,
     ylab="Market share",
     ylim=c(0,ceiling(max(data$marketshare))),
     xlab="Year",
     type="l",
     col="red",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:19, labels=data$year)

# Add in the points for the figure
points(data$time,data$marketshare,
       col="red",
       pch=20)

# Label the weather change
abline(v=8.5,lty=2)

########################
# Modeling
########################

# A preliminary OLS regression
model_ols <- lm(marketshare ~ time + level + trend, data=data)
summary(model_ols)
confint(model_ols)

# Durbin-watson test, 12 time periods
dwt(model_ols,max.lag=8,alternative="two.sided")

# Graph the residuals from the OLS regression to check for serially correlated errors
plot(data$time,
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
# No significant spike in ACF, no significant spike in PACF, model p=0, q=0

########################
# Run the final model
########################

# Fit the GLS regression model
model_p0q0 <- gls(marketshare ~ time + level + trend,
                 data=data,
                 correlation=NULL,
                 method="ML")
summary(model_p0q0)


########################
# Diagnostic tests
########################

# Likelihood-ratio tests to check AR process
model_p1q0 <- update(model_p0q0,correlation=corARMA(p=1,form=~time))
anova(model_p0q0,model_p1q0)

# Likelihood-ratio tests to check AR process
model_p0q1 <- update(model_p0q0,correlation=corARMA(q=1,form=~time))
anova(model_p0q0,model_p0q1)




pred <- fitted(model_p0q0)[14]
pred

cfac <- model_p0q0$coef[1] + model_p0q0$coef[2]*14
cfac

# Absolute change
pred - cfac
# Relative change
(pred - cfac) / cfac

# Relative change 2 years after
(fitted(model_p0q0)[16] - (model_p0q0$coef[1] + model_p0q0$coef[2]*16)) / (model_p0q0$coef[1] + model_p0q0$coef[2]*16)






