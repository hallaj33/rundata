running <- read.csv('rundata.csv')
library('rockchalk')
library('car')
library('olsrr')
time2 <- combineLevels(running$timeofday, c(1,2,3), newLabel ="NONIGHT")
model <- lm(avgspeed ~ avghr + avgcadence + distance + runner + time2 + avghr:avgcadence + avgcadence:distance, running)
summary(model)

# Normality is violated due to high influence points. We can verify this from QQPlot or Shapiro-Wilk test.
qqPlot(model) 
shapiro.test(rstandard(model)) #p-value close to 0.

# Homoscedasticity seems good - no obvious fan shape in scale-location plot.
plot(model,which=3) 
ncvTest(model) #The p-value from Breusch-Pagan test indicates homoscedasticity with large p-value.

# Linearity seems OK.
plot(model,which=1) #Very slight decreasing trend
residualPlots(model) #p-value suggests we should investigate adding quadratic avgcadence term.

# Investigating Influential Points
influencePlot(model) 
ols_plot_resid_stud(model) 
ols_plot_resid_lev(model) 

# Investigation at correcting normality by removing points with high influence.
model1 <- update(model, subset=-c(77,80,19,146,7,149,61,94,144,18,79,68,81,91,168,75,82,38))
par(mfrow=c(2,2)); plot(model1); par(mfrow=c(1,1))
summary(model1)
# Removing 18 datapoints to correct normality doesn't seem like the greatest strategy, but it does improve model.
shapiro.test(rstandard(model1))


# Quadratic Predictor Variable
# I used the "drop1" command to arrive at this final model. I investigated removing the categorical predictor for Time of Day.
model2 <- lm(avgspeed ~ avghr + avgcadence + I(avgcadence^2) + distance + runner + I(avgcadence^2):distance, running)
summary(model2)
par(mfrow=c(2,2)); plot(model2); par(mfrow=c(1,1))
plot(model2,which=1)
# Interestingly, adding the quadratic predictor seems to violate the homoscedasticity assumption.
ncvTest(model2)
# Weighted Least Squares to fix homoscedasticity
model3 <- update(model2, weights=1/avgcadence^2)
summary(model3)
plot(weighted.residuals(model3)~fitted(model3))
ncvTest(model3)

invResPlot(model3) #optimal lambda near 0 -> investigate logarithmic response variable

# Log transformation on response variable for model with quadratic avgcadence and without time of day.
model4 <- update(model3, log(avgspeed) ~ .)
summary(model4)
par(mfrow=c(2,2)); plot(model4); par(mfrow=c(1,1)) #Slightly improved normality
ncvTest(model4)

# Looking at other transformations for predictor variables
invTranPlot(avgspeed ~ distance, running) 
invTranPlot(avgspeed ~ avgcadence, running) 
invTranPlot(avgspeed ~ avghr, running) 
# looking at the above plots and the comparing RSS, transformations may not be useful for any of the numerical predictors.

# Comparing models based solely on adjusted r-squared:
summary(model)$adj.r.squared #Original model (includes time of day)
summary(model1)$adj.r.squared #Removing Observations
summary(model2)$adj.r.squared #Quadratic Predictor (excludes time of day)
summary(model3)$adj.r.squared #Adding weighted least-squares WLS 
summary(model4)$adj.r.squared #Adding logarithmic response variable transformation

residualPlots(model4)
# Interesting how this new model suggests an improvement could be made by including a quadratic avghr term.

#I looked at the following plots for the project last week. Reviewing these made me more curious about the quadratic relationship possibility for avghr.
library(ggplot2) #Looking at plots to determine any obvious relationships
ggplot(running, aes(x=avghr, y=avgspeed, col=runner)) + geom_point(size=2) 
ggplot(running, aes(x=avgcadence, y=avgspeed, col=runner)) + geom_point(size=2) 
ggplot(running, aes(x=distance, y=avgspeed, col=runner)) + geom_point(size=2)