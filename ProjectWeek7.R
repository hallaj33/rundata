running <- read.csv('rundata.csv')
running$season <- as.factor(running$season)
running$timeofday <- as.factor(running$timeofday)
running$course <- as.factor(running$course)
library(leaps)
library(car)

modmat <- model.matrix(~.-durationsec -elevgain -elevloss -avgcadence -avgstride, running) #remove obvious collinear predictors
modmat <- subset(modmat, select = - c(1)) #drop intercept column
modmat <- as.data.frame(modmat)

mod.full <- lm(avgspeed ~ ., modmat)
mod.reduced <- lm(avgspeed ~ 1, modmat)

vif(mod.full) #no alarms going off

#Condition Numbers
distance.pr <- prcomp( ~ . - distance, modmat, scale=TRUE)
distance.pr$sdev[1]/distance.pr$sdev 
#6.3
avghr.pr <- prcomp( ~ . - avghr, modmat, scale=TRUE)
avghr.pr$sdev[1]/avghr.pr$sdev 
#4.7
maxhr.pr <- prcomp( ~ .  - maxhr, modmat, scale=TRUE)
maxhr.pr$sdev[1]/maxhr.pr$sdev 
#6.1
maxcadence.pr <- prcomp( ~ . - maxcadence, modmat, scale=TRUE)
maxcadence.pr$sdev[1]/maxcadence.pr$sdev 
#6.2
maxspeed.pr <- prcomp( ~ . - maxspeed, modmat, scale=TRUE)
maxspeed.pr$sdev[1]/maxspeed.pr$sdev 
#6.3
elevchange.pr <- prcomp( ~ . - elevchange, modmat, scale=TRUE)
elevchange.pr$sdev[1]/elevchange.pr$sdev 
#6.3
runner.pr <- prcomp( ~ . -runnerLeah, modmat, scale=TRUE)
runner.pr$sdev[1]/runner.pr$sdev 
#6.3
season.pr <- prcomp(~ . -seasonSPRING -seasonWINTER -seasonSUMMER, modmat, scale=TRUE)
season.pr$sdev[1]/season.pr$sdev 
#6.5
timeofday.pr <- prcomp( ~ . -timeofdayEVENING -timeofdayMORNING -timeofdayNIGHT, modmat, scale=TRUE)
timeofday.pr$sdev[1]/timeofday.pr$sdev 
#6.5
course.pr <- prcomp(~. -courseFLAT -courseUPHILL, modmat, scale=TRUE)
course.pr$sdev[1]/course.pr$sdev 
#6.3

#no presence of collinearity detected as all condition numbers < 15 

step(mod.reduced, scope=formula(mod.full), direction='forward')
step(mod.full, direction='backward')
step(mod.full, direction='both')
aic.lm <- lm(formula = avgspeed ~ avghr + distance + runnerLeah + courseFLAT + maxcadence + timeofdayEVENING + maxspeed + maxhr + courseUPHILL, data = modmat)

step(mod.reduced, scope=formula(mod.full), direction='forward', k=log(nrow(modmat)))
step(mod.full, direction='backward', k=log(nrow(modmat)))
step(mod.full, direction='both', k=log(nrow(modmat)))
bic.lm <- lm(formula = avgspeed ~ runnerLeah + timeofdayEVENING + distance + avghr + maxcadence + courseFLAT, data = modmat)

subset <- regsubsets(avgspeed~., modmat)
plot(subset, scale='Cp')
#avghr, distance and runner are best predictors. course seems helpful as well which makes sense as running on a flat course is much easier than uphill.
plot(subset, scale='adjr2')

subsets(subset, statistic='cp', ylim=c(0,20))
cp.lm <- lm(avgspeed ~ runnerLeah + timeofdayEVENING + avghr + distance + maxcadence + maxhr + courseFLAT, modmat)
redcp.lm <- lm(avgspeed~ runnerLeah +timeofdayEVENING + distance + avghr +maxcadence + courseFLAT, modmat)

subsets(subset, statistic='adjr2', ylim=c(.80,.85)) 
adjr2.lm <- lm(avgspeed ~ runnerLeah + timeofdayEVENING + avghr + distance + maxcadence + maxhr + maxspeed +courseFLAT, modmat)
redadjr2.lm <- lm(avgspeed~ runnerLeah + distance + avghr +maxcadence + courseFLAT, modmat)

# investigate additions explored in week 5 project:
# log response
# quadratic avghr
# interactions???

inv.mod <- lm(log(avgspeed) ~ . + I(avghr^2), modmat)
step(inv.mod, direction='both', k=log(nrow(modmat)))
extra.lm <- lm(formula = log(avgspeed) ~ runnerLeah + timeofdayEVENING + distance + avghr + maxhr + maxcadence + maxspeed + courseFLAT + I(avghr^2), data = modmat)
  
subset2 <- regsubsets(log(avgspeed) ~ . + I(avghr^2), modmat)
plot(subset2, scale='Cp')
plot(subset2, scale='adjr2')
#Interesting how including a transformation and quadratic predictor changed the model. 
simple.lm <- lm(log(avgspeed) ~ runnerLeah + avghr + maxcadence, modmat)

#Comparison
summary(simple.lm) #Simplest
summary(aic.lm)
summary(bic.lm) #Balanced
summary(cp.lm)
summary(redcp.lm)
summary(adjr2.lm)
summary(redadjr2.lm)
summary(extra.lm) #I believe this is the "strongest" based on adjr2, probably overfit

par(mfrow=c(2,2))
plot(simple.lm) # problems with linearity and homescedasticity
plot(bic.lm) # problems with linearity and homescedasticity
plot(extra.lm) # problems with linearity and homescedasticity
par(mfrow=c(1,1))
invResPlot(simple.lm) #tukey's ladder suggests 1/x transformation may slightly improve
ncvTest(extra.lm)
influencePlot(extra.lm) #outliers present with large studentized residuals
residualPlots(extra.lm)

# want to look for more cool stuff but also want to sleep. Hope this is satisfactory.