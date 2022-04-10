running<-read.csv('rundata.csv')

library(ggplot2) #Looking at plots to determine any obvious relationships
ggplot(running, aes(x=avghr, y=avgspeed, col=runner)) + geom_point(size=2) 
ggplot(running, aes(x=avgcadence, y=avgspeed, col=runner)) + geom_point(size=2) 
ggplot(running, aes(x=distance, y=avgspeed, col=runner)) + geom_point(size=2) 

mod1 <- lm(avgspeed ~ (avghr+avgcadence+distance+runner+timeofday)^2, running)
drop1(mod1, test='F')
mod2 <- update(mod1, ~ . - avghr:distance)
drop1(mod2, test='F')
mod3 <- update(mod2, ~ . - distance:runner)
drop1(mod3, test='F')
mod4 <- update(mod3, ~ . - avgcadence:runner)
drop1(mod4, test='F')
mod5 <- update(mod4, ~ . - avghr:runner)
drop1(mod5, test='F')
mod6 <- update(mod5, ~ . - distance:timeofday)
drop1(mod6, test='F')
mod7 <- update(mod6, ~ . - avghr:timeofday)
drop1(mod7, test='F')
mod8 <- update(mod7, ~ . - avgcadence:timeofday)
drop1(mod8, test='F')
mod9 <- update(mod8, ~ . - runner:timeofday) 
drop1(mod9, test='F')

# I typed this next line out incase something crashes, you don't have to rerun all of the above testing to get final mod.
mod9 <- lm(avgspeed ~ avghr + avgcadence + distance + runner + timeofday + avghr:avgcadence + avgcadence:distance)

# Compare all pairwise model to reduced pairwise model.
anova(mod1, mod9) 
# Adding interaction is slightly significant 

par(mfrow=c(2,2)); plot(mod9); par(mfrow=c(1,1))
# Linearity is met from Residuals vs Fitted graph
# Normality appears violated from QQplot (heavy right tail) 
# Homoscedasticity is met from Scale-Location plot

library('olsrr')
ols_plot_dfbetas(mod9)
ols_plot_resid_lev(mod9) # there are quite a few observations that could be considered influential (high leverage, outliers or both)

summary(mod9)

# Interactions 

# avghr:avgcadence (numeric:numeric) 
# avgspeed increases by 0.0004828 mph due to the interaction between avghr and avgcadence.

  # if average heart rate was 160 beats per minute, we expect to observe an increase in avgspeed 
  # of 0.077248 mph for each additional step per minute.

# avgcadence:distance (numeric:numeric)
# avgspeed decreases by 0.0027276 mph due to the interaction between avgcadence and distance.

  # if average cadence was 170 steps per minutes, we expect to observe a decrease in avgspeed 
  # of 0.463692 mph for each additional mile run.

# Can we combine levels of timeofday?
library('car')
linearHypothesis(mod9, 'timeofdayEVENING = timeofdayNIGHT') 
linearHypothesis(mod9, 'timeofdayMORNING = timeofdayEVENING') #Significant!
linearHypothesis(mod9, 'timeofdayMORNING = timeofdayNIGHT')

time <- relevel(running$timeofday, "NIGHT")
mod10 <- lm(avgspeed ~ (avghr+avgcadence+distance+runner+time)^2 - avghr:distance - distance:runner - avgcadence:runner - avghr:runner - distance:time - avghr:time - avgcadence:time - runner:time, running)
summary(mod10)
linearHypothesis(mod10, 'timeAFTERNOON = timeEVENING')
linearHypothesis(mod10, 'timeAFTERNOON = timeMORNING') #Very very close. p = .05
linearHypothesis(mod10, 'timeEVENING = timeMORNING')

#Perhaps it would be wise to change the levels of timeofday into NIGHT vs NO NIGHT (ie: combine AFTERNOON, EVENING, and MORNING into one level)

install.packages("rockchalk")
library("rockchalk")
levels(timeofday)
time2 <- combineLevels(timeofday, c(1,2,3), newLabel ="NONIGHT")
mod11 <- lm(avgspeed ~ avghr + avgcadence + distance + runner + time2 + avghr:avgcadence + avgcadence:distance)
summary(mod11) 

anova(mod9,mod11) #improvement is SIGNIFICANT

time3 <- combineLevels(timeofday, c(2,3), newLabel ="MO-EV")
mod12 <- lm(avgspeed ~ avghr + avgcadence + distance + runner + time3 + avghr:avgcadence + avgcadence:distance)
summary(mod12) 

anova(mod9,mod12) #improvement is very SIGNIFICANT

anova(mod11,mod12) #Not Significant comparing "NO NIGHT" combo to "MO-EV" combo.

# It makes more logical sense to combine Morning, Afternoon, Evening -> No Night.
# The difference between combining just Morning and Evening vs combining to No Night isn't significant.
# Therefore, I think mod11 is preferred.
