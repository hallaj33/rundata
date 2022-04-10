running<-read.csv('rundata.csv')
X <- model.matrix(~ avghr+maxhr+avgcadence+maxspeed+distance, data=running) #design matrix
Y <- running$avgspeed
P <- X %*% solve(crossprod(X)) %*% t(X) #Projection matrix
scatterplotMatrix(~avgspeed+avghr+maxhr+avgcadence+maxspeed+distance, running, smooth=FALSE) #Scatterplot Matrix

mod <- lm(avgspeed ~ avghr+maxhr+avgcadence+maxspeed+distance, running)
res <- rstandard(mod)
fitval <- fitted(mod)
qqnorm(res) # normal qq plot of standardized residuals (also could be done with plot(mod, which=2))
qqline(res) # reference line
shapiro.test(res) #Shapiro-Wilk normality test

library('car')
residualPlots(mod, type="rstandard", id=list(n=4)) #standardized residuals vs predictors and fitted values

plot(mod, which=3) #spread-location plot
plot(mod, which=4) #cook's distance plot
plot(mod, which=5) #residuals vs leverage

library('olsrr')
ols_plot_dffits(mod) #index plot of DFFITS
ols_plot_dfbetas(mod) #index plot of DFBETAS

crPlots(mod, id=TRUE) #component residual plots

influencePlot(mod) #Influence Plot
ols_plot_resid_lev(mod) #Outlier and Leverage Diagnostics
