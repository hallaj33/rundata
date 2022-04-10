running <- read.csv('rundata.csv')
X <- model.matrix(~ avghr+maxhr+avgcadence+maxspeed+distance, data=running) #design matrix
Y <- running$avgspeed

print(beta.hat <- solve(crossprod(X), crossprod(X,Y))) #coefficients

Yhat <- X %*% beta.hat #fitted values
res <- Y - Yhat #residuals

df.full <- nrow(X) - ncol(X) #residual degrees of freedom
SSE.full <- crossprod(res) #SSE
sigma.sq <- SSE.full/df.full #sigma squared

n <- length(Y)
p <- ncol(X)-1
SST <- (n-1)*var(running$avgspeed)
print(Rsq <- 1 - SSE.full / SST) #Coefficient of Determination R2
Cor <- sqrt(Rsq) #Cor(Y, Yhat) #Correlation Coefficient
print(R2adj <- 1 - (SSE.full/(n-p-1))/(SST/(n-1))) #Adjusted R2

X.reduced <- matrix(rep(1, times=n), nrow=n) #reduced matrix with no predictors
betahat.reduced <- solve(crossprod(X.reduced), crossprod(X.reduced, Y))
Yhat.reduced <- X.reduced %*% betahat.reduced 
res.reduced <- Y - Yhat.reduced
SSE.reduced <- crossprod(res.reduced)
df.reduced <- nrow(X.reduced) - ncol(X.reduced)
F.stat <- ((SSE.reduced - SSE.full)/(df.reduced - df.full))/(SSE.full/df.full) #F-Statistic in the test of overall regression
print(p.value <- pf(F.stat, df.reduced - df.full, df.full, lower.tail=FALSE)) #p-value

C <- solve((t(X) %*% X)) #unscaled covariance matrix
sigma.hat <- sqrt(SSE.full/(n-p-1))
var.beta <- as.numeric((sigma.hat)^2) * diag(C)
se.beta <- sqrt(var.beta) #standard error of each regression coefficient

t.stat <- (beta.hat - 0) / se.beta #t-statistics
print(p.val <- 2 * pt(t.stat, df.full, lower.tail=FALSE)) #p-values

A <- rbind(c(1, 160, 180, 170, 10, 3.1), c(1, 160, 190, 170, 10, 3.1), c(1, 160, 180, 170, 10, 5), c(1, 155, 180, 170, 13, 3.1), c(1, 160, 180, 120, 10, 3.1) ) #5 extra observations
mu <- A %*% beta.hat 
mu.cov.unscaled <- A %*% C %*% t(A)
mu.SE.unscaled <- sqrt(diag(mu.cov.unscaled))
t.crit <- qt(.975, df.full)
mu.Lconf <- mu - t.crit * sigma.hat * mu.SE.unscaled
mu.Uconf <- mu + t.crit * sigma.hat * mu.SE.unscaled
mu.CI <- cbind(mu, mu.Lconf, mu.Uconf)
colnames(mu.CI) <- c('Mean', '2.5%', '97.5%')
mu.CI #prediction based on 95% prediction interval for 5 observations

# the section below is used to check the work above
mod <- lm(avgspeed ~ avghr+maxhr+avgcadence+maxspeed+distance, running) 
mod0 <- update(mod, ~ 1) 
summary(mod)
anova(mod0, mod)
newdata <- data.frame(A[,-1])
colnames(newdata) <- c('avghr','maxhr','avgcadence','maxspeed','distance')
predict(mod, newdata, interval='confidence')
# end check

#hypothesis test for reduced model without avghr, maxhr or distance
mod.reduced <- update(mod, ~ avgcadence + distance) 
dev.red <- deviance(mod.reduced)
dev <- deviance(mod)
df <- df.residual(mod)
df.red <- df.residual(mod.reduced)
F.stat1 <- (dev.red - dev)/(df.red-df)/((dev)/df)
p.value1 <- pf(F.stat1, df.red - df, df, lower.tail=FALSE)
anova(mod.reduced, mod) #not significant - keep full model


#hypothesis test with known coefficient for distance: running an extra mile causes average speed to decrease .1 mph
dd <- lm(avgspeed ~ avghr+maxhr+avgcadence+maxspeed + offset(I(-0.1*distance)), running) 
anova(dd, mod) #not significant - keep regular coefficient


#hypothesis test where coefficients for avghr and avgcadence are equal
mod.reduced <- lm(avgspeed ~ I(avghr + avgcadence), running)
mod.full <- lm(avgspeed ~ avghr + avgcadence, running)
anova(mod.reduced, mod.full) #not significant - retain full model

