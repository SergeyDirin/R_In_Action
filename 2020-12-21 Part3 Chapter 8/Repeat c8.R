

fit <- lm(weight ~ height, data=women)

summary(fit)

head(women)
?women

women$weight
fitted(fit)

residuals(fit)
plot(women$height,women$weight,
     xlab="Height (in inches)",
     ylab="Weight (in pounds)")
abline(fit)

fit2 <- lm(weight ~ height + I(height^2), data=women)
summary(fit2)
plot(women$height,women$weight,
     xlab="Height (in inches)",
     ylab="Weight (in lbs)")
lines(women$height,fitted(fit2))


library(car)
scatterplot(weight ~ height, data=women,
            spread=FALSE, smoother.args=list(lty=2), pch=19,
            main="Women Age 30-39",
            xlab="Height (inches)",
            ylab="Weight (lbs.)")



states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
cor(states)
scatterplotMatrix(states, spread=FALSE, smoother.args=list(lty=2),
                  main="Scatter Plot Matrix")
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,
          data=states)
summary(fit)


fit <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(fit)


install.packages("effects")
library(effects)
plot(effect("hp:wt", fit,, list(wt=c(2.2,3.2,4.2))), multiline=TRUE)





states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
confint(fit)


fit <- lm(weight ~ height, data=women)
par(mfrow=c(2,2))
plot(fit)


fit2 <- lm(weight ~ height + I(height^2), data=women)
par(mfrow=c(2,2))
plot(fit2)

newfit <- lm(weight~ height + I(height^2), data=women[-c(13,15),])
par(mfrow=c(2,2))
plot(newfit)


states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
par(mfrow=c(2,2))
plot(fit)

par(mfrow=c(1,1))

library(car)
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
qqPlot(fit, labels=row.names(states), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")
states["Nevada",]
fitted(fit)["Nevada"]
residuals(fit)["Nevada"]


residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}
residplot(fit)

durbinWatsonTest(fit)


crPlots(fit)

ncvTest(fit)
spreadLevelPlot(fit)


install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)

vif(fit)

sqrt(vif(fit)) > 2 # problem?


outlierTest(fit)

hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)

cutoff <- 4/(nrow(states)-length(fit$coefficients)-2)
plot(fit, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")


avPlots(fit, ask=FALSE, id.method="identify")

influencePlot(fit, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")


summary(powerTransform(states$Murder))

boxTidwell(Murder~Population+Illiteracy,data=states)




states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
           data=states)
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
anova(fit2, fit1)

AIC(fit1,fit2)

library(MASS)
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,
          data=states)
stepAIC(fit, direction="backward")



install.packages("leaps")
library(leaps)
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
leaps <-regsubsets(Murder ~ Population + Illiteracy + Income +
                     Frost, data=states, nbest=4)
plot(leaps, scale="adjr2")
library(car)
subsets(leaps, statistic="cp",
        main="Cp Plot for All Subsets Regression")
abline(1,1,lty=2,col="red")


shrinkage <- function(fit, k=10){
  require(bootstrap)
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
  x <- fit$model[,2:ncol(fit$model)]
  y <- fit$model[,1]
  results <- crossval(x, y, theta.fit, theta.predict, ngroup=k)
  r2 <- cor(y, fit$fitted.values)^2
  r2cv <- cor(y, results$cv.fit)^2
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
  cat("Change =", r2-r2cv, "\n")
}


install.packages("bootstrap")
library(bootstrap)
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Income + Illiteracy + Frost, data=states)
shrinkage(fit)


states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
zstates <- as.data.frame(scale(states))
zfit <- lm(Murder~Population + Income + Illiteracy + Frost, data=zstates)
coef(zfit)
