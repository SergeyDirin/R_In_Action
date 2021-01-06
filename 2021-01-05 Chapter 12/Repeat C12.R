
install.packages("lmPerm")
library(lmPerm)

set.seed(1234)
fit <- lmp(weight~height, data=women, perm="Prob")

summary(fit)

fit <- lmp(weight~height + I(height^2), data=women, perm="Prob")
summary(fit)


states <- as.data.frame(state.x77)
fit <- lmp(Murder~Population + Illiteracy+Income+Frost,
           data=states, perm="Prob")
summary(fit)

library(multcomp)
fit <- aovp(response~trt, data=cholesterol, perm="Prob")
anova(fit)

fit <- aovp(weight ~ gesttime + dose, data=litter, perm="Prob")
anova(fit)


#bootstrap

rsq <- function(formula, data, indices) {
  d <- data[indices,]
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}


library(boot)
set.seed(1234)
results <- boot(data=mtcars, statistic=rsq,
                R=1000, formula=mpg~wt+disp)

print(results)

plot(results)

boot.ci(results, type=c("perc", "bca"))

bs <- function(formula, data, indices) {
  d <- data[indices,]
  fit <- lm(formula, data=d)
  return(coef(fit))
}
results <- boot(data=mtcars, statistic=bs,
                R=1000, formula=mpg~wt+disp)
print(results)
plot(results, index=2)
boot.ci(results, type="bca", index=2)








