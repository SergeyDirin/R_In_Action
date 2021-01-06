
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
