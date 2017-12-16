library(ggplot2)
library(Sleuth3)
library(gridExtra)
## Q1-Ex:8.16
ggplot(data = ex0816, aes(Time, pH)) + geom_point()
ggplot(data = ex0816, aes(log(Time), pH)) + geom_point()

slr <- lm(pH ~ log(Time), data = ex0816)
summary(slr)
confint(slr)

pHdata <- ex0816
fits <- predict(slr, se.fit = TRUE)
n <- dim(ex0816)[1]
M <- sqrt(2*qf(0.95, 2, n-2))
pHdata$lower <- fits$fit - M * fits$se.fit
pHdata$upper <- fits$fit + M * fits$se.fit
(bhat <- coefficients(slr))
bhat <- as.numeric(bhat)
(estimate <- (6.0 - bhat[1])/bhat[2])
exp(estimate)

pHdata$residuals <- residuals(slr)
ggplot(data = pHdata, aes(log(Time), residuals)) + geom_point()+ geom_abline(intercept = 0.2, slope = -0.2)
qplot(slr$fitted.values, slr$residuals)+ geom_hline(aes(yintercept=0))+xlab("Fitted values") + ylab("Residuals") 

smm <- aov(pH ~ as.factor(log(Time)), data = pHdata)
summary(smm)

anova(slr, smm)

## Q2-Ex:8.22

p1<- ggplot(data = ex0822, aes(Area, Species)) + geom_smooth(method = "lm") + geom_point()
p2<- ggplot(data = ex0822, aes(log(Area), Species)) + geom_smooth(method = "lm") + geom_point()
p3<- ggplot(data = ex0822, aes(Area, log(Species))) + geom_smooth(method = "lm") + geom_point()
p4<- ggplot(data = ex0822, aes(log(Area), log(Species))) + geom_smooth(method = "lm") + geom_point()
grid.arrange(p1,p2,p3,p4, ncol = 2)

slr1 <- lm(Species ~ log(Area), data = ex0822)
summary(slr1)
confint(slr1)
smm1 <- aov(Species ~ as.factor(log(Area)), data = ex0822)
anova(slr1, smm1)

slr2 <- lm(log(Species) ~ log(Area), data = ex0822)
summary(slr2)
confint(slr2)
smm2 <- aov(log(Species) ~ as.factor(log(Area)), data = ex0822)
anova(slr2, smm2)

qplot(slr1$fitted.values, slr1$residuals)+ geom_hline(aes(yintercept=0))+xlab("Fitted values") + ylab("Residuals")
qplot(slr2$fitted.values, slr2$residuals)+ geom_hline(aes(yintercept=0))+xlab("Fitted values") + ylab("Residuals")