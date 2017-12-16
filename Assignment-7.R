library(ggplot2)
library(Sleuth3)

##Q2-Ex:7.24
mod_den <- lm(Denmark ~ Year, data = ex0724)
summary(mod_den)

mod_neth <- lm(Netherlands ~ Year, data = ex0724)
summary(mod_neth)

mod_can <- lm(Canada ~ Year, data = ex0724)
summary(mod_can)

mod_usa <- lm(USA ~ Year, data = ex0724)
summary(mod_usa)

confint(mod_den)
confint(mod_neth)
confint(mod_can)
confint(mod_usa)

mod_den2 <- lm(Denmark ~ Year-1, data = ex0724)
summary(mod_den2)

mod_neth2 <- lm(Netherlands ~ Year-1, data = ex0724)
summary(mod_neth2)

mod_usa2 <- lm(USA ~ Year-1, data = ex0724)
summary(mod_usa2)

mod_can2 <- lm(Canada ~ Year-1, data = ex0724)
summary(mod_can2)

anova(mod_den2, mod_den)
anova(mod_usa2, mod_usa)
anova(mod_neth2, mod_neth)
anova(mod_can2, mod_can)

bhat_den <- coefficients(mod_den)
ggplot(data = ex0724, aes(Year, Denmark)) + geom_point()  + 
  geom_abline(slope = bhat_den[2], intercept = bhat_den[1])

bhat_usa <- coefficients(mod_usa)
ggplot(data = ex0724, aes(Year, USA)) + geom_point()  + 
  geom_abline(slope = bhat_usa[2], intercept = bhat_usa[1])

bhat_can <- coefficients(mod_can)
ggplot(data = ex0724, aes(Year, Canada)) + geom_point()  + 
  geom_abline(slope = bhat_can[2], intercept = bhat_can[1])

bhat_neth <- coefficients(mod_neth)
ggplot(data = ex0724, aes(Year, Netherlands)) + geom_point()  + 
  geom_abline(slope = bhat_neth[2], intercept = bhat_neth[1])

##Q3-Ex:7.30
out <- lm(Refusal~Age, data=ex0730)
summary(out)
confint(out)