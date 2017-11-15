## Q1 - Ex:5.17
## We have total df = 31 , given within groups df = 24, hence dfof between groups = 31-24 = 7
## Total sum sqaure = 70907 and within groups sum of sqaure = 35088 , thus between groups sum of sqaure = 70907 - 35088 = 35819
## Mean sqaure = (sum of sq)/df 
## between groups mean sq = 35819/7 = 5117
## within groups mean sq = 35088/24 = 1462
## F = (between groups mean sq)/(within groups mean sq) = 5117/1462 = 3.5
## Using F-statistics p-value = 0.009942

## Df_BG = Df_Tot - Df_WG = 31-24 = 7
## The number of groups are,
## Df_BG = k - 1 , 
## Hence, 7 = k - 1
## k = 7-1 = 6 groups

## Since the p-value is less than 0.05 so we can reject the null hypothesis and state
## that there exist atleast one group mean value which is different

##    Source      | df  |  Sum of sq  |  Mean sq  |  F  |  p-value
##-----------------------------------------------------------------
## Between groups | 7   |  35819      |  5117     | 3.5 | 0.009942
##-----------------------------------------------------------------
## Within groups  | 24  |  35088      |  1462     | 
##-----------------------------------------------
##  Total         | 31  |  70907

## Q2 - Ex:5.20
## Assuming all 10 means having the same mean (mu) and the same variance (sigma2).
## The sample variance of the sample means therefore estimates (sigma^2/24)
## Hence s^2 = sigma^2/24
## mu = (13.6 + 15.4 + 14.7 + 12.4 + 9.2 + 13.7 + 10.3 + 7 + 9.5 + 9.5)/10 = 11.53
## s^2 = [sum (x-mu)^2]/(N-1)
## s^2 = [(4.2849)+(14.9769)+(10.0489)+(0.7569)+(5.4289)+(4.7089)+(1.5129)+(20.5209)+(4.1209)+(4.1209)]/(10-1)
## s^2 = 70.481/9 = 7.8312
## Since s^2 = sigma^2/24 ; thus 7.8312 = sigma^2/24
## sigma^2 = 7.8312 * 24 = 187.949
## sigma = 13.71

## Q3 - Ex:5.24
library(Sleuth3)
library(ggplot2)

step1 <- aov(Income2005 ~ IQquartile, data = ex0524)
summary(step1)
step1b <- aov(log(Income2005) ~ IQquartile, data = ex0524)
summary(step1b)

ggplot(ex0524, aes(IQquartile, Income2005)) + 
  geom_boxplot(fill = "yellow") + ylab("Income2005") + xlab("IQquartile")

ggplot(ex0524, aes(IQquartile, log(Income2005))) + 
  geom_boxplot(fill = "yellow") + ylab("log(Income2005)") + xlab("IQquartile")

quartile <- ex0524
quartile$high_low = with(ex0524, ifelse(IQquartile == "4thQuartile",1,0))
quartile <- quartile[!(quartile$IQquartile == "3rdQuartile" | quartile$IQquartile == "2ndQuartile"),]

step2 <- aov(Income2005 ~ IQquartile, data = quartile)
summary(step2)
step2b <- aov(log(Income2005) ~ IQquartile, data = quartile)
summary(step2b)

(step2b <- t.test(Income2005 ~ IQquartile, data = quartile, var.equal = TRUE))

(step2c <- t.test(log(Income2005) ~ IQquartile, data = quartile, var.equal = TRUE))

## Thus the mean difference of the group is (10.78478 - 10.05009) = 0.73649

step2c$statistic^2
anova(step2)$`F value`[1]

kruskal.test(Income2005 ~ IQquartile, data = quartile) 