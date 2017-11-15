library(Sleuth3)
library(ggplot2)
##Q1
wilcox.test(Score ~ Treatment, data = case0101, conf.int = TRUE, alternative = "greater")

library(exactRankTests)
wilcox.exact(Score ~ Treatment, data = case0101, conf.int = TRUE)
t.test(Score~Treatment, data=case0101)
t.test(Score~Treatment, data=case0101, var.equal = TRUE)

ext <- case0101[ which(case0101$Treatment=="Extrinsic") , ] 
intr <- case0101[ which(case0101$Treatment=="Intrinsic") , ] 


##Q2
fertdiff <- ex0428$Cross-ex0428$Self
ggplot(data=ex0428, aes(fertdiff))+geom_histogram()
ggplot(data=ex0428, aes(fertdiff))+geom_histogram(binwidth = 2)
ggplot(data=ex0428, aes(fertdiff))+geom_histogram(binwidth = 5)


##paired t-test for the hypothesis
t.test(ex0428$Cross, ex0428$Self, paired = TRUE)

##95% CI for additive tretment effect
wilcox.exact(ex0428$Cross,ex0428$Self, conf.int = TRUE, paired = TRUE)

## In the histogram, certain outliers can be observed. The point between 3-5 is moderately skewed. This is because of the presence of certain difference points which are far from the mean value.

## signed rank test to find a two-sided p-value
wilcox.test(ex0428$Cross,ex0428$Self, paired = TRUE, alternative = "two.sided")
wilcox.exact(ex0428$Cross,ex0428$Self, paired = TRUE, alternative = "two.sided")

##Q3
t.test(ex0430$Sunscreen, ex0430$PreTreatment, paired=TRUE)
ggplot(ex0430, aes(Sunscreen-PreTreatment))+geom_histogram()
ggplot(ex0430, aes(Sunscreen-PreTreatment))+geom_histogram(binwidth = 50)

ggplot(ex0430, aes(log(Sunscreen)-log(PreTreatment)))+geom_histogram()
ggplot(ex0430, aes(log(Sunscreen)-log(PreTreatment)))+geom_histogram(binwidth = 0.5)

ggplot(ex0430, aes(" ",log(Sunscreen)-log(PreTreatment)))+geom_boxplot()
wilcox.exact(ex0430$Sunscreen,ex0430$PreTreatment, conf.int = TRUE)
wilcox.exact(ex0430$Sunscreen,ex0430$PreTreatment,paired = TRUE, conf.int = TRUE)

wilcox.exact(ex0430$Sunscreen-ex0430$PreTreatment,conf.int=TRUE)

##The di???erence from 0 is obvious from the plots. Although the normal paired t-Test assumptions appear to hold, Signed-Rank test has been used to generate the con???dence intervals.
exp(wilcox.test(log(ex0430$Sunscreen)-log(ex0430$PreTreatment),conf.int=TRUE)$conf.int)
##[ 4.62 , 12.25 ]With 95% con???dence, we estimate the median person will experience an SPF from 4.6 to 12.2. Note: the paired t-Test gives a similar answer. Using this test has little practical e???ect on the analysis.
exp(t.test(log(ex0430$Sunscreen)-log(ex0430$PreTreatment))$conf)
##[ 4.76 , 11.43 ]There may be confounding depending on the details of the experiment, but the fact that pairs consist of the same individual strengthen the conclusion





