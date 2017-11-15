library(Sleuth3)

##Q1
## Statistically p-value determines whether the null hypothesis could be rejected 
## in order to support the alternative hypothesis. Generally, alpha value is set to
## be 0.05 and if p-value is less than or equal to alpha than we can reject the null
## hypothesis else we need to accept it. If p-value is 0.049 then by statistical theory
## we can reject the null hyothesis and accept the alternative hypothesis and if p-value
## is 0.051 then the null hypothesis cannot be rejected.
## But in my view when p-value is 0.049, it means it holds a strong correlation to 
## the critical region. In other case if the alpha value is set to 0.01 or 0.1, then
## we can accept or reject the null hypothesis respectively for the given p-value.

##Q3- Ex:2.16
## Welch Two Sample t-test
t.test(Score~Treatment, data=case0101)
## Two Sample t-test
t.test(Score~Treatment, data=case0101, var.equal = TRUE)

## From the output we can see that p-value is greater than 0.005 and
## hence it fails to reject the null hypothesis. 
## The 95% confidence interval lies between -7 to -1.3
## As it can be seen that the mean of intrinsic group is 4.1 point more 
## than the extrinsic group, hence it can be concluded that increase in score
## is attributed to intrinsic group

##Q4- Ex:2.22
## The null and alternative hypothesis are:
##H0: M1=M2
##H1: M1!=M2
library(psych)
summary(ex0222)
describe(ex0222)
aggregate(ex0222$AFQT~ex0222$Gender, data=ex0222, FUN=mean)
aggregate(ex0222$AFQT~ex0222$Gender, data=ex0222, FUN=sd)
male <- ex0222[ which(ex0222$Gender=="male") , ] 
female <- ex0222[ which(ex0222$Gender=="female") , ] 
t.test(male$AFQT, female$AFQT)
t.test(male$AFQT, female$AFQT, var.equal = TRUE)

## As we can see that the p-value is greater than 0.06, hence we cannot reject the
## null hypothesis. Thus, we can state that the provided data is insufficient to 
## prove that male distribution difference female AFQT scores.
## The 95% confidence interval is between -0.100 to 4.181

mean(male$AFQT-female$AFQT)
## The mean difference between the AFQT score of male and female is 2.1 which seems to be on higher side

library(ggplot2)
pl <- ggplot(data=ex0222, aes(ex0222$Gender,ex0222$AFQT)) + geom_boxplot() 
pl+xlab("Gender")+ylab("AFQT")

t.test(male$Arith, female$Arith)
t.test(male$Arith, female$Arith, var.equal = TRUE)
mean(male$Arith-female$Arith)

arithm <- ggplot(data=ex0222, aes(ex0222$Gender,ex0222$Arith)) + geom_boxplot() 
arithm+xlab("Gender")+ylab("Arithmetic")

t.test(male$Parag, female$Parag)
t.test(male$Parag, female$Parag, var.equal = TRUE)
mean(male$Parag-female$Parag)

para <- ggplot(data=ex0222, aes(ex0222$Gender,ex0222$Parag)) + geom_boxplot() 
para+xlab("Gender")+ylab("Paragraph")

t.test(male$Word, female$Word)
t.test(male$Word, female$Word, var.equal = TRUE)
mean(male$Word-female$Word)

word <- ggplot(data=ex0222, aes(ex0222$Gender,ex0222$Word)) + geom_boxplot() 
word+xlab("Gender")+ylab("Word")

t.test(male$Math, female$Math)
t.test(male$Math, female$Math, var.equal = TRUE)
mean(male$Math-female$Math)

math <- ggplot(data=ex0222, aes(ex0222$Gender,ex0222$Math)) + geom_boxplot() 
math+xlab("Gender")+ylab("Mathematics")
