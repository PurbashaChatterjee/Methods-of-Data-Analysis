library(Sleuth3)
##Q1
# Without log transformation
ttest<-t.test(Rainfall~Treatment, data=case0301);ttest
diffrain = -diff(mean(Rainfall ~ Treatment, data=case0301)); diffrain
mult = exp(diffrain); mult
exp(ttest$conf.int)

# With log transformation
lograin <- log(case0301$Rainfall)
ttestlograin <- t.test(lograin~Treatment, data=case0301);ttestlograin
logdiff = -diff(mean(lograin ~ Treatment, data=case0301)); logdiff
mult = exp(logdiff); mult
exp(ttestlograin$conf.int)

##Q2-Ex:3.28
t.test(Humerus~Status, data = ex0221)
t.test(Humerus~Status, data = ex0221, 
       subset=Humerus>0.659)

t.test(log(Humerus)~Status, data = ex0221, var.equal=TRUE)
t.test(log(Humerus)~Status, data = ex0221, 
       subset=Humerus>0.659, var.equal=TRUE)

##Q3-Ex:3.32
library(ggplot2)
public <- ex0332[ which(ex0332$Type=="Public") , ] 
private <- ex0332[ which(ex0332$Type=="Private") , ]
## Analyze the extent to which out of state tuition is more expensive than in state tuition for population of public schools
t.test(public$OutOfState,public$InState)
t.test(public$OutOfState,public$InState, var.equal = TRUE)

diffPublic <- ex0332$OutOfState[ex0332$Type=="Public"]-ex0332$InState[ex0332$Type=="Public"]
boxplot(diffPublic)
ggplot(data=public, aes(OutOfState)) + geom_histogram()
ggplot(data=public, aes(InState)) + geom_histogram()
sd(public$OutOfState - public$InState)

## Analyze the extent to which private school In-state tuition is more expensive than public In-state tuition
t.test(public$InState,private$InState)
t.test(public$InState,private$InState, var.equal = TRUE)
diffInState <- ex0332$InState[ex0332$Type=="Private"]-ex0332$InState[ex0332$Type=="Public"]
boxplot(diffInState)
ggplot(data=private, aes(InState)) + geom_histogram()
ggplot(data=public, aes(InState)) + geom_histogram()
ggplot(data=ex0332, aes(Type,InState)) + geom_boxplot()
sd(private$InState - public$InState)

## Analyze the extent to which private school In-state tuition is more expensive than public In-state tuition
t.test(public$OutOfState,private$OutOfState)
t.test(public$OutOfState,private$OutOfState, var.equal = TRUE)
diffOutState <- ex0332$OutOfState[ex0332$Type=="Private"]-ex0332$OutOfState[ex0332$Type=="Public"]
boxplot(diffOutState)
ggplot(data=private, aes(OutOfState)) + geom_histogram()
ggplot(data=public, aes(OutOfState)) + geom_histogram()
ggplot(data=ex0332, aes(Type,OutOfState)) + geom_boxplot()
sd(private$OutOfState - public$OutOfState)
