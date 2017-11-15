## Q1
# I am a graduate student with specialization in machine learning and natural language 
# processing. Pursuing this course could help me to understand and analyze the
# data well

# Completing the sequel of this course with 521 would help me to gain MS minor
# in Statistics

# I have used R before but this class could help me to learn and use R more
# efficiently

## Q2
# Random Sampling is a sampling techniques where each sample has an equal
# probability of getting selected from population. This process is completely 
# based on chance

# Random assignment is an experimental process which allows the experimental 
# participants to be assigned to treatment or control group using random procedure
# These two procedures ensures that experiment is fair, avoids any kind of
# unfairness and lurking variables that can influence the outcome of the experiment 

##Ex-1.16
library(Sleuth3)
library(ggplot2)
boxplot(ex0116$PerCapitaGDP, main="Per Capita GDP", 
        ylab="Gross DOmestic Product Per Capita in $U.S.")

hist(ex0116$PerCapitaGDP)
## Bin width is $20,000 (9 breaks)

ggplot(data=ex0116, aes(PerCapitaGDP)) + geom_histogram()
##In this case the bin width is 5956.67

hist(ex0116$PerCapitaGDP[ex0116$PerCapitaGDP >= 0 & ex0116$PerCapitaGDP < 150000],
     xlab="Gross DOmestic Product Per Capita in $U.S.", 
     main="Per Capita GDP",breaks=seq(0,150000,by=5000))

##Another method to get the bin width of $5000
p <- ggplot(data=ex0116, aes(PerCapitaGDP)) + geom_histogram(binwidth = 5000)
p + xlab("Gross Domestic Product Per Capita in $U.S.")

## Histogram with 30 breaks
hist(ex0116$PerCapitaGDP, xlab="Gross DOmestic Product Per Capita in $U.S.", 
     main="Per Capita GDP", breaks=30)

##Ex-1.25

boxplot(ex0125$Zinc~ex0125$Group)
ggplot(data=ex0125, aes(x=Group, y=Zinc)) + geom_dotplot()

require(gridExtra)
plot1 <- ggplot(data=ex0125, aes(x=Group, y=Zinc)) + geom_boxplot()
plot2 <- ggplot(data=ex0125, aes(x=Group, y=Zinc)) + geom_dotplot()
grid.arrange(plot2, plot1, ncol=2)

# Boxplot is better because it displays the distribution of data values using 
# median, quartiles and extremes. This helps in identifying outliers and comparing
# distribution whereas dotplot are used for continuous and univariate data.
# It counts the frequency within group, making it difficult to make interpretation. 
