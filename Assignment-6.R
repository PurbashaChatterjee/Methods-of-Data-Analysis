library(Sleuth3)
library(ggplot2)
library(agricolae)
library(multcomp)

ggplot(data = case0601, aes(Handicap, Score)) + geom_boxplot(fill = "yellow") + 
  geom_jitter(width = 0.03)
Handicap_Mod <- aov(Score ~ Handicap, data = case0601)

##Tukey-test
Handicap_Tukey <- glht(Handicap_Mod, linfct = mcp(Handicap = "Tukey"))
confint(Handicap_Tukey)
HSD.test(Handicap_Mod, "Handicap", group = FALSE, alpha = 0.05, console = TRUE) 

##Bonferroni
confint(Handicap_Tukey, calpha = univariate_calpha(), level = 0.995)

##Scheffe test
scheffe.test(Handicap_Mod, "Handicap", group = FALSE, alpha = 0.05, main = NULL, console = TRUE)

##Fisher LSD 
confint(Handicap_Tukey, calpha = univariate_calpha())
LSD.test(Handicap_Mod, "Handicap", group = FALSE, alpha = 0.05, console = TRUE)

##Q2:
ggplot(data = ex0623, aes(Group, WtLoss24)) + geom_boxplot(fill = "yellow") + 
  geom_jitter(width = 0.03)

Wtloss_Mod <- aov(WtLoss24 ~ Group, data = ex0623)
summary(Wtloss_Mod)

wtloss_Tukey <- glht(Wtloss_Mod, linfct = mcp(Group = "Tukey"))
confint(wtloss_Tukey)
HSD.test(Wtloss_Mod, "Group", group = FALSE, alpha = 0.05, console = TRUE) 
summary(wtloss_Tukey)


wtloss_dunnett <- glht(Wtloss_Mod, linfct = mcp(Group = "Dunnett"))
confint(wtloss_dunnett)
summary(wtloss_dunnett)

LSD.test(Wtloss_Mod, "Group", group = FALSE, alpha = 0.05, console = TRUE)
