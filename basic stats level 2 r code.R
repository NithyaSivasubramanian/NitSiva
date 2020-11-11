library(readxl
)
comp_msr <- read.csv("C://Data Science//ExcelR//Assignments//Basic Statistics Level 2//company measure.csv")
#1st business moment
comp_msr$Measure.X
comp_msr$Measure.V
attach(comp_msr)
View(comp_msr)
mean(comp_msr$Measure.V)

median(comp_msr$Measure.V)


install.packages("NCmisc")
library(NCmisc)

summary(comp_msr$Measure.V)
boxplot(comp_msr$Measure.V)
hist(comp_msr$Measure.V)
#2nd Business moment
var(comp_msr$Measure.V) # Var
sd(comp_msr$Measure.V) # standard deviation
range(comp_msr$Measure.V) # range

var(q7_at$Score) # Var
sd(q7_at$Score) # standard deviation
range(Score) # range
