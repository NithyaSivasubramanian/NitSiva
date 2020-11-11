getwd() # function we use to know the present working directory
setwd("C://ExcelR//Assignments//Hypothesis Testing") # Created a new working directory
library(readxl)
library(readr)
cutlets_data <- read.csv("C://ata Science//ExcelR//Assignments//Hypothesis Testing//Cutlets.csv")
#1st business moment

BuyerRatio_data <- read.csv("C://Data Science//ExcelR//Assignments//Hypothesis Testing//BuyerRatio.csv")
Faltoons_data <- read.csv("C://Data Science//ExcelR//Assignments//Hypothesis Testing//Faltoons.csv")
LabTAT_data <- read.csv("C://Data Science//ExcelR//Assignments//Hypothesis Testing//LabTAT.csv")
attach(cutlets_data)
View(cutlets_data)

# check normality for unit A
shapiro.test(Unit.A)
shapiro.test(Unit.B)

# now variance test
var.test(Unit.A,Unit.B)

# now cample T test
t.test(Unit.A, Unit.B,alternative = "two.sided",conf.level = 0.95,correct = TRUE)#two sample T.Test
# alternative = "two.sided" means we are checking for equal and unequal

# assignment 2nd question on lap turn around time. 
# check normality for lab 1
attach(LabTAT_data)
View(LabTAT_data)
shapiro.test(Laboratory.1)
shapiro.test(Laboratory.2)
shapiro.test(Laboratory.3)
shapiro.test(Laboratory.4)

var.test(Laboratory.1,Laboratory.2)
var.test(Laboratory.3,Laboratory.4)

#setwd("C://Data Science//ExcelR//Datasets") # Created a new working directory

#library(readxl)
#CRD<-read_excel("ContractRenewal_Data.xlsx")   # ContractRenewal_Data(unstacked).xlsx
#CRD_read <- read.csv("C://Data Science//ExcelR//Datasets//ContractRenewal_Data(unstacked).xlsx")
##1st business moment
#View(CRD)

##CRD<-read_excel("ContractRenewal_Data(unstacked).xlsx")   # ContractRenewal_Data(unstacked).xlsx
##View(CRD)

Stacked_Data <- stack(LabTAT_data)
View(Stacked_Data)

leveneTest(values,ind,data=Stacked_Data)

Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)

# third question on hypothesis testin

library("MASS")
BuyerRatio_data <- read.csv("C://ExcelR//Assignments//Hypothesis Testing//BuyerRatio.csv")

attach(BuyerRatio_data)
View(BuyerRatio_data)
stack(BuyerRatio_data$East,BuyerRatio_data$Wast)
Stacked_Data_MFE <- stack(BuyerRatio_data$East,BuyerRatio_data$Wast)
View(Stacked_Data_MFE)
malefemale_Data_EW = table(BuyerRatio_data$East,BuyerRatio_data$West)
malefemale_Data_NS = table(BuyerRatio_data$North,BuyerRatio_data$South)

table_Data_MaleFemale = table(BuyerRatio_data)
chisq.test(malefemale_Data_EW)
chisq.test(malefemale_Data_NS)

print(chisq.test(malefemale_Data_EW))
print(chisq.test(malefemale_Data_NS))

# p-value = 0.6315 > 0.05  => Accept null hypothesis
# => All countries have equal proportions 

# 4th question telecall
#teleCall uses 4 centers around the globe to process customers order forms. They audit certain % of the customer 
#order forms. Any error in order form renders it defective and has to be reworked before processing. The manager 
#wants to check whether the defective % varies by center. Please analyze the data at 5% significant level and help
#the manager draw appropriate inferences. 

custorder_data <- read.csv("C://Data Science//ExcelR//Assignments//Hypothesis Testing//Costomer+OrderForm.csv")
View(custorder_data)
stacked_cord<-stack(custorder_data)
View(stacked_cord)
#table_custorder = table(custorder_data)
#table_custPh = table(custorder_data$Phillippines)
#table_custindo = table(custorder_data$Indonesia)
#table_custmal = table(custorder_data$Malta)
#table_custindia = table(custorder_data$India)
#chisq.test(table_custPh)
#chisq.test(table_custindo)

#chisq.test(table_custmal)
#chisq.test(table_custindia)

##
attach(stacked_cord)
View(stacked_cord)
table(stacked_cord$ind,stacked_cord$values)
chisq.test(table(stacked_cord$ind,stacked_cord$values))

# the above is hte correct solution for question 4

# question number 4

# sample
library("MASS")
Johnytalkers<-read_excel("JohnyTalkers.xlsx")   # JohnyTalkers.xlsx
Johnytalkers<-read_excel(file.choose())
View(Johnytalkers) 
custorder_data <- read.csv("C://Data Science//ExcelR//Assignments//Hypothesis Testing//Costomer+OrderForm.csv")
View(custorder_data)
View(custorder_data) 
attach(custorder_data)
Stacked_Data_custorder <- stack(Phillippines,Indonesia)
#custorder_data$Malta,custorder_data$India)
table(Phillippines,Indonesia)
table(Indonesia)
table(Malta)
table(India)
table1 <- table(Phillippines,Indonesia)
table1
table2 <- table(Malta,India)
chisq.test(table1)
chisq.test(table2)
table1
?prop.test
prop.test(x=c(58,152),n=c(480,740),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
# two. sided -> means checking for equal proportions of Adults and children under purchased
# p-value = 6.261e-05 < 0.05 accept alternate hypothesis i.e.
# Unequal proportions 

prop.test(x=c(58,152),n=c(480,740),conf.level = 0.95,correct = FALSE,alternative = "greater")
# Ha -> Proportions of Adults > Proportions of Children
# Ho -> Proportions of Children > Proportions of Adults
# p-value = 0.999 >0.05 accept null hypothesis 
# so proportion of Children > proportion of adult
# Do not launch the ice cream shop

#question 5 Panntaloos sales manager commented that % of males versus females walking into the store differ 
#based on the day of the week. Analyse the data and determine whether there is evidence at 5% significance level 
#to support this hypothesis. 

panta_data <- read.csv("C://Data Science//ExcelR//Assignments//Hypothesis Testing//Faltoons.csv")
View(panta_data)
View(panta_data) 
attach(panta_data)

table(Weekdays)
table(Weekend)
table2 <- table(Weekdays,Weekend)
table2
?prop.test
prop.test(x=c(47,66),n=c(167,233),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
# two. sided -> means checking for equal proportions of Adults and children under purchased
# p-value = 6.261e-05 < 0.05 accept alternate hypothesis i.e.
# Unequal proportions 

prop.test(x=c(47,66),n=c(167,233),conf.level = 0.95,correct = FALSE,alternative = "greater")

#library(readxl)
#JohnyTalkers_data <- read_excel("C://ExcelR//Assignments//Hypothesis Testing//JohnyTalkers.xlsx")
#View(JohnyTalkers_data)
#attach(JohnyTalkers_data)
#table(Icecream)
#table(Person)
#table1 <- table(Icecream,Person)
#table1
#?prop.test
#prop.test(x=c(58,152),n=c(480,740),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
# two. sided -> means checking for equal proportions of Adults and children under purchased
# p-value = 6.261e-05 < 0.05 accept alternate hypothesis i.e.
# Unequal proportions 

#prop.test(x=c(58,152),n=c(480,740),conf.level = 0.95,correct = FALSE,alternative = "greater")
