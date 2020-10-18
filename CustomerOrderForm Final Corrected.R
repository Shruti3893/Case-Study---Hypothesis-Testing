#Hypothesis Testing Exercise

#TeleCall uses 4 centers around the globe to process customer order forms. They 
#audit a certain %  of the customer order forms. Any error in order form renders 
#it defective and has to be reworked before processing.  The manager wants to check 
#whether the defective %  varies by centre. Please analyze the data at 5% 
#significance level and help the manager draw appropriate inferences

#Minitab File: CustomerOrderForm.mtw


install.packages("readr")
library(readr)
setwd("C://Users//Lenovo//Desktop//ExcelR//Assignments//Hypothesis Testing")
getwd()
CustOrderForm<-read.csv("Costomer+OrderForm.csv")
View(CustOrderForm)
str(CustOrderForm)
summary(CustOrderForm)
CustOrderForm$Phillippines<-as.numeric(as.factor(CustOrderForm$Phillippines))
CustOrderForm$Indonesia<-as.numeric(as.factor(CustOrderForm$Indonesia))
CustOrderForm$Malta<-as.numeric(as.factor(CustOrderForm$Malta))
CustOrderForm$India<-as.numeric(as.factor(CustOrderForm$India))
View(CustOrderForm)
var(CustOrderForm)
sd(CustOrderForm$Phillippines)
sd(CustOrderForm$Indonesia)
sd(CustOrderForm$Malta)
sd(CustOrderForm$India)
install.packages("moments")
library(moments)
skewness(CustOrderForm)
kurtosis(CustOrderForm)
hist(CustOrderForm$Phillippines)
hist(CustOrderForm$Indonesia)
hist(CustOrderForm$Malta)
hist(CustOrderForm$India)
pairs(CustOrderForm)
barplot(CustOrderForm$Phillippines)
barplot(CustOrderForm$Indonesia)
barplot(CustOrderForm$Malta)
barplot(CustOrderForm$India)
boxplot(CustOrderForm$Phillippines,CustOrderForm$Indonesia,CustOrderForm$Malta,CustOrderForm$India)
sum(is.na(CustOrderForm))
attach(CustOrderForm)

#############Normality test###############

shapiro.test(Phillippines) 
shapiro.test(Indonesia)
shapiro.test(Malta) 
shapiro.test(India)
# p-value = 2.2e-16 < 0.05 so p low null fly => It does not follow normal distribution

#########X and Y are descrete|Hence, go for Chi Squared Test#################

Stacked_Data <- stack(CustOrderForm)
View(Stacked_Data)
attach(Stacked_Data)
library(car)
table(Stacked_Data)
chisq.test(table(Stacked_Data))
# p-value = 0.2771 > 0.05  => Accept null hypothesis
# => Both Units have equal proportions 