2+4
2*4
a<-2
b<-4
c=a+b
Dataset_A
dataset_A
d="hello!"
# %in% operator
vector <- c("a","b","c","d","e")
"a" %in% vector
"g" %in% vector
!"a" %in% vector
#  getwd() to get directory
# setwd() to set directory
#install packages
install.packages(c("readxl","psych","FSA","car","ggplot2","stargazer"))
#loading packages
library(readxl)
library(psych)
library(FSA)
library(car)
library(ggplot2)
library(stargazer)

# importing dataset 

datadf <- read_excel("M:/EC969/R/EC969/wages.xlsx",sheet="WAGE2")

# woolridge dataset

# install.packages("wooldridge")
library(wooldridge)
data('wage2')

# install.packages('AER')
library(AER)
data("Fatalities")

# install.packages("devtools")
library(devtools)
devtools::install_git("https://github.com/ccolonescu/PoEdata")

#install.packages(POEdata)
library(PoEdata)
data(food, package='PoEdata')

# View the dataset
View(datadf)
View(Fatalities)

#view particular part of the data
View(datadf$wage)
View(datadf[,1:3])
View(datadf[1:10,])
print(datadf[1:10,1:3])

# datatype
str(datadf)
str(Fatalities)
str(food)

#Summary/Descriptive statistics

summary(datadf)
psych::describe(datadf)

# plotting data

ggplot(datadf,aes(x=exper,y=wage))+geom_point(color='darkgreen')

# PoE data using plot function

plot(food$income, food$food_exp, 
     ylim=c(0, max(food$food_exp)),
     xlim=c(0, max(food$income)),
     xlab="weekly income in $100", 
     ylab="weekly food expenditure ($)", 
     type = "p")

# deleting variables

## delete column by name

dfsthur <- subset(datadf, select = -c(south,urban))
View(dfsthur)

## drop column by index number

dfsthur1 <- datadf[,-c(9,10)]
View(dfsthur1)

#install packages (more)
install.packages(c("sandwich","lmtest"))
#loading library packages
library(sandwich)
library(lmtest)


# estimating linear regression

# 1. create a new variage with log of wage

lwage<-log(datadf$wage)

# running a regression

reg<- lm(lwage ~ age + exper + tenure + educ, datadf)

# summary result from linear regression

summary(reg)

# using a fancy table style

stargazer::stargazer(reg, type='latex')

# regression with no intercept

regnointer<- lm(lwage ~ age + exper + tenure + educ -1, datadf)
summary(regnointer)

# to save residuals

reg.res <- reg$residuals

# to save fitted values
reg.fit <- reg$fitted.values

# to view stardard residual error
sigma(reg)
# to view coeficients

reg$coefficients

# using different dataset (PoE)

mod1 <- lm(food_exp~income,data = food)

b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]

# saving summary of the results

smod1 <- summary(mod1)
smod1

# plotting with coefficients

plot(food$income, food$food_exp, 
     xlab = "weekly income in $100",
     ylab= "weekly food expenditure in $",
     type="p")
abline(b1,b2)
# list of name in regression

names(mod1)

names(smod1)

# coefficients

mod1$coefficients

smod1$coefficients


# prediction with linear regression model

mod1 <- lm(food_exp~income, data=food)
newx <- data.frame(income = c(20, 25, 27))
y_hat <- predict(mod1,newx)
names(y_hat) <- c("income=$2000","$2500","$2700")
y_hat

# estimated variances and covariance of regression coefficients

varb1 <- vcov(mod1)[1, 1]; varb1
varb2 <- vcov(mod1)[2, 2]; varb2

covb1b2<- vcov(mod1)[1, 2]; covb1b2
