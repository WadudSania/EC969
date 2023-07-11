## Lab 2 worksheet
## Script by Sonkurt Sen / tested and adapted by Angus Holford

setwd("M:/EC969/R")

remove(list=ls())

sink(file = "lab2.log", split = T)   #open a log file or can use logr package
## We will need to install packages, normally install.packages("X") would install one package, if you need to install more than one
## you can combine the packages useing c() function.

install.packages(c("tidyverse", "naniar", "haven", "survey", "labelled","Rmisc","expss","ggplot2", "AER","estimatr","margins","ggeffects","ivreg"))
## If you are asked "Do you want to install from sources the packages which need compilation", type "Yes".

##Let's activate the packages 
packages <-c("tidyverse", "naniar", "haven", "survey", "labelled","Rmisc","expss","ggplot2", "AER","estimatr","margins","ggeffects","ivreg")
lapply(packages, library, character.only = T)

## Now, we will start with lab 2

## Inspecting and Manipulating BHPS Data for Wave 1: Introduction to cross-sectional linear regression (wage models)

## First, lets import the dataset into R and call it aindresp 
## We will use read_dta function from haven library to import .dta file into R

aindresp <- read_dta("//sernt2/ec969$/UKDA-5151-stata/stata/stata13/bhps_w1/aindresp.dta")

## If we would like to import a dataset but only some of its variables, then we can write 
## Keep in mind that in order to run this as two lines, you need to include %>% at the end of 
## your first line and then choose both lines before running the code.


aindresp <- read_dta("//sernt2/ec969$/UKDA-5151-stata/stata/stata13/bhps_w1/aindresp.dta") %>%  
  select(ahid, pid, asex, amastat, aage, aqfachi, alknbrd, apaygu)

## If you are getting an error saying "could not find function "%>%", then run the commands in lines 35 and 36
## install.packages("dplyr")
## library(dplyr)

## In order to view your data
View(aindresp)

## Studying some of the variables 

## If you would like to see first 20 observations
head(aindresp, n=20)
## If you would like to transpose the way you view the dataset (you can also click on the little
## arrow next to the name of the dataset on the "Data" tab)
glimpse(aindresp)
## See the name of the variables
names(aindresp)
## Summary statistics of the data: min, max, mean, quartiles
summary(aindresp)
## See the labels of the variables 
sapply(aindresp, attr, "labels")


## Let's focus one variable at a time


## If you would like to see the label for one specific variable
attr(x=aindresp$alknbrd, which="labels")
## If you would like to create a frequency table 
table(aindresp$alknbrd)
## If you would like to see sample size, mean, standard deviation, standard error and confidence interval
summarySE(measurevar = "alknbrd",data = aindresp, na.rm=TRUE )
# NB: This includes missing values

## Now let's go back to the complete dataset
aindresp <- read_dta("//sernt2/ec969$/UKDA-5151-stata/stata/stata13/bhps_w1/aindresp.dta")
View(aindresp)

## Let's recode the missing values (these are -1, -2, -7, -8, -9)
## First, start with creating a vector of missing values 
missingv <- c(-9, -8, -7, -2, -1)
## Then, write a loop where each of these missing values will be replaced with NA 
## NA is the missing value label of R

for (i in 1:length(missingv)) {
  aindresp<-aindresp %>% 
    mutate_all(., list(~na_if(., missingv[i])))
}

glimpse(aindresp)

## Q: How does the proportion of those who like/dislike their neighborhood vary by gender?
## Let's cross-tabulate the numbers in the dataset
## We can add the variables into the table by spefying deparse.level=2
table(aindresp$alknbrd, aindresp$asex, deparse.level=2)

## Let's get the percentages of the population 
aindresp %>% group_by(asex, alknbrd) %>%
  dplyr::summarise(n=n()) %>% 
  mutate(perc = (n/sum(n)) * 100,
         cumperc = cumsum(freq = n / sum(n))*100)

## 40.4% are male and like their neighborhood
## 4.08% are male and do not like their neighoorhood
## 46.2% are female and like their neighborhoold
## 5.32% are female and do not like their neihgborhood

## In order to get percetages within each group, we use "filter" (sex==1 is for males)
aindresp  %>%  filter(asex==1 ) %>%  group_by(asex, alknbrd) %>%
  dplyr::summarise(n = n()) %>%
  mutate(perc = (n/sum(n)) * 100,
         cumperc = cumsum(freq = n / sum(n)) * 100) 

## This shows that 85.9% of males like their neighborhood 
## Then let's do the same thing for females (sex==2 is for females)
aindresp  %>%  filter(asex==2 ) %>%  group_by(asex, alknbrd) %>%
  dplyr::summarise(n = n()) %>%
  mutate(perc = (n/sum(n)) * 100,
         cumperc = cumsum(freq = n / sum(n)) * 100) 
## 87.2% of the females like their neighborhood 


## Let's rename the variables 
aindresp <- dplyr::rename(aindresp,  LikesNeighbourhood = alknbrd, sex = asex) 

# Generate and label a female dummy
aindresp <- aindresp %>%
  mutate(Female = ifelse(sex ==2 , 1, 0))

## OR
aindresp$female2<-0
aindresp$female2[aindresp$sex==2]<-1

var_lab(aindresp$Female)<-"Female"
#val_lab(aindresp$Female)<- "1 Female
#                            0 Male"

val_lab(aindresp$Female) = c("Female" = 1,
                   "Male" = 0)  


table(aindresp$Female, aindresp$sex, deparse.level = 2) 
table(aindresp$female2, aindresp$sex, deparse.level = 2) 
table(aindresp$Female, aindresp$female2, deparse.level = 2) 

## Now let's take the square of age, create a married dummy as well as quarter dummies and the log of pay all together
aindresp <- aindresp %>%   
  dplyr::rename(age = aage) %>%      
  mutate(age2 = age^2,                     
         Married = ifelse(amastat == 1 | amastat == 2, 1, 0), 
         Q1 = ifelse(aqfachi <= 2, 1, 0),
         Q2 = ifelse(aqfachi == 3, 1, 0),
         Q3 = ifelse(aqfachi == 4, 1, 0),
         Q4 = ifelse(aqfachi == 5, 1, 0),
         Q5 = ifelse(aqfachi == 6, 1, 0),
         Q6 = ifelse(aqfachi == 7, 1, 0),
         LnW = log(apaygu))      
# You may get a warning message that 'NaNs are produced'. This is because trying to log transform NA values will obviously give you NAs.

## Let's label the variables 
var_lab(aindresp$Q1) <- "1st degree or higher"
var_lab(aindresp$Q2) <- "hnd, hnc, teaching"
var_lab(aindresp$Q3) <- "a level"
var_lab(aindresp$Q4) <- "o level"
var_lab(aindresp$Q5) <- "cse"
var_lab(aindresp$Q6) <- "none of these qualif"
var_lab(aindresp$Female) <- "Whether respondent is female"
var_lab(aindresp$LnW) <- "Log wage"
var_lab(aindresp$Married) <- "Whether respondent is married"
var_lab(aindresp$age2) <- "Square of age"

aindresp <- aindresp %>%   
  mutate(Q_post = ifelse(aqfachi>=1 & aqfachi<=4, 1, ifelse(aqfachi>=5 & aqfachi<=7, 0, NA )),
         Q_any = ifelse(aqfachi>=1 & aqfachi<=7, 1, 0))

var_lab(aindresp$Q_post) <- "Some post compulsory education"
var_lab(aindresp$Q_any) <- "Some post compulsory education"      

summarySE(data = aindresp, measurevar = "LnW", na.rm = T) 

## Sample Selection

## Let's restrict sample to those between ages 23 and 60 and those whose main activity status 
## is "in paid employment" and whose wage variable is not missing (over 0)
## Here, we use option "filter"



aindresp <- aindresp %>%
  filter(age >= 23 & age <= 60,
         ajbstat == 2,
         LnW >=0, 
         apaygu>=0) 




## Let's save the "filtered" dataset

write.csv(aindresp, "Lab02_TempFile.csv", row.names=FALSE)
saveRDS(aindresp, "Lab02_TempFile.rds")

## Wage regressions:
## Runing the linear regression and calling it wge.regress
wge.regress <- lm(LnW ~ age + age2 + Female + Married + Q1 + Q2 + Q3 + Q4 + Q5, data = aindresp)
## Displaying the regression results
summary(wge.regress)

## Computing robust standard errors with lm_robust function
wge.regress2 <- lm_robust(LnW ~ age + age2 + Female + Married + Q1 + Q2 + Q3 + Q4 + Q5, se_type = "stata", data = aindresp)
summary(wge.regress2)

## Computing clustered standard errors (clustered at ahid level)
wge.regress3 <- lm_robust( LnW ~ age + age2 + Female + Married + Q1 + Q2 + Q3 + Q4 + Q5,
                          se_type = "stata", cluster = ahid, data = aindresp)
summary(wge.regress3)
nobs(wge.regress3)


## Testing combinations of coefficients
linearHypothesis(wge.regress3, c("Female = 0"))
linearHypothesis(wge.regress3, c("Q1 = Q2"))
linearHypothesis(wge.regress3, c("Q1 = 0", "Q2 = 0", "Q3 = 0", "Q4 = 0", "Q5 = 0"))

## Computing confidence intervals
confint(wge.regress3, level = 0.95)
confint(wge.regress3, level = 0.90)  # adjust level for 90% confidence interval

## Using Interaction Terms

## We can either generate an interaction term mar_fem=married * female, and include it in the model:

aindresp <- aindresp %>%   
  mutate(mar_fem = Female * Married)

wge.interact <- lm_robust(LnW ~ age + age2 + Female + Married + Q1 + Q2 + Q3 + Q4 + Q5 +
                            mar_fem, se_type = "stata", cluster = ahid, data = aindresp)
summary(wge.interact)

## Or use R's own interaction term notation which is :
wge.interact2 <- lm_robust(LnW ~ age + age2 + Female + Married + Q1 + Q2 + Q3 + Q4 + Q5 +
                             Female:Married, se_type = "stata", cluster = ahid, data = aindresp)
summary(wge.interact2)

## Calculating marginal effects

## Because of the female * married interaction, the effect of being female
## will not be the same for everyone.
## And because of age and age-squared, the effect of an additional year of age
## will not be the same for everyone.
## R can take this into account if you tell it which variables are categorical
## which are continuous, and which are non-linear transformations of themselves.

## Create factor variables and continuous ones (alternative method to mutate!)
aindresp$quals <- factor(ifelse(aindresp$aqfachi == 1 | aindresp$aqfachi == 2, 1,
                                ifelse(aindresp$aqfachi == 3, 2,
                                       ifelse(aindresp$aqfachi == 4, 3,
                                              ifelse(aindresp$aqfachi == 5, 4,
                                                     ifelse(aindresp$aqfachi == 6, 5,
                                                            ifelse(aindresp$aqfachi == 7, 0, NA)))))))  # note that we code 7, none of these qualifs, as 0 so that it becomes the reference category
aindresp$sex <-  factor(aindresp$Female)
aindresp$married <-  factor(aindresp$Married)
aindresp$age <-  as.numeric(aindresp$age)  # age is a continuous variable

## Regression model with robust SEs
wge.margins <- lm_robust(LnW ~ age + I(age^2) + sex + married + quals + sex*married, 
                         se_type = "stata", cluster = ahid, data = aindresp)
summary(wge.margins)

## Compute average marginal effects
me <- margins(wge.margins)
summary(me)

## Compute at mean age
summary(margins(wge.margins, at = list(age = mean(aindresp$age, na.rm = T))))

## Compute at different specific ages
summary(margins(wge.margins, at = list(age = c(30)))) 
summary(margins(wge.margins, at = list(age = c(40)))) 
summary(margins(wge.margins, at = list(age = c(50)))) 
summary(margins(wge.margins, at = list(age = c(60)))) 


## We can also retrieve the marginal effects for specified ages and save them in a variable for later 
wge.by.age <- summary(margins(wge.margins, at = list(age = c(30, 40, 50, 60)))) 

## We can then plot the effects
as.data.frame(wge.by.age) %>%
  filter(grepl("age", factor)) %>%
  ggplot(aes(x = age, y = AME, ymin = lower, ymax = upper, group = 0)) +
  geom_pointrange() + geom_line()
ggsave(file = "Lab02_wage_by_age.png")


## wge.by.married <- summary(margins(wge.margins, at = list(married = c(0, 1), sex = c(0, 1) ))) # we can also retrieve the marginal effects for specified ages and save them in a variable for later 

## predictions of DV for different types of individuals
ggpredict(wge.margins, terms = c("sex [1]","married [1]","quals [1]"))
ggpredict(wge.margins, terms = c("sex [1]","married [0]","quals [1]"))
ggpredict(wge.margins, terms = c("sex [1]","married [1]","quals [0]"))
ggpredict(wge.margins, terms = c("sex [1]","married [0]","quals [0]"))


## Alternative to see marginal effects at different levels of some factors using some nifty change in coding
## If you are interested, this link explain how this works: https://grantmcdermott.com/2019/12/16/interaction-effects/
wge.by.sex <- lm_robust(LnW ~ age + I(age^2) + sex / married + quals, se_type = "stata", data = aindresp)
summary(wge.by.sex)

## It works the other way round too (see how AMEs for sex varies by marriage)
wge.by.married <- lm_robust(LnW ~ age + I(age^2) + married / sex + quals, se_type = "stata", data = aindresp)
summary(wge.by.married)

## plot effects
tidy(wge.by.sex, conf.int = T) %>%  # tidy up the model estimates into a table 
  filter(grepl("married", term)) %>%  # find only the married AMEs (i.e., from the interactions with sex)
  mutate(
    sex = ifelse(grepl("sex0", term), "Male", "Female")    # create a variable to identify the interaction AME for males and females
  ) %>%
  ggplot(aes(x = sex, y = estimate, ymin = conf.low, ymax = conf.high, group = 0)) +
  geom_pointrange() + geom_line() + 
  ylab("Average marginal effect of marriage")

tidy(wge.by.sex, conf.int = T) %>%
  filter(grepl("quals", term)) %>%
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, group= 0)) +
  geom_pointrange() + geom_line() + 
  ylab("Average marginal effect of qualifications")

## If we would like to know more about the estimation sample
## For example, the number of observations in a specific regression
nobs(wge.interact) 

## We can identify the sample in the regression by omitting the missing values within the regression variables
sample <- aindresp %>% 
  dplyr::select(LnW, age, age2, Female, Married, Q1, Q2, Q3, Q4, Q5) %>% 
  na.omit()
nobs(wge.regress)
summary(sample)

## We can also Inspect the data graphically

## For this, we will use ggplot command
ggplot(data = aindresp) + geom_density(aes(x = apaygu))
ggplot(data = aindresp) + geom_density(aes(x = LnW))

## Save nice plot with title and axis labels
ggplot(data = aindresp) + geom_density(aes(x = LnW)) + xlab("Log wage") + ylab("Frequency") + ggtitle("Log Gross Monthly Wage")
ggsave(file = "Lab02__logwage.png")

## Bar graph for educational qualifications
ggplot(data = aindresp) + geom_bar(aes(x = as.factor(aqfachi)))
ggplot(data = aindresp) + 
  geom_bar(aes(x = as.factor(aqfachi), fill = as.factor(Female)), 
           position = position_dodge())

ggplot(data = aindresp) + 
  geom_bar(aes(x = as.factor(aqfachi), fill = as.factor(Female)), 
           position = position_dodge()) +
  scale_fill_discrete(name = "Sex", labels = c("Male","Female")) +
  scale_x_discrete("Educational qualification", labels = c("higher degree",
                                                           "1st degree",
                                                           "hnd, hnc, teaching",
                                                           "a level",
                                                           "o level",
                                                           "cse",
                                                           "none of these qualif",
                                                           "no response")) +
  ylab("Frequency")
ggsave(file = "Lab02_edu_sex.png", width = 10)

## Histogram for age
ggplot(data = aindresp) + geom_histogram(aes(x = age))
ggplot(data = aindresp) + geom_histogram(aes(x = age), binwidth = 10)
ggsave(file = "Lab02_age.png")

## Produce estimation table for a single regression:
## Create a list that includes the list of results from a regression
regression <- summary(wge.regress)   
## Then, retrieve the coefficients in a table for
table <- as.data.frame(regression$coefficients)  
## We can also get the R2
r2 <- paste0("R^2 = ", regression$r.squared)  
## Then, we can include the r2 below the rest of the table
table <- rbind(table, c(r2,"","",""))  
## And save it as a CSV file. We include "quote = F" so that each cell does not wrap values in quotation marks
write.csv(table, file = "Lab02_coefficients.csv", quote = F)


## If we woudl like to see several regressions results in a nice looking table
## We use stargazer package

install.packages("stargazer")
library(stargazer)

## Let's present the results of regressions wge.regress, wge.regress2 and wge.regress3

r1 <- lm(LnW ~ age + age2 + Female + Married, data = aindresp)
r2 <- lm(LnW ~ age + age2 + Female + Married + Q1 + Q2 + Q3 + Q4 + Q5, data = aindresp)

stargazer(r1, r2, type="text", column.labels=c("Model 1", "Model 2"), model.numbers=FALSE)

## Stargazer package does not work with robust or clustered errors. However, if we would like to include them alongside other models, here's what we can do

install.packages("multiwayvcov")
library(multiwayvcov)

wge.regress <- lm(LnW ~ age + age2 + Female + Married + Q1 + Q2 + Q3 + Q4 + Q5, data = aindresp)

## Calculate robust standard errors
cov1         <- vcovHC(wge.regress, type = "HC1")
robust_se    <- sqrt(diag(cov1))

## Calculate clustered standard errors (clustered at ahid level)
cl.cov1 <- cluster.vcov(wge.regress, aindresp$ahid)
clustered_se <- sqrt(diag(cl.cov1))


## Then produce the table with standard errors as basic, robust and clustered, respectively
stargazer(wge.regress, wge.regress, wge.regress, se=list(NULL, robust_se, clustered_se), type = "text") 


sink() # close and save the log file


