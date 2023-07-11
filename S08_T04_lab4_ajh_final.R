## Lab 4 worksheet
## Script by Sonkurt Sen / Tested and adapted by Angus Holford

## As in the previous lab, we will first set the working directory, clear the environment and open a log file

setwd("M:/EC969/R/EC969")

remove(list=ls())
sink(file = "lab4_v2_dplyrs_added_FTPTcorrected_FDwrong.log", split = T)   #open a log file or can use logr package

## You should have most of these packages from the previous week but in case you missed them, please download again 

install.packages(c("tidyverse","htmltools", "naniar", "haven", "survey", "labelled","Rmisc","expss","ggplot2", "AER","estimatr","margins","ggeffects","ivreg", "plm", "stargazer","htmltools"))
## If you are asked "Do you want to install from sources the packages which need compilation", type "Yes".

##Let's activate the packages 
packages <-c("tidyverse", "htmltools","naniar", "haven", "survey", "labelled","Rmisc","expss","ggplot2", "AER","estimatr","margins","ggeffects","ivreg", "plm", "stargazer","htmltools")
lapply(packages, library, character.only = T)

## Week 4: Gains from marriage and cohabitation
## Equivalising and deflating household income measures.
## Linear panel data regression models

## Similar to lab 3, let's use loops to append the first dataset so that we can have a panel dataset of 18 waves

nwaves <- 18  
variables <- list("hid", "fihhmn", "fieqfcb") # list of variables you want in the data 
## We need an empty data frame to load the data into
hhresp <- data.frame()  

for (i in 1:nwaves){    
  letter <- letters[i]  ## We make use of the letters vector inbuilt into R to reference the letter for each wave number (i.e., 1 = a, 2 = b)
  vars <- paste(letter, variables, sep = "") # We paste in the letter for each variable in our list 
  setwd(paste0("//sernt2/ec969$/UKDA-5151-stata/stata/stata13/bhps_w",i)) # the path of the datafiles
  data <- read_dta(file = paste0(letter,"hhresp.dta")) %>% # paste in the letter for the data file name as well
    dplyr::select(vars)
  colnames(data) <- variables   # rename all columns to the list you created before
  data$wave <- i # create a variable 'wave' that records which wave this dataset was from  
  hhresp <- bind_rows(hhresp, data)  # bind on the loaded data below the others
}

## Let's drop unnecessary values 
rm(letter, nwaves, variables, vars)

setwd("M:/EC969/R/EC969")


## Let's also import the individual level panel dataset
indresp <- readRDS("Lab03_FinalFile.rds")

## Let's also read cross-wave data
xwave<- read_dta(file = "//sernt2/ec969$/UKDA-5151-stata/stata/stata13/bhps_wx/xwavedat.dta") %>%
  dplyr::select(pid, memorig, sex, racel) %>%
  arrange(pid) 

## Now, let's first merge individual and household level responses using hid and wave as the merging variables
df <- merge(indresp, hhresp, by= c("hid", "wave"), all = T)

## Then, let's merge this file with xwave using pid variable
df <- merge(df, xwave, by= c("pid"), all = T) 

## Let's modify the missing data as usual
missval <- c(-9, -8, -7, -2, -1)        
for (i in 1:length(missval)) {            
  df <- df %>%             
    mutate_all(., list(~na_if(., missval[i]))) 
}

## Let's keep only variables that have non-missing values 
df <- df %>%
  filter(hid>0,memorig>0)

## Now let's work with dependent and independent variables and transform them
## Deflator: Retail Price Index (before housing cost, normalized to January 1987 prices) for Jan of each yar
## Taken from Net Income Files created by Jenkins et al
## In most cases, you would need to link these data yourself. As an optional
## exercise you could find prices indices at a monthly or regional level to link to these data. 

df <- df %>%
  dplyr::mutate(deflator = ifelse(wave ==1, 135.0, 
                           ifelse(wave ==2, 139.4,
                                  ifelse(wave ==3, 142.4,
                                         ifelse(wave ==4, 145.5,
                                                ifelse(wave ==5, 151.0,
                                                       ifelse(wave ==6, 154.0,
                                                              ifelse(wave ==7, 159.3,
                                                                     ifelse(wave ==8, 164.2 ,
                                                                            ifelse(wave ==9, 165.7,
                                                                                   ifelse(wave ==10, 171.0,
                                                                                          ifelse(wave ==11, 173.6,
                                                                                                 ifelse(wave ==12, 176.2,
                                                                                                        ifelse(wave ==13, 180.5,
                                                                                                               ifelse(wave ==14, 185.8,
                                                                                                                      ifelse(wave ==15, 190.6,
                                                                                                                             ifelse(wave ==16, 197.5,
                                                                                                                                    ifelse(wave ==17, 205.3,
                                                                                                                                           ifelse(wave ==18, 215.6, NA)))))))))))))))))))

var_lab(df$deflator) <- "Implicit GDP deflator"

df <- df %>%   
  dplyr::mutate(equivHHinc=fihhmn/fieqfcb, realequivHHinc=equivHHinc/deflator, log_incomeHH=log(realequivHHinc))


var_lab(df$equivHHinc) <- "Nominal equivalised household income"
var_lab(df$realequivHHinc) <- "Real equivalised household income"
var_lab(df$log_incomeHH) <- "Log real equivalised household income"

Rmisc::summarySE(data = df, measurevar = "equivHHinc", na.rm = T) 
Rmisc::summarySE(data = df, measurevar = "realequivHHinc", na.rm = T) 
Rmisc::summarySE(data = df, measurevar = "log_incomeHH", na.rm = T) 

## Now, let's generate explanatory variables

## Let's get rid of the duplicate from different sources first
table(df$sex.x, df$sex.y, deparse.level=2)

df <- dplyr::rename(df, 
                    sex = sex.y)

df <-df %>% 
  select(-sex.x)

df <-df %>%   
  dplyr::mutate(married = ifelse((mastat == 1 | mastat == 7), 1, 
                          ifelse(mastat>=2 & mastat<=6, 0, NA))        ,  ## Dummy for being married
         cohabiting = ifelse(mastat == 2 , 1, 
                             ifelse(mastat==1 |(mastat>=3 & mastat<=6), 0 , NA )), ## Dummy for cohabiting
         white = ifelse(racel>=1 & racel<=5, 1, 
                        ifelse(racel>=6 & racel<=18, 0, NA)), ## Dummy for White ethnicity
         age2 = age^2,  ## Age squared, etc
         age3 = age^3,
         age4 = age^4,
         Q1 = ifelse(qfachi <= 2, 1, 0), ## Qualifications dummies
         Q2 = ifelse(qfachi == 3, 1, 0),
         Q3 = ifelse(qfachi == 4, 1, 0),
         Q4 = ifelse(qfachi >= 5 & qfachi <=6 , 1, 0),
         Q5 = ifelse(qfachi == 7, 1, 0),
         PT = ifelse(jbft==2, 1, 0), ## Dummy for part-time employment
         FT = ifelse(jbft==1, 1, 0), ## Dummy for full-time employment
         anykids = ifelse(nchild>0, 1, 
                          ifelse(nchild==0, 0, 
                                 ifelse(is.na(nchild)==1, NA, NA))), ## Dummy for any kids
         parents_HH = ifelse(hgfno == 0 & hgmno==0, 0, 
                             ifelse(hgfno> 0 | hgmno>0 , 1, NA)), # Need to check this one.
         female = ifelse(sex==2, 1, 0))

df$enrolled<-0
df$enrolled[df$scnow ==1 | df$fenow==1]<-1

## Produce two-way tables for number of kids/any kids dummy and marriage status/married dummy to check correctly generating these variables
table(df$nchild, df$anykids, deparse.level = 2, useNA = "ifany")
table(df$ifvio, df$region2, deparse.level = 2, useNA = "ifany") ######################## doesn't work

table(df$FT, df$PT, deparse.level = 2, useNA = "ifany")
df <- df %>%
 dplyr::mutate(PT = ifelse(is.na(PT)==1, 0, PT) , FT = ifelse(is.na(FT)==1, 0, FT) ) 
table(df$FT, df$PT, deparse.level = 2, useNA = "ifany")

## Labels 
var_lab(df$married) <- "Married"
var_lab(df$cohabiting) <- "Cohabiting"
var_lab(df$white) <- "White"
var_lab(df$age2) <- "Age-squared"
var_lab(df$age3) <- "Age-cubed"
var_lab(df$age4) <- "Age-to power 4"
var_lab(df$Q1) <- "Degree"
var_lab(df$Q2) <- "Diploma"
var_lab(df$Q3) <- "A-Level"
var_lab(df$Q4) <- "O-Level"
var_lab(df$Q5) <- "None"
var_lab(df$PT) <- "Work PT"
var_lab(df$FT) <- "Work FT"
var_lab(df$anykids) <- "Any children?"
var_lab(df$parents_HH) <- "Parents living in HH?"
var_lab(df$female) <- "Female"
var_lab(df$enrolled) <- "Currently in school college"

## Sample selection

df <- df  %>%
  filter(df$ivfio==1 & df$region2<=9 & (df$memorig==1 | df$memorig==5) & df$enrolled==0)

## Let's get instrumental variable and call the new dataset relig 
relig<- read_dta(file = "//sernt2/ec969$/UKDA-5151-stata/stata/stata13/bhps_w1/aindresp.dta") %>%
  dplyr::select(pid, aoprlg1, aoprlg2, aoprlg3) %>%
  arrange(pid) 

df <- merge(df, relig, by= c("pid"), all = T) # 

## Generate being religious dummy in wave 1
df <-df %>%   
  dplyr::mutate(religious = ifelse(aoprlg1 >=2 & aoprlg1<=14, 1, 
                            ifelse(aoprlg1==1, 0 , NA)))

table(df$married, df$religious, deparse.level = 2, useNA = "ifany")

## Now let's look at an example of Instrumental Variables estimation



## First, check the baseline 'OLS' specification
ols <- lm_robust(log_incomeHH ~ age + I(age^2) + female + cohabiting + Q1 + Q2 + Q3 + Q4 + PT  + anykids + parents_HH + married,
                        data = df, subset = wave==1 & log_incomeHH>0)
summary(ols)
nobs(ols)

## Here while using ivreg command, we first write down the second stage and then first stage
tsls <- ivreg(log_incomeHH ~ age + I(age^2) + female + cohabiting + Q1 + Q2 + Q3 + Q4 + PT  + anykids + parents_HH + married   |   
                         age + I(age^2) + female + cohabiting + Q1 + Q2 + Q3 + Q4 + PT  + anykids + parents_HH + religious , 
                       data = df, subset = wave==1 & log_incomeHH>0)
nobs(tsls)

summary(tsls)
confint(tsls)
cov1         <- vcovHC(tsls, type = "HC1")
robust_se    <- sqrt(diag(cov1))
robust_se

## If we would like to run the two stages explicitly
## First, choose the sample to work with  (omitting all NAs so that it will be the same for both stages of the regressions)
s_married <- df %>%   
  dplyr::select(log_incomeHH, age, age2, female, cohabiting, Q1, Q2, Q3 , Q4  , PT,  anykids, parents_HH, married, religious, wave) %>%
  na.omit()

s_married <- s_married %>%
  dplyr::filter(wave==1 & log_incomeHH>0)

## First stage
stg1_marr <- lm(married ~  age + age2 + female + cohabiting + Q1 + Q2 + Q3 + Q4 + PT  + anykids + parents_HH + religious, data = s_married, subset = wave==1 & log_incomeHH>0)  # regress endogenous variable on the instrumental
summary(stg1_marr)

cov1         <- vcovHC(stg1_marr, type = "HC1")
s1_robust_se    <- sqrt(diag(cov1))
s1_robust_se

s_married <-s_married %>%   
 dplyr::mutate(marriedHat =  fitted.values(stg1_marr)) # ADDED DPLYR HERE *AFTER* GOT THE FE reg to work

Rmisc::summarySE(data = s_married, measurevar = "marriedHat", na.rm = T)  # ADDED DPLYR HERE *AFTER* GOT THE FE reg to work

## Second stage
stg2_marr <- lm(log_incomeHH ~ marriedHat + age + age2 + female + cohabiting + Q1 + Q2 + Q3 + Q4  + PT  + anykids + parents_HH, data=s_married, subset = wave==1 & log_incomeHH>0)
cov1         <- vcovHC(stg2_marr, type = "HC1")
s2_robust_se    <- sqrt(diag(cov1))
s2_robust_se

## Now, let's work with panel structure of the data

## Let's first summarize all the variables in the data
summary(df)
## Gel a value called N which shows that how many times each pid appears and produce the basic statistics about it.
N <- df %>% group_by(pid) %>% dplyr::summarise(N = n())
summary(N)

## Now, let's describe the changes over time
## Let's filter the dataset so that we can keep only those with non-missing rate_region values 
df <- df %>%
  filter(wave>1 & rate_region>0) 

df <- df %>%
  arrange(pid, wave)

df$rate_region <-  as.numeric(df$rate_region)
df$log_incomeHH <- as.numeric(df$log_incomeHH)

## Now, calculate the changes (see pay variable, we use lag function to calculate it)

library(data.table)
setDT(df)
is.data.table(df)

df <- df %>%
  arrange(pid, wave) 
df[, lag.paygu:=c(NA, paygu[-.N]), by=pid]
df[, lag.married:=c(NA, married[-.N]), by=pid]
df[, lag.cohabiting:=c(NA, cohabiting[-.N]), by=pid]
df[, lag.white:=c(NA, white[-.N]), by=pid]
df[, lag.age:=c(NA, age[-.N]), by=pid]
df[, lag.age2:=c(NA, age2[-.N]), by=pid]
df[, lag.anykids:=c(NA, anykids[-.N]), by=pid]
df[, lag.parents_HH:=c(NA, parents_HH[-.N]), by=pid]
df[, lag.PT:=c(NA, PT[-.N]), by=pid]
df[, lag.FT:=c(NA, FT[-.N]), by=pid]
df[, lag.Q1:=c(NA, Q1[-.N]), by=pid]
df[, lag.Q2:=c(NA, Q2[-.N]), by=pid]
df[, lag.Q3:=c(NA, Q3[-.N]), by=pid]
df[, lag.Q4:=c(NA, Q4[-.N]), by=pid]
df[, lag.female:=c(NA, female[-.N]), by=pid]
df[, lag.log_incomeHH:=c(NA, log_incomeHH[-.N]), by=pid]
df[, lag.rate_region:=c(NA, rate_region[-.N]), by=pid]


df <- df %>%
  arrange(pid, wave) %>%  # make sure data is sorted by waves
  group_by(pid) %>%   # group by pid so the lag function can identify changes across pid
  dplyr::mutate(paych = paygu - lag.paygu) %>% 
  dplyr::mutate(marriedch = married - lag.married) %>%  
  dplyr::mutate(cohabitingch = cohabiting - lag.cohabiting) %>%
  dplyr::mutate(whitech = white - lag.white) %>%
  dplyr::mutate(agech = age - lag.age) %>%
  dplyr::mutate(age2ch = age2 - lag.age2) %>%
  dplyr::mutate(anykidsch = anykids - lag.anykids) %>%
  dplyr::mutate(parents_HHch = parents_HH - lag.parents_HH) %>%
  dplyr::mutate(PTch = PT - lag.PT) %>%
  dplyr::mutate(FTch = FT - lag.FT) %>%
  dplyr::mutate(Q1ch = Q1 - lag.Q1) %>%
  dplyr::mutate(Q2ch = Q2 - lag.Q2) %>%
  dplyr::mutate(Q3ch = Q3 - lag.Q3) %>%
  dplyr::mutate(Q4ch = Q4 - lag.Q4) %>%
  dplyr::mutate(femalech = female - lag.female) %>%
  dplyr::mutate(log_incomeHHch = log_incomeHH - lag.log_incomeHH) %>% 
  dplyr::mutate(rate_regionch = rate_region - lag.rate_region) %>%
  ungroup() # ungroup to return to long form

## Let's check some of these variables now by tabulating them 
# Sense check on these variables: 

RmiscRRmisc::summarySE(df, measurevar = "paych", na.rm = T)

## Let's see how many people are changing their cohabiting and marital status
table(df$cohabitingch)
table(df$marriedch) 

## And now change in their ethnicity
table(df$whitech) 

## And the change in age, this is mostly 1 but can also be 0 and 2
table(df$agech)

table(df$anykidsch) 
table(df$parents_HHch)
table(df$PTch) 
table(df$FTch) 
table(df$femalech)
summarySE(df, measurevar = "rate_regionch", na.rm = T)
table(df$rate_region, df$wave)
table(df$FT)
## Now, Panel Data Analysis starting with Pooled oLS
HHinc.regress <- lm(log_incomeHH ~ married + cohabiting +age + age2 + Q1 + Q2 + Q3 + Q4+ white + anykids + parents_HH + PT + FT +  rate_region, data=df, subset= female==1 & log_incomeHH!=-Inf)
summary(HHinc.regress)

## Fixed effects

## With lm and factor. (THIS TAKES VERY LONG - run in your time and investigate differences)
#HHinc.FElm <- lm(log_incomeHH ~ married + cohabiting +age + age2 + age3 + age4 + Q1 + Q2 +   white + anykids + parents_HH + PT +  rate_region + factor(pid), data = df, subset= female==1 & log_incomeHH!=-Inf)
#summary(HHinc.FElm)







df_preserved <- df

df <-df_preserved

df <- df %>%  filter(log_incomeHH >=0,  married >=0 , cohabiting >=0 ,age >=0, age2 >=0 , Q1 >=0 ,Q2 >=0 ,  Q3 >=0 , Q4 >=0 , white >=0 , anykids >=0,parents_HH >=0 , PT  >=0 & FT>=0, rate_region>=0,  pid  >=0 , wave>=0 )
N <- df %>% group_by(pid) %>% dplyr::summarise(N = n())
View(N)
df <- merge(df, N, by = "pid", all= T)

#%>% 
 # filter(N == 2)

class(df$pid)
df$pid <- factor(df$pid)
class(df$wave)
df$wave <- factor(df$wave)




# Save dataset in case we need it later
#saveRDS(df, "Lab04_FinalFile.rds")
#write.csv(df,"Lab04_FinalFile.csv")


## Restrict to females only:
df <- df %>%
  filter(female==1) 
## Restrict to only those with a valid y variable

df$dropy<-0
df$dropy[df$log_incomeHH==NA | df$log_incomeHH==NaN | df$log_incomeHH==-Inf]<-1
df <- df %>%
  filter(dropy==0)

class(df$pid)
df$pid <- factor(df$pid)
class(df$wave)
df$wave <- factor(df$wave)


## Let's declare that df is a panel dataset with individual id being pid and time id being wave
df <- pdata.frame(df, index = c("pid", "wave"))
# Looks like have to do this *right before* starting the panel analysis, when everything else is prepared.


## Now use plm
HHinc.FEplm1 <- plm(log_incomeHH ~ married + cohabiting +age + age2 + Q1 + Q2 +  Q3 + Q4 +  white + anykids + parents_HH + PT  + FT + rate_region , data = df,  model = "within", effect = "twoways")
summary(HHinc.FEplm1)
nobs(HHinc.FEplm1)

#install.packages("remotes")
#remotes::install_github("wviechtb/esmpack")
#check.timeinvar(white, pid, data=df, out=1, na.rm=TRUE)


## Before being careful to drop all missing observations, this had retained "white" even though this should be time-invariant.

#df2 <- df %>% 
# filter(N >= 2)
#
#HHinc.FEplm2 <- plm(log_incomeHH ~ married + cohabiting +age + age2 + Q1 + Q2 +  Q3 + Q4 +  white + anykids + parents_HH + PT  + FT + rate_region , data = df2,  model = "within", effect = "twoways")
#summary(HHinc.FEplm2) 
#nobs(HHinc.FEplm2)
#df <- df %>%
  #mutate(white2 =white)
  #
#df <- df %>%
  #  arrange(pid, wave) 
#dplyr::mutate(white_lag = lag(white2, n = 1, default = NA)) 
#
#df <- df %>%
  #  arrange(pid, wave) %>%  # make sure data is sorted by waves
  #  group_by(pid) %>%   # group by pid so the lag function can identify changes across pid
  #  dplyr::mutate(whitech2 = white2 - white_lag) 
#table(df$whitech2) 

## With lm and factor - Two way Fixed Effects (Add wave effects as well - often standard) (THIS TAKES VERY LONG)
#HHinc.FElm2 <- lm(log_incomeHH ~ married + cohabiting +age + age2 + Q1 + Q2 + Q3 + Q4 +   white + anykids + parents_HH + PT  + FT +  rate_region + factor(wave) + factor(pid) , data = df, subset= female==1 & log_incomeHH!=-Inf)

## If you would like to report only some of the coefficients, then you need to write
#SHHinc.FElm2 <- summary(HHinc.FElm2)
#SHHinc.FElm2$coefficients[1:16,]

# Version with 'factor' also takes very long - run in own time and investigate differences
#HHinc.FElefe2 <- plm(log_incomeHH ~ married + cohabiting +age + age2 + Q1 + Q2 +   Q3 + Q4 +  white + anykids + parents_HH + PT + FT  + rate_region |   factor(pid) + factor(wave), data = df, )
#SHHinc.FElefe2 <- summary(HHinc.FElefe2)
#nobs(HHinc.FElefe2)
#SHHinc.FElefe2$coefficients[1:13,]



## Random effects
HHinc.REplm2 <- plm(log_incomeHH ~ married + cohabiting +age + age2 + Q1 + Q2 +  Q3 + Q4 +   white + anykids + parents_HH + PT  + FT + rate_region , data = df, model = "random", effect = "twoways")
summary(HHinc.REplm2)
nobs(HHinc.REplm2)

## First difference

## OK, having corrected the FT and PT, the First Difference no longer works, apparently because there's missing/NA cases of the changes.

df$keepchange<-0
df <- df %>%
  dplyr::mutate(keepchange = ifelse(( log_incomeHHch>-Inf & marriedch>-Inf  & cohabitingch>-Inf  &  agech>-Inf  &  age2ch>-Inf  &  Q1ch>-Inf  &  Q2ch>-Inf  &  Q3ch>-Inf  &  Q4ch>-Inf  &  whitech>-Inf  &  anykidsch>-Inf &  parents_HHch>-Inf  &  PTch>-Inf  &  FTch>-Inf  &  rate_regionch>-Inf  & log_incomeHHch<Inf & marriedch<Inf  & cohabitingch<Inf  &  agech<Inf  &  age2ch<Inf  &  Q1ch<Inf  &  Q2ch<Inf  &  Q3ch<Inf  &  Q4ch<Inf  &  whitech<Inf  &  anykidsch<Inf &  parents_HHch<Inf  &  PTch<Inf  &  FTch<Inf  &  rate_regionch<Inf)  , 1, keepchange   ))



#
 # dplyr::mutate(dropchange = ifelse((log_incomeHHch==NA | log_incomeHHch==NaN | log_incomeHHch==-Inf |marriedch==NA | marriedch==NaN | marriedch==-Inf |cohabitingch==NA | cohabitingch==NaN | cohabitingch==-Inf |agech==NA | agech==NaN | agech==-Inf |age2ch==NA | age2ch==NaN | age2ch==-Inf |Q1ch==NA | Q1ch==NaN | Q1ch==-Inf |Q2ch==NA | Q2ch==NaN | Q2ch==-Inf |Q3ch==NA | Q3ch==NaN | Q3ch==-Inf |Q4ch==NA | Q4ch==NaN | Q4ch==-Inf |whitech==NA | whitech==NaN | whitech==-Inf |anykidsch==NA | anykidsch==NaN | anykidsch==-Inf |parents_HHch==NA | parents_HHch==NaN | parents_HHch==-Inf |PTch==NA | PTch==NaN | PTch==-Inf |FTch==NA | FTch==NaN | FTch==-Inf |rate_regionch==NA | rate_regionch==NaN | rate_regionch==-Inf)  , 1, dropchange   ))
#df$dropchange[log_incomeHHch==NA | df$log_incomeHHch==NaN | df$log_incomeHHch==-Inf |df$marriedch==NA | df$marriedch==NaN | df$marriedch==-Inf |df$cohabitingch==NA | df$cohabitingch==NaN | df$cohabitingch==-Inf |df$agech==NA | df$agech==NaN | df$agech==-Inf |df$age2ch==NA | df$age2ch==NaN | df$age2ch==-Inf |df$Q1ch==NA | df$Q1ch==NaN | df$Q1ch==-Inf |df$Q2ch==NA | df$Q2ch==NaN | df$Q2ch==-Inf |df$Q3ch==NA | df$Q3ch==NaN | df$Q3ch==-Inf |df$Q4ch==NA | df$Q4ch==NaN | df$Q4ch==-Inf |df$whitech==NA | df$whitech==NaN | df$whitech==-Inf |df$anykidsch==NA | df$anykidsch==NaN | df$anykidsch==-Inf |df$parents_HHch==NA | df$parents_HHch==NaN | df$parents_HHch==-Inf |df$PTch==NA | df$PTch==NaN | df$PTch==-Inf |df$FTch==NA | df$FTch==NaN | df$FTch==-Inf |df$rate_regionch==NA | df$rate_regionch==NaN | df$rate_regionch==-Inf  ]<-1

#[log_incomeHHch==NA | df$log_incomeHHch==NaN | df$log_incomeHHch==-Inf |df$marriedch==NA | df$marriedch==NaN | df$marriedch==-Inf |df$cohabitingch==NA | df$cohabitingch==NaN | df$cohabitingch==-Inf |df$agech==NA | df$agech==NaN | df$agech==-Inf |df$age2ch==NA | df$age2ch==NaN | df$age2ch==-Inf |df$Q1ch==NA | df$Q1ch==NaN | df$Q1ch==-Inf |df$Q2ch==NA | df$Q2ch==NaN | df$Q2ch==-Inf |df$Q3ch==NA | df$Q3ch==NaN | df$Q3ch==-Inf |df$Q4ch==NA | df$Q4ch==NaN | df$Q4ch==-Inf |df$whitech==NA | df$whitech==NaN | df$whitech==-Inf |df$anykidsch==NA | df$anykidsch==NaN | df$anykidsch==-Inf |df$parents_HHch==NA | df$parents_HHch==NaN | df$parents_HHch==-Inf |df$PTch==NA | df$PTch==NaN | df$PTch==-Inf |df$FTch==NA | df$FTch==NaN | df$FTch==-Inf |df$rate_regionch==NA | df$rate_regionch==NaN | df$rate_regionch==-Inf  ]<-1
changes <- df %>%
  filter(keepchange==1)
table(changes$marriedch, useNA = "ifany")
table(changes$cohabitingch, useNA = "ifany")
table(changes$agech, useNA = "ifany")
table(changes$age2ch, useNA = "ifany")
table(changes$Q1ch, useNA = "ifany")
table(changes$Q2ch, useNA = "ifany")
table(changes$Q3ch, useNA = "ifany")
table(changes$Q4ch, useNA = "ifany")
table(changes$whitech, useNA = "ifany")
table(changes$anykidsch, useNA = "ifany")
table(changes$parents_HHch, useNA = "ifany")
table(changes$PTch, useNA = "ifany")
table(changes$FTch, useNA = "ifany")
table(changes$rate_regionch, useNA = "ifany")
summarySE(changes, measurevar = "log_incomeHH",  na.rm = T)

#subset = log_incomeHHch!=NA & log_incomeHHch!=NaN & log_incomeHHch!=-Inf &marriedch!=NA & marriedch!=NaN & marriedch!=-Inf &cohabitingch!=NA & cohabitingch!=NaN & cohabitingch!=-Inf &agech!=NA & agech!=NaN & agech!=-Inf &age2ch!=NA & age2ch!=NaN & age2ch!=-Inf &Q1ch!=NA & Q1ch!=NaN & Q1ch!=-Inf &Q2ch!=NA & Q2ch!=NaN & Q2ch!=-Inf &Q3ch!=NA & Q3ch!=NaN & Q3ch!=-Inf &Q4ch!=NA & Q4ch!=NaN & Q4ch!=-Inf &whitech!=NA & whitech!=NaN & whitech!=-Inf &anykidsch!=NA & anykidsch!=NaN & anykidsch!=-Inf &parents_HHch!=NA & parents_HHch!=NaN & parents_HHch!=-Inf &PTch!=NA & PTch!=NaN & PTch!=-Inf &FTch!=NA & FTch!=NaN & FTch!=-Inf &rate_regionch!=NA & rate_regionch!=NaN & rate_regionch!=-Inf
HHinc.FD <- lm(log_incomeHHch ~ marriedch + cohabitingch +agech + age2ch + Q1ch + Q2ch + Q3ch + Q4ch  + whitech + anykidsch + parents_HHch + PTch  + FTch +  rate_regionch, data = changes )
summary(HHinc.FD)
nobs(HHinc.FD)

## Tests of which model to use: 

# Pooled OLS?
HHinc.lm <- lm(log_incomeHH ~ married + cohabiting +age + age2 + Q1 + Q2 + Q3 + Q4 +  white + anykids + parents_HH + PT  + FT + rate_region, data = df)
summary(HHinc.lm)
nobs(HHinc.lm)
# Fixed Effect?
HHinc.FEplm2 <- plm(log_incomeHH ~ married + cohabiting +age + age2 + Q1 + Q2 + Q3 + Q4 +  white + anykids + parents_HH + PT  + FT + rate_region , data = df, model = "within", effect = "twoways")
summary(HHinc.FEplm2)
# Random Effect?
HHinc.REplm2 <- plm(log_incomeHH ~ married + cohabiting +age + age2 + Q1 + Q2 + Q3 + Q4 +  white + anykids + parents_HH + PT  + FT + rate_region , data = df, model = "random", effect = "twoways")
summary(HHinc.REplm2)

## Breusch and Pagan Lagrangian multiplier test

bptest(HHinc.REplm2)
## or
plmtest(HHinc.REplm2, type=c("bp"))


## Hausman test
# (to view coefficients side by side:)
stargazer(HHinc.FEplm2, HHinc.REplm2, type="text", column.labels=c("FE", "RE"), model.numbers=FALSE)
# Hausman test output
phtest(HHinc.FEplm2, HHinc.REplm2)

stargazer(HHinc.lm ,HHinc.FEplm2,  HHinc.FD, HHinc.REplm2, type="text", column.labels=c("Pooled", "FE", "FD",  "RE"), model.numbers=FALSE)

sink()




