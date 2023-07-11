## Lab 7 worksheet
## Script by Sonkurt Sen  / Tested and adapted by Angus Holford

## As in the previous lab, we will first set the working directory, clear the environment and open a log file

setwd("M:/EC969/R/EC969")

remove(list=ls())
sink(file = "lab7_final.log", split = T)   #open a log file or can use logr package

## You should have most of these packages from the previous week but in case you missed them, please download again 
install.packages(c("tidyverse", "naniar", "haven", "survey", "labelled","Rmisc","expss","ggplot2", "AER","estimatr","margins","ggeffects", "plm", "ivreg", "fastDummies", "pglm", "multiwayvcov", "data.table", "stargazer","htmltools"))
## If you are asked "Do you want to install from sources the packages which need compilation", type "Yes".

## Let's activate the packages 
packages <-c("tidyverse", "naniar", "haven", "survey", "labelled","Rmisc","expss","ggplot2", "AER","estimatr","margins","ggeffects","ivreg", "plm", "fastDummies", "pglm", "multiwayvcov", "data.table", "stargazer","htmltools")
lapply(packages, library, character.only = T)


## Import data from Wave X

xwaveid <- read_dta("//sernt2/ec969$/UKDA-5151-stata/stata/stata13/bhps_wx/xwaveid.dta") %>%  
  select(pid, bivfio) 

xwaveid <- xwaveid %>%
  filter(bivfio==1)

## Then merge it with the week 3 dataset
indresp <- readRDS("Lab03_FinalFile.rds")

df <- merge(xwaveid, indresp, by=c("pid"), all=T)

df <- df %>%
  filter(sex==1 & (age>=16 & age<=64) & bivfio==1) 

drop <- c("bivfio")
df = df[,!(names(df) %in% drop)]

table(df$jbstat)

df$UEStatus[df$jbstat==3]<-1
df$UEStatus[df$jbstat==2]<-0


val_lab(df$UEStatus) <- make_labels("0 Employed
                                    1 Unemployed")

table(df$UEStatus)

df$LnW=log(df$paygu)
var_lab(df$LnW) <- "Ln wage"

df <-df %>%   
  dplyr::mutate(married = ifelse((mastat == 1 | mastat==2 | mastat==7), 1,
                          ifelse((mastat>=3 & mastat<=6) | (mastat>=8 & mastat<=10), 0, NA)),
         Q1 = ifelse((qfachi==1 | qfachi==2), 1, 0),
         Q2 = ifelse(qfachi==3, 1, 0),
         Q3 = ifelse(qfachi==4, 1, 0), 
         Q4 = ifelse(qfachi==5, 1, 0), 
         Q5 = ifelse(qfachi==6, 1, 0), 
         Q6 = ifelse(qfachi==7, 1, 0))

var_lab(df$Q1) <- "1st degree or higher"
var_lab(df$Q2) <- "hnd,hnc,teaching"
var_lab(df$Q3) <- "a level"
var_lab(df$Q4) <- "o level"
var_lab(df$Q5) <- "cse"
var_lab(df$Q6) <- "none of these qualif"

## Creating a new variable with the first digit of the soc code
df$SOC <- as.numeric(substr(df$jbsoc_cc, 1, 1))

## Creating region, year and soc dummies (though we will not need them for the regressions)
df$R <- df$region2
df$Y <- df$year
df <- dummy_cols(df, select_columns = c("R", "Y", "SOC"))  # This comes from the FastDummies package

df$PartTime[df$jbft==1] <- 0
df$PartTime[df$jbft==2] <- 1

table(df$PartTime)

## Generating job tenure 
df$JobTenure <- df$year - df$jbbgy4
df$JobTenure[df$JobTenure==-1]<-0
table(df$JobTenure)

summarySE(data = df, measurevar =  "JobTenure", na.rm = T)

df <- df %>%
  filter(year!=1991) 

temp <-df  %>%
  filter(year==1992)

temp <-temp  %>%
  select(pid, UEStatus)

temp <- dplyr::rename(temp,  UEStatus1992 = UEStatus) 

df <- merge(df, temp, by=c("pid"), all=T)

df_preserved <-  df

#library(data.table)
setDT(df)
is.data.table(df)
df <- df %>%
  arrange(pid, year)
df[, LagUEStatus:=c(NA, UEStatus[-.N]), by=pid]

#df <- df %>%
#  arrange(pid, year) %>%  # make sure data is sorted by waves
#  group_by(pid) %>%   # group by pid so the lag function can identify changes across pid
#  dplyr::mutate(LagUEStatus = lag(UEStatus, 1, default = NA))
table(df$LagUEStatus, df$UEStatus, deparse.level=2)

df <- pdata.frame(df, index = c("pid", "wave"))
df <- df %>%
  filter(is.na(UEStatus1992)==0) 
saveRDS(df, "Lab07_FinalFile.rds")
#df <-  df_preserved

df <- df %>%
  arrange(pid, wave) 







df$R <- factor(df$R)
df$Y <- factor(df$Y)
#df$Q <-factor(df$Q)
#df$Unemployed <- factor(df$Unemployed)
df$married <- factor(df$married)
df$claimant_count_pc <- as.numeric(df$claimant_count_pc)

table(df$LagUEStatus, df$UEStatus, deparse.level=2)

## Is there persistence in unemployment?
prob1<- glm(UEStatus ~ LagUEStatus + age + married + Q1 + Q2 + Q3 + Q4 + Q5 + R + Y + claimant_count_pc, data=df, family = binomial(link = "probit")) 
summary(prob1)
nobs(prob1)
cl.cov1 <- cluster.vcov(prob1, df$pid)
clustered_se1 <- sqrt(diag(cl.cov1))

margins(prob1, variables="LagUEStatus")
margins_prob1<-margins(prob1, variables="LagUEStatus")

## Tell R that R, Y and SOC are dummy variables (or categorical variables/factor variables)
#df$R <- factor(df$R)
#df$Y <- factor(df$Y)
df$SOC <-factor(df$SOC)
df$LagUEStatus <- factor(df$LagUEStatus)
df$Married <- factor(df$married)


prob2<- glm(UEStatus ~ LagUEStatus + age + Married + Q1 + Q2 + Q3 + Q4 + Q5 + R + Y + claimant_count_pc, data=df, family = binomial("probit")) 
summary(prob2)
nobs(prob2)
cl.cov2 <- cluster.vcov(prob2, df$pid)
clustered_se2 <- sqrt(diag(cl.cov2))


margins(prob2, variables="LagUEStatus")
margins_prob2<-margins(prob2, variables="LagUEStatus")

## Individual heterogeneity 


prob3<-pglm(UEStatus ~ LagUEStatus + age + Married + Q1 + Q2 + Q3 + Q4 + Q5 + R + Y + claimant_count_pc, data=df, family = binomial('probit'), model = "random")
summary(prob3)
#nobs(prob3)
#margins(prob3, variables="LagUEStatus")
#margins_prob3<-margins(prob3, variables="LagUEStatus1")
# Panel GLM does not 'predict', so cannot calculate margins
# So we'll ignore the panel dimension when producing margins, but still
# show how to produce the required variables. 

# See: https://stackoverflow.com/questions/72026016/how-to-obtain-the-marginal-effect-for-a-panel-data-logistic-regression-in-r


# Make sure we have the correct age variable
df2 <- setDT(df)[, c(.(counter=mean(age)), lapply(.SD, max)), 
                 .(pid), .SDcols = grep("age", names(df))]
View(df2)
df <- merge(df, df2, by='pid', all=T)



table(df$age.x, df$age.y)
df <- dplyr::rename(df,  age = age.x) 

#NB: No R_12 below
df$Married_num <- as.numeric(df$married)

means_v <- df %>%
  group_by(pid) %>%
  dplyr::mutate(mean_claimant_count_pc = mean(claimant_count_pc),
            mean_age = mean(age),
            mean_married=mean(Married_num),
            mean_Q1=mean(Q1),
            mean_Q2=mean(Q2),
            mean_Q3=mean(Q3),
            mean_Q4=mean(Q4),
            mean_Q5=mean(Q5),
            mean_R1=mean(R_1),
            mean_R2=mean(R_2),
            mean_R3=mean(R_3),
            mean_R4=mean(R_4),
            mean_R5=mean(R_5),
            mean_R6=mean(R_6),
            mean_R7=mean(R_7),
            mean_R8=mean(R_8),
            mean_R9=mean(R_9),
            mean_R10=mean(R_10),
            mean_R11=mean(R_11),
            mean_R13=mean(R_13),
            mean_RNA=mean(R_NA),
            mean_Y1993=mean(Y_1993),
            mean_Y1994=mean(Y_1994),
            mean_Y1995=mean(Y_1995),
            mean_Y1996=mean(Y_1996),
            mean_Y1997=mean(Y_1997),
            mean_Y1998=mean(Y_1998),
            mean_Y1999=mean(Y_1999),
            mean_Y2000=mean(Y_2000),
            mean_Y2001=mean(Y_2001),
            mean_Y2002=mean(Y_2002),
            mean_Y2003=mean(Y_2003),
            mean_Y2004=mean(Y_2004),
            mean_Y2005=mean(Y_2005),
            mean_Y2006=mean(Y_2006),
            mean_Y2007=mean(Y_2007),
            mean_Y2008=mean(Y_2008))
 
means_v <- means_v %>%
  select(pid, mean_age, mean_claimant_count_pc, mean_married, mean_Q1, mean_Q2, mean_Q3 , mean_Q4, mean_Q5, mean_R1, mean_R2, mean_R3, mean_R4, mean_R5, mean_R6, mean_R7, mean_R8, mean_R9, mean_R10, mean_R11, mean_R13, mean_RNA,  mean_Y1993, mean_Y1994, mean_Y1995, mean_Y1996, mean_Y1997, mean_Y1998, mean_Y1999, mean_Y2000, mean_Y2001, mean_Y2002, mean_Y2003, mean_Y2004, mean_Y2005, mean_Y2006, mean_Y2007, mean_Y2008)

means_v <- means_v %>% group_by(pid) %>% dplyr::mutate(counter = 1)
means_v <- means_v %>% group_by(pid) %>% dplyr::mutate(nth = cumsum(counter))

means_v <- means_v %>%
  filter(nth==1)

           

df_basic<-df %>%
  select(UEStatus, LagUEStatus,  UEStatus1992, age, married, Q1, Q2, Q3, Q4, Q5, R, Y, claimant_count_pc, pid, wave, cjsten)
df_wmeans<-merge(df_basic, means_v,  by='pid', all=T )


df_wmeans <- pdata.frame(df_wmeans, index = c("pid", "wave"))

#df_wmeans <- dplyr::rename(df_wmeans,  age = age.y) 
## NB: Here are trying without means of the time periods or regions:
## . means take everything else in the data as independent variables
prob4<-pglm(UEStatus ~    LagUEStatus + age + married + Q1 + Q2 + Q3 + Q4 + Q5 + R + Y + claimant_count_pc + mean_age + mean_claimant_count_pc + mean_married + mean_Q1 + mean_Q2 + mean_Q3  + mean_Q4 + mean_Q5 , data=df_wmeans[df$year>1992,],  family = binomial('probit'), model = "random")
#prob4<-pglm(UEStatus ~    LagUEStatus + age + married + Q1 + Q2 + Q3 + Q4 + Q5 + R + Y + claimant_count_pc + mean_claimant_count_pc + mean_married + mean_Q1 + mean_Q2 + mean_Q3  + mean_Q4 + mean_Q5 + mean_R1 + mean_R2 + mean_R3 + mean_R4 + mean_R5 + mean_R6 + mean_R7 + mean_R8 + mean_R9 + mean_R10 + mean_R11  +   mean_Y1994 + mean_Y1995 + mean_Y1996 + mean_Y1997 + mean_Y1998 + mean_Y1999 + mean_Y2000 + mean_Y2001 + mean_Y2002 + mean_Y2003 + mean_Y2004 + mean_Y2005 + mean_Y2006 + mean_Y2007 + mean_Y2008, data=df_wmeans[df$year>1992,],  family = binomial('probit'), model = "random")
summary(prob4)
#nobs(prob4)
#margins(prob4, variables="LagUEStatus")
#margins_prob4<-margins(prob4, variables="LagUEStatus")





# To practice extracting marginal effects, we'll forget the formal panel element
# but see how results change when controlling for means of time-varying characteristics.
prob4_2<- glm(UEStatus ~ LagUEStatus + age + married + Q1 + Q2 + Q3 + Q4 + Q5 + R + Y + claimant_count_pc + mean_age + mean_claimant_count_pc + mean_married + mean_Q1 + mean_Q2 + mean_Q3  + mean_Q4 + mean_Q5 + mean_R1 + mean_R2 + mean_R3 + mean_R4 + mean_R5 + mean_R6 + mean_R7 + mean_R8 + mean_R9 + mean_R10+ mean_R11  +   mean_Y1994 + mean_Y1995 + mean_Y1996 + mean_Y1997 + mean_Y1998 + mean_Y1999 + mean_Y2000 + mean_Y2001 + mean_Y2002 + mean_Y2003 + mean_Y2004 + mean_Y2005 + mean_Y2006 + mean_Y2007 + mean_Y2008, data=df_wmeans[df$year>1992,],  family = binomial('probit')) 
summary(prob4_2)
#cl.cov42 <- cluster.vcov(prob4_2, df_wmeans$pid)
#clustered_se4_2 <- sqrt(diag(cl.cov42))
margins(prob4_2, variables="LagUEStatus")
margins_prob4_2<-margins(prob4_2, variables="LagUEStatus")



df_wmeans <- df_wmeans %>%  filter(wave!=1) 
# Get rid of first observation, because don't have unemployment rate...


# Results will be slightly different for these: 
prob5<-pglm(UEStatus ~ LagUEStatus + age + married + Q1 + Q2 + Q3 + Q4 + Q5 + R + Y + claimant_count_pc + mean_claimant_count_pc + mean_married + mean_Q1 + mean_Q2 + mean_Q3  + mean_Q4 + mean_Q5 , data= df_wmeans,  family = binomial('probit'), model = "random")
#prob5<-pglm(UEStatus ~ LagUEStatus + age + married + Q1 + Q2 + Q3 + Q4 + Q5 + R + Y + claimant_count_pc + mean_claimant_count_pc + mean_married + mean_Q1 + mean_Q2 + mean_Q3  + mean_Q4 + mean_Q5 + mean_R1 + mean_R2 + mean_R3 + mean_R4 + mean_R5 + mean_R6 + mean_R7 + mean_R8 + mean_R9 + mean_R10 + mean_R11 +   mean_Y1994 + mean_Y1995 + mean_Y1996 + mean_Y1997 + mean_Y1998 + mean_Y1999 + mean_Y2000 + mean_Y2001 + mean_Y2002 + mean_Y2003 + mean_Y2004 + mean_Y2005 + mean_Y2006 + mean_Y2007 + mean_Y2008, data= df_wmeans,  family = binomial('probit'), model = "random")
summary(prob5)
#nobs(prob5)

prob5_2<-glm(UEStatus ~ LagUEStatus + age + married + Q1 + Q2 + Q3 + Q4 + Q5 + R + Y + claimant_count_pc + mean_claimant_count_pc + mean_married + mean_Q1 + mean_Q2 + mean_Q3  + mean_Q4 + mean_Q5 + mean_R1 + mean_R2 + mean_R3 + mean_R4 + mean_R5 + mean_R6 + mean_R7 + mean_R8 + mean_R9 + mean_R10 + mean_R11 +    mean_Y1994 + mean_Y1995 + mean_Y1996 + mean_Y1997 + mean_Y1998 + mean_Y1999 + mean_Y2000 + mean_Y2001 + mean_Y2002 + mean_Y2003 + mean_Y2004 + mean_Y2005 + mean_Y2006 + mean_Y2007 + mean_Y2008,  data= df_wmeans,  family = binomial('probit'))
summary(prob5_2)
nobs(prob5_2)
cl.cov52 <- cluster.vcov(prob5_2, df_wmeans$pid)
clustered_se5_2 <- sqrt(diag(cl.cov52))

margins(prob5_2, variables="LagUEStatus")
margins_prob5_2<-margins(prob5_2, variables="LagUEStatus")

probInit<-pglm(UEStatus ~ LagUEStatus + UEStatus1992 + age + married + Q1 + Q2 + Q3 + Q4 + Q5 + R + Y + claimant_count_pc + mean_claimant_count_pc + mean_married + mean_Q1 + mean_Q2 + mean_Q3  + mean_Q4 + mean_Q5 , data= df_wmeans,  family = binomial('probit'), model = "random")
#probInit<-pglm(UEStatus ~ LagUEStatus + UEStatus1992 + age + married + Q1 + Q2 + Q3 + Q4 + Q5 + R + Y + claimant_count_pc + mean_claimant_count_pc + mean_married + mean_Q1 + mean_Q2 + mean_Q3  + mean_Q4 + mean_Q5 + mean_R1 + mean_R2 + mean_R3 + mean_R4 + mean_R5 + mean_R6 + mean_R7 + mean_R8 + mean_R9 + mean_R10 + mean_R11 +   mean_Y1994 + mean_Y1995 + mean_Y1996 + mean_Y1997 + mean_Y1998 + mean_Y1999 + mean_Y2000 + mean_Y2001 + mean_Y2002 + mean_Y2003 + mean_Y2004 + mean_Y2005 + mean_Y2006 + mean_Y2007 + mean_Y2008, data= df_wmeans,  family = binomial('probit'), model = "random")
summary(probInit)
#nobs(probInit)

probInit_2<-glm(UEStatus ~ LagUEStatus + UEStatus1992 +  age + married + Q1 + Q2 + Q3 + Q4 + Q5 + R + Y + claimant_count_pc + mean_claimant_count_pc + mean_married + mean_Q1 + mean_Q2 + mean_Q3  + mean_Q4 + mean_Q5 + mean_R1 + mean_R2 + mean_R3 + mean_R4 + mean_R5 + mean_R6 + mean_R7 + mean_R8 + mean_R9 + mean_R10 + mean_R11 +    mean_Y1994 + mean_Y1995 + mean_Y1996 + mean_Y1997 + mean_Y1998 + mean_Y1999 + mean_Y2000 + mean_Y2001 + mean_Y2002 + mean_Y2003 + mean_Y2004 + mean_Y2005 + mean_Y2006 + mean_Y2007 + mean_Y2008,  data= df_wmeans,  family = binomial('probit'))
summary(probInit_2)
nobs(probInit_2)
cl.covI2 <- cluster.vcov(probInit_2, df_wmeans$pid)
clustered_seInit_2 <- sqrt(diag(cl.covI2))

margins(probInit_2, variables="LagUEStatus")
margins_probInit_2<-margins(probInit_2, variables="LagUEStatus")

## Add keep options to the stargazer
#stargazer(prob1, prob2, prob3, prob4, prob4_2, prob5, prob5_2,  probInit_2, se=list(clustered_se1, clustered_se2, NULL, NULL, clustered_se4_2, NULL, clustered_se5_2, clustered_seInit_2), type = "text") 
#stargazer(margins_prob1, margins_prob2,margins_prob4_2, margins_prob5_2, margins_probInit_2, type = "text") 


# Important things to cover
# probit with factor notation:
summary(prob3)
#nobs(prob3)
# Random effects probit

# Correlated random effects probit:
summary(prob5)
#nobs(prob5)
# Wooldridge treatment
summary(probInit)
#nobs(probInit)

# 'Correlated probit:'
nobs(prob5_2)
# 'Correlated probit + Wooldridge'
nobs(probInit_2)





#table(df_wmeans$UEStatus, df_wmeans$cjsten)
df_wmeans <-df_wmeans %>%   
  dplyr::mutate(longspell = ifelse(cjsten>365, 1, ifelse(cjsten<=365, 0, NA)))

table(df_wmeans$UEStatus, df_wmeans$longspell, deparse.level = 2, useNA = "ifany")

## Overlapping spells

df_wmeans$drop[df_wmeans$UEStatus==1 & df_wmeans$longspell==1]<-1
df_wmeans$drop[df_wmeans$UEStatus==0 | df_wmeans$longspell==0]<-0

table(df_wmeans$UEStatus, df_wmeans$drop, deparse.level = 2, useNA = "ifany")

df_dropoverlap <- df_wmeans %>%
  filter(drop!=1)
table(df_dropoverlap$UEStatus, df_dropoverlap$longspell, deparse.level = 2, useNA = "ifany")

#df_dropoverlap$cjsten[df_dropoverlap$cjsten<0]=NA

## At this stage, correctly we would create new means to reflect new composition 
## of the sample, but we'll skip this for reasons of space/time.

# (i.e. replicate all of above. )
probnov<-glm(UEStatus ~ LagUEStatus + age + married + Q1 + Q2 + Q3 + Q4 + Q5 + R + Y + claimant_count_pc , data=df_dropoverlap,  family = binomial('probit'))
summary(probnov)
nobs(probnov)
cl.covnov <- cluster.vcov(probnov, df_dropoverlap$pid)
clustered_senov <- sqrt(diag(cl.covnov))
margins(probnov, variables="LagUEStatus")
margins_probnov<-margins(probnov, variables="LagUEStatus")


#prob6<-pglm(UEStatus ~ LagUEStatus + age + married + Q1 + Q2 + Q3 + Q4 + Q5 + R + Y + claimant_count_pc + mean_claimant_count_pc + mean_married + mean_Q1 + mean_Q2 + mean_Q3  + mean_Q4 + mean_Q5 + mean_R1 + mean_R2 + mean_R3 + mean_R4 + mean_R5 + mean_R6 + mean_R7 + mean_R8 + mean_R9 + mean_R10 + mean_R11 +   mean_Y1994 + mean_Y1995 + mean_Y1996 + mean_Y1997 + mean_Y1998 + mean_Y1999 + mean_Y2000 + mean_Y2001 + mean_Y2002 + mean_Y2003 + mean_Y2004 + mean_Y2005 + mean_Y2006 + mean_Y2007 + mean_Y2008,  data= df_dropoverlap,  family = binomial('probit'), model = "random")
#margins(prob6, variables="LagUEStatus")
#margins_prob6<-margins(prob6, variables="LagUEStatus")


prob6_2<-glm(UEStatus ~ LagUEStatus + age + married + Q1 + Q2 + Q3 + Q4 + Q5 + R + Y + claimant_count_pc + mean_claimant_count_pc + mean_married + mean_Q1 + mean_Q2 + mean_Q3  + mean_Q4 + mean_Q5 + mean_R1 + mean_R2 + mean_R3 + mean_R4 + mean_R5 + mean_R6 + mean_R7 + mean_R8 + mean_R9 + mean_R10 + mean_R11 +   mean_Y1994 + mean_Y1995 + mean_Y1996 + mean_Y1997 + mean_Y1998 + mean_Y1999 + mean_Y2000 + mean_Y2001 + mean_Y2002 + mean_Y2003 + mean_Y2004 + mean_Y2005 + mean_Y2006 + mean_Y2007 + mean_Y2008, data=df_dropoverlap,  family = binomial('probit'))
summary(prob6_2)
nobs(prob6_2)
cl.cov62 <- cluster.vcov(prob6_2, df_dropoverlap$pid)
clustered_se6_2 <- sqrt(diag(cl.cov62))

margins(prob6_2, variables="LagUEStatus")
margins_prob6_2<-margins(prob6_2, variables="LagUEStatus")

#prob7<-pglm(UEStatus ~ ., data=df_wmeans,  model = "between", effect = "twoways")
#margins(prob7, variables="LagUEStatus")
#margins_prob7<-margins(prob7, variables="LagUEStatus")

prob7_2<-glm(UEStatus ~ LagUEStatus + UEStatus1992 + age + married + Q1 + Q2 + Q3 + Q4 + Q5 + R + Y + claimant_count_pc + mean_claimant_count_pc + mean_married + mean_Q1 + mean_Q2 + mean_Q3  + mean_Q4 + mean_Q5 + mean_R1 + mean_R2 + mean_R3 + mean_R4 + mean_R5 + mean_R6 + mean_R7 + mean_R8 + mean_R9 + mean_R10 + mean_R11 +   mean_Y1994 + mean_Y1995 + mean_Y1996 + mean_Y1997 + mean_Y1998 + mean_Y1999 + mean_Y2000 + mean_Y2001 + mean_Y2002 + mean_Y2003 + mean_Y2004 + mean_Y2005 + mean_Y2006 + mean_Y2007 + mean_Y2008, df_dropoverlap,  family = binomial('probit'))
summary(prob7_2)
nobs(prob7_2)
cl.cov72 <- cluster.vcov(prob7_2, df_dropoverlap$pid)
clustered_se7_2 <- sqrt(diag(cl.cov72))

margins(prob7_2, variables="LagUEStatus")
margins_prob7_2<-margins(prob6_2, variables="LagUEStatus")


## To differentiate marginal effects on older and younger workers, you would need to:

# Split the dataset
# Recreate the means within each dataset
# Rerun the above models, separately.


sink()


