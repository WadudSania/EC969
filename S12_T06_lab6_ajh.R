## Lab 6 worksheet
## Script by Sonkurt Sen  / Tested and adapted by Angus Holford

## As in the previous lab, we will first set the working directory, clear the environment and open a log file

setwd("M:/EC969/R/EC969")

remove(list=ls())
sink(file = "lab6.log", split = T)   #open a log file or can use logr package

## You should have most of these packages from the previous week but in case you missed them, please download again 

install.packages(c("tidyverse", "naniar", "haven", "survey", "labelled","Rmisc","expss","ggplot2", "AER","estimatr","margins","ggeffects", "plm", "ivreg", "fastDummies", "stargazer", "multiwayvcov","htmltools"))
## If you are asked "Do you want to install from sources the packages which need compilation", type "Yes".

## Let's activate the packages 
packages <-c("tidyverse", "naniar", "haven", "survey", "labelled","Rmisc","expss","ggplot2", "AER","estimatr","margins","ggeffects","ivreg", "plm", "fastDummies", "stargazer", "multiwayvcov","htmltools")
lapply(packages, library, character.only = T)

## Let's use week 3 final final which is the individual response file
indresp <- readRDS("Lab03_FinalFile.rds")

indresp <- indresp %>%
  filter(sex==1 & (age>=16 & age<=64) & jbstat==2)

indresp$LnW=log(indresp$paygu)

var_lab(indresp$LnW) <- "Ln wage"
indresp <-indresp %>%   
  dplyr::mutate(married = ifelse((mastat == 1 | mastat==2 | mastat==7), 1,
                           ifelse((mastat>=3 & mastat<=6) | (mastat>=8 & mastat<=10), 0, NA)),
                 Q1 = ifelse((qfachi==1 | qfachi==2), 1, 0),
                 Q2 = ifelse(qfachi==3, 1, 0),
                 Q3 = ifelse(qfachi==4, 1, 0), 
                 Q4 = ifelse(qfachi==5, 1, 0), 
                 Q5 = ifelse(qfachi==6, 1, 0), 
                 Q6 = ifelse(qfachi==7, 1, 0))

## Creating a new variable with the first digit of the soc code
indresp$SOC <- as.numeric(substr(indresp$jbsoc_cc, 1, 1))

table(indresp$region2)

#indresp <- indresp %>%
#  mutate(year = wave + 1990)

table(indresp$year)
table(indresp$SOC)

## Creating region, year and soc dummies (though we will not need them for the regressions)
indresp$R <- indresp$region2
indresp$Y <- indresp$year
indresp <- dummy_cols(indresp, select_columns = c("R", "Y", "SOC"))

indresp <- dummy_cols(indresp, select_columns = c("Y"))
indresp <- dummy_cols(indresp, select_columns = c("SOC"))

 
indresp$PartTime[indresp$jbft==1] <- 0
indresp$PartTime[indresp$jbft==2] <- 1

## Generating job tenure


indresp <- indresp %>%
  dplyr::mutate(JobTenure = year - jbbgy4)

indresp <- indresp %>%
  dplyr::mutate(JobTenure = ifelse(JobTenure<0, 0, JobTenure))

table(indresp$JobTenure)

#indresp$JobTenure <- indresp$year - indresp$jbbgy4
#indresp$JobTenure[indresp$JobTenure==-1]<-0



extra_data<-readRDS("Lab06_ExtraData.rds")

df <- merge(indresp, extra_data, by=c("pid", "wave"), all = T)


# Filter out the people who were *only* in the Extra data
df <- df %>% filter(is.na(hid)==0)


table(df$jhstat, df$jbstat)
table(df$jhstat)


# Look at the variable jhstat and compute a dummy which is 1 for those who 
# entered the current job from unemployment
#tab jhstat jbstat // jhstat only populated for people currently employed // Check derivation in the extra log file in the worksheet.
# jhstat

#df<- df %>%
#  dplyr::mutate(UtoJob = ifelse(jbstat==2 & jhstat!=3, 0, ifelse(jhstat==3, 1, NA)) )
  
df$UtoJob[df$jbstat==2]<-0
df$UtoJob[df$jhstat==3]<-1 

table(df$UtoJob)

var_lab(df$UtoJob)<- "Dummy for those who entered the job from unemployment"

## Estimate the wage equation using OLS
## create the dummy variables for OLS

df$R <- factor(df$R)
df$Y <- factor(df$Y)
df$SOC <-factor(df$SOC)

names(df)[names(df) == "age.x"]<-"age"

df$claimant_count_pc <-  as.numeric(df$claimant_count_pc)  # age is a continuous variable

ols1 <- lm(LnW ~ UtoJob + age + married + JobTenure + PartTime + Q1 + Q2 + Q3 + Q4 + Q5 + R + Y + SOC + claimant_count_pc, data=df)
summary(ols1)
nobs(ols1)

ols2 <- lm(LnW ~ UtoJob + age + married + JobTenure + PartTime + Q1 + Q2 + Q3 + Q4 + Q5 + R + Y + SOC + claimant_count_pc, data=df)
cl.cov2 <- cluster.vcov(ols2, df$pid)
clustered_se <- sqrt(diag(cl.cov2))
summary(ols2)
summary(cl.cov2)
summary(clustered_se)
nobs(ols2)

## Now FE regressions

df <- pdata.frame(df, index = c("pid", "year"))

fe1<- plm(LnW ~ UtoJob + age + married + JobTenure + PartTime + Q1 + Q2 + Q3 + Q4 + Q5 + SOC + R +Y+ claimant_count_pc, data=df, model = "within", effect = "twoways")
summary(fe1)

# For random effects, include the year dummies explicitly instead of within "twoways"
re1 <- plm(LnW ~ UtoJob + age + married + JobTenure + PartTime + Q1 + Q2 + Q3 + Q4 + Q5 + SOC + R +Y+ claimant_count_pc, data=df, model = "random")
summary(re1)

stargazer(fe1 ,re1, type="text", column.labels=c("FE",  "RE"), model.numbers=FALSE)

## Hausman test
phtest(fe1, re1)

## Add interaction of utojob with claimant count
fe2 <- plm(LnW ~ UtoJob + age + married + JobTenure + PartTime + Q1 + Q2 + Q3 + Q4 + Q5 + SOC + R +Y+ claimant_count_pc + claimant_count_pc*UtoJob , data=df, model = "within", effect = "twoways")
summary(fe2)
nobs(fe2)


df$YearsSinceU[is.na(df$UtoJob)==0]<-0

df2 <- df
df2 <- df2 %>%
  filter(UtoJob==1)



######################################


# Code below not working, attempting to produce a Years since Unemployment variable

#df2 <- df2 %>%
#  group_by(year, pid) %>%
#  summarise_at(vars(UtoJob), list(name=pid)) 

#df$YearsSinceU<-df2$
#rm(df2)  
#df$YearsSinceU <- df$YearsSinceu-1

#table(df$YearsSinceU, df$UtoJob)

#fe2<-plm(LnW ~ UtoJob + YearsSinceU + age + married + JobTenure + PartTime + Q1 + Q2 + Q3 + Q4 + Q5 + SOC + R + Y + claimant_count_pc, data=df, model = "within", effect = "twoways")
#




# FE3 needs to have a running sum. 

#df <- df %>% group_by(pid, UtoJob, jbbgy4) %>% dplyr::mutate(nth_years = cumsum(UtoJob))
#df <- df %>% group_by(pid, UtoJob, jbbgy4) %>% dplyr::mutate(N_years = sum(UtoJob))

#df$YearsSinceU2[df$UtoJob==1]<- df$doiy4 - df$jbbgy4 
#df$YearsSinceU2[df$UtoJob==0]<-0

#df <- df %>% 
#dplyr::mutate(YearsSinceU3 = ifelse(UtoJob==1 & is.na(doiy4)==0 & is.na(jbbgy4)==0, doiy4 - jbbgy4, ifelse(UtoJob==0 & is.na(doiy4)==0 & is.na(jbbgy4)==0, 0, NA)))

# Vector error messages.


# Back to what works: Length of spell.






table(df$SpellLength)

df2$LengthUSpell <- df2$UtoJob * df2$SpellLength
df$LengthUSpell <- df$UtoJob * df$SpellLength


df$LengthUSpell[is.na(df$LengthUSpell)==1]<-0
fe4 <- plm(LnW ~ UtoJob + LengthUSpell+ age + married + JobTenure + PartTime + Q1 + Q2 + Q3 + Q4 + Q5 + SOC + R +Y+ claimant_count_pc  , data=df, model = "within", effect = "twoways")
summary(fe4)
nobs(fe4)

fe5 <- plm(LnW ~ LengthUSpell+ age + married + JobTenure + PartTime + Q1 + Q2 + Q3 + Q4 + Q5 + SOC + R +Y+ claimant_count_pc  , data=df2, model = "within", effect = "twoways")
summary(fe5)
nobs(fe5)


stargazer(ols1, ols2, fe1, re1, fe2, fe4, fe5, se=list(NULL, clustered_se, NULL, NULL, NULL, NULL),  type = "text", column.labels=c("OLS", "FE",  "RE", "FE Unemp Int",  "SpellLength UtoJob Only", "SpellLength" ), model.numbers=FALSE)



sink()








#extra_data<-readRDS("Week06_ExtraData.rds")
















