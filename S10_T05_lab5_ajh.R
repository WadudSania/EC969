## Lab 5 worksheet
## Script by Sonkurt Sen  / Tested and adapted by Angus Holford

## As in the previous lab, we will first set the working directory, clear the environment and open a log file

setwd("M:/EC969/R/EC969")

remove(list=ls())

sink(file = "lab5_final.log", split = T)   #open a log file or can use logr package

## You should have most of these packages from the previous week but in case you missed them, please download again 

install.packages(c("tidyverse", "naniar", "haven", "survey", "labelled","Rmisc","expss","ggplot2", "AER","estimatr","margins","ggeffects", "plm", "ivreg", "mfx", "multiwayvcov", "stargazer","htmltools"))
## If you are asked "Do you want to install from sources the packages which need compilation", type "Yes".

## Let's activate the packages 
packages <-c("tidyverse", "naniar", "haven", "survey", "labelled","Rmisc","expss","ggplot2", "AER","estimatr","margins","ggeffects","ivreg", "plm", "mfx", "multiwayvcov", "stargazer","htmltools")
lapply(packages, library, character.only = T)

## Let's use week 3 final final which is the individual response file
indresp <- readRDS("Lab03_FinalFile.rds")
## Then, merge it with xwave, so first import the data and then merge
xwave<- read_dta(file = "//sernt2/ec969$/UKDA-5151-stata/stata/stata13/bhps_wx/xwavedat.dta") %>%
  dplyr::select(pid, memorig, sex) %>%
  arrange(pid) 

df <- merge(indresp, xwave, by= c("pid")) 

## Let's modify the missing data as usual
missval <- c(-9, -8, -7, -2, -1)        
for (i in 1:length(missval)) {            
  df <- df %>%             
    mutate_all(., list(~na_if(., missval[i]))) 
}

## Let's keep only variables that have non-missing values 
df <- df %>%
  filter(hid>0 & memorig>0)



## Some data exploration
#df %>%
#  select_if(is.numeric) %>%
#  map_dbl(sum)

# Remind ourselves of labels:
attr(x=df$mastat, which="labels")
# Frequency table
table(df$mastat)
# With proportions
df %>% group_by(sex.x, mastat) %>%
  dplyr::summarise(n=n()) %>% 
  mutate(perc = (n/sum(n)) * 100,
         cumperc = cumsum(freq = n / sum(n))*100)
# This table is very long. Let's reduce the categories to a more manageable number


df <-df %>%   
  dplyr::mutate(mstat = ifelse(mastat == 6, 1,
                          ifelse((mastat==1 | mastat==2 | mastat==7), 2,
                                 ifelse((mastat>=3 & mastat<=5)|(mastat>=8 & mastat<=10), 3, NA))),
         qualifications = ifelse(qfachi<=2, 1, 
                                  ifelse(qfachi==3, 2, 
                                         ifelse(qfachi==4, 3, 
                                                ifelse(qfachi==5 | qfachi==6, 4,
                                                       ifelse(qfachi==7, 5, NA))))))  

var_lab(df$mstat) <- "Marital Status (3 cat)"
var_lab(df$qualifications) <- "Qualifications (5 cat)"
val_lab(df$mstat) <- make_labels("
        1 never married
        2 partnered
        3 seperated, widowed, etc.")
val_lab(df$qualifications) <- make_labels("
                                          1 Degree
                                          2 Diploma 
                                          3 A Level
                                          4 O level 
                                          5 None")

df$Female<-0
df$Female[df$sex.x==2]<-1


# Remind ourselves of labels:
attr(x=df$mstat, which="labels")
# And show for nonmissing only:
df %>% filter(is.na(mastat)==0 & is.na(sex.x)==0) %>% 
  group_by(sex.x, mstat) %>%
  dplyr::summarise(n=n()) %>% 
  mutate(perc = (n/sum(n)) * 100,
         cumperc = cumsum(freq = n / sum(n))*100)


## Let's create a histogram showing the percentage of marriage status by wave
## seperately for male and females and put these graphs next to each other
## First let's tell R the outline we would like and create sub samples

df_f <- df %>%
  filter(df$Female==1)

df_m <- df %>%
  filter(df$Female==0)

par(mfrow=c(1,2))
ggplot(data=df_f) + geom_bar(aes(x=as.factor(wave), fill=as.factor(mstat)), position="fill")
ggplot(data=df_m) + geom_bar(aes(x=as.factor(wave), fill=as.factor(mstat)), position="fill")
ggsave(file = "Lab05_marstat_wave_sex.png", width = 10)
## Look in the "plot" winder to see this

par(mfrow=c(1,2))
ggplot(data=df_f) + geom_bar(aes(x=as.factor(qualifications), fill=as.factor(mstat)), position="fill")
ggplot(data=df_m) + geom_bar(aes(x=as.factor(qualifications), fill=as.factor(mstat)), position="fill")
ggsave(file = "Lab05_marstat_quals_sex.png", width = 10)



# Preserve a version so we can come back to it without running all of the above
df_preserved <-df
df<-df_preserved 




df <- pdata.frame(df, index = c("pid", "wave"))

## Let's get a variable for those who married between waves

library(data.table)
setDT(df)
is.data.table(df)

df <- df %>%
  arrange(pid, wave) 
df[, lag.mastat:=c(NA, mastat[-.N]), by=pid]

# I got the original attempt at this variable wrong. Remove and try again
#df <- df%>%
#  df=subset(df, select=-c(FstMar))

df$FstMar[(df$lag.mastat==2 | df$lag.mastat==6) & (df$mastat==2 | df$mastat==6)]<-0
df$FstMar[(df$lag.mastat==2 | df$lag.mastat==6) & (df$mastat==1 | df$mastat==7)]<-1


table(df$FstMar)
table(df$FstMar, df$mastat, deparse.level = 2) 
table(df$FstMar, df$mastat, deparse.level = 2, useNA = "ifany") 
# So the FstMar variable is missing for all but the first year of people's marriages 



## Now let's drop those who are already married for more than one year, or who are separated
## or widowed etc.

df$drop2[is.na(df$FstMar)==1 & df$mastat!=6 & df$mastat!=2]<-1
df$keep2[df$drop2!=1 | is.na(df$drop2)==1]<-1
df<- df %>%
  filter(keep2==1)
table(df$FstMar, df$mastat, deparse.level = 2, useNA = "ifany") 

table(df$FstMar)

# Some possible inconsistencies remain - e.g. switching between widowed and
# never married. Let's assume this reflects changes in how people describe
# partners rather than that they were married.
table(df$FstMar, df$lag.mastat, deparse.level = 2, useNA = "ifany") 
table(df$mastat, df$lag.mastat, deparse.level = 2, useNA = "ifany") 


## The remaining NAs should reflect people with missing lags (because they missed a wave)
# Fill these in

#df$FstMar[is.na(df$FstMar)==1]<-0

# Here attempted to replicate the 'counter' stuff in lines 79-80 ofWeek05_newIssue. 
# Am bypassing - what we have is good enough
#df2 <- setDT(df)[, c(.(counter=sum(FstMar)), lapply(.SD, max)), 
 #                .(pid), .SDcols = grep("FstMar", names(df))]
#View(df2)
#df2 <- df2 %>%
#  df2=subset(df2, select=-c(FstMar))

#df <- merge(df, df2, by='pid', 'wave', all=F)

#df$drop[(df$counter==1 & df$FstMar!=1) | (df$counter>1 & is.na(df$counter)!=1)]<-1
#df$keep[df$drop!=1 | is.na(df$drop)==1]<-1
#
#df <- df %>%
#  filter(keep==1)
#View(df)


table(df$FstMar)

## The data should now be ready.

# Derive a variable for cohabiting
df$coh[df$mastat==2]<-1
df$coh[(df$mastat==1 | df$mastat==6 | df$mastat==7)]<-0
table(df$coh)

## Derive a variable for having children
df$anykids[df$nchild>0 ]<-1
df$anykids[df$nchild==0]<-0
table(df$anykids)

## Derive a variable for having parents in the household
df$parentsHH[((df$hgfno>0 ) | (df$hgmno>0 ))]<-1
df$parentsHH[df$hgfno==0 & df$hgmno==0]<-0
table(df$parentsHH)

## Derive a variable for being enrolled in full-time education
df$enrolled<-0
df$enrolled[(df$scnow==1 | df$fenow==1)]<-1
table(df$enrolled)

## Derive a (factor!) variable for category of hours of work
#df$hrwk[df$is.na(jbft)==1]<-0
df$hrwk[df$jbft==1]<-2
df$hrwk[df$jbft==2]<-1

df <- df %>%
  dplyr::mutate(hrwk = ifelse(is.na(jbft)==1, 0, hrwk) ) 


val_lab(df$hrwk) <- make_labels("0 Not employed
                                 1 Part-time
                                 2 Full-time")
table(df$hrwk)

df$london[(df$region2>=1 & df$region2<=9)]<-0
df$london[df$region2==7]<-1
table(df$london)

df <- df %>%
  arrange(pid, wave)

library(data.table)

setDT(df)
is.data.table(df)

df <- df %>%
  arrange(pid, wave) 
df[, lag.age:=c(NA, age[-.N]), by=pid]
df[, lag.anykids:=c(NA, anykids[-.N]), by=pid]
df[, lag.parentsHH:=c(NA, parentsHH[-.N]), by=pid]
df[, lag.enrolled:=c(NA, enrolled[-.N]), by=pid]
df[, lag.qualifications:=c(NA, qualifications[-.N]), by=pid]
df[, lag.london:=c(NA, london[-.N]), by=pid]
df[, lag.hrwk:=c(NA, hrwk[-.N]), by=pid]
df[, lag.hlghq1:=c(NA, hlghq1[-.N]), by=pid]
df[, lag.coh:=c(NA, coh[-.N]), by=pid]
df[, lag.claimant_count_pc:=c(NA, claimant_count_pc[-.N]), by=pid]

df$keep[(df$ivfio==1 & (df$memorig==1 | df$memorig==5) & df$region2<10 & (df$age>=19 & df$age<=60))]<-1

table(df$lag.anykids)


df <- df %>%
  filter(keep==1)

saveRDS(df, "Lab05_FinalFile.rds")
write.csv(df, "Lab05_FinalFile.csv")

#install.packages("mfx")
#library(mfx)

## Let's run logit regressions and look at marginal probabilities
## First, start with females

#df <-df %>%   
#  dplyr::mutate(FstMar = FstMar.y) # Angus trying to get the regression to run. WHY DO I HAVE TO DO THIS???
table(df$FstMar)

df$lag.age<-as.numeric(df$lag.age)
table(df$lag.anykids)

lm_version <-lm(FstMar ~ lag.age:lag.age+lag.coh+lag.anykids+lag.parentsHH+lag.enrolled+lag.qualifications+
                  lag.london+lag.hrwk+lag.hlghq1+lag.claimant_count_pc, data=df, subset= Female==1)
summary(lm_version)
# This version is clearly treating factor variables as continuous, and the continuous variable as a factor


df$pid<-factor(df$pid)
df$lag.anykids<-factor(df$lag.anykids)
df$lag.parentsHH<-factor(df$lag.parentsHH)
df$lag.enrolled<-factor(df$lag.enrolled)
df$lag.qualifications<-factor(df$lag.qualifications)
df$lag.london<-factor(df$lag.london)
df$lag.hrwk<-factor(df$lag.hrwk)
df$lag.claimant_count_pc<-as.numeric(df$lag.claimant_count_pc)

df<-df  %>%
  dplyr::mutate(lag.age2=(lag.age)^2)


# Do a linear probability model version in the first instance, as a sense check.
lm_version <-lm(FstMar ~ lag.age+lag.age2+lag.coh+lag.anykids+lag.parentsHH+lag.enrolled+lag.qualifications+
                  lag.london+lag.hrwk+lag.hlghq1+lag.claimant_count_pc, data=df, subset= Female==1)
summary(lm_version)
nobs(lm_version)


# Trying to set dataset to exactly the sample
df$keepif<-0
df_female<- df %>% 
  dplyr::select(pid, wave, FstMar, lag.age, lag.coh, lag.anykids, lag.parentsHH, lag.enrolled, lag.qualifications, lag.london, lag.hrwk, lag.hlghq1, lag.claimant_count_pc, Female ) %>%
  dplyr::mutate(keepif= ifelse((Female==1 &  is.na(FstMar)==0 & is.na( lag.age)==0 & is.na(lag.coh)==0 & is.na(lag.anykids)==0 & is.na(lag.parentsHH)==0 & is.na( lag.enrolled)==0 & is.na(lag.qualifications)==0 & is.na(lag.london)==0 & is.na(lag.hrwk)==0 & is.na(lag.hlghq1)==0 & is.na(lag.claimant_count_pc)==0 ), 1, 0 )) %>% 
  dplyr::filter(keepif==1)%>% 
  arrange(pid, wave) 

# Do a logit version:
logit1<-glm(FstMar~lag.age:lag.age+lag.coh+lag.anykids+lag.parentsHH+lag.enrolled+lag.qualifications+
              lag.london+lag.hrwk+lag.hlghq1+lag.claimant_count_pc, data=df_female[df_female$Female==1,], family="binomial")
# These will be the logit coefficients only.
summary(logit1)
nobs(logit1)
cl.cov1<-cluster.vcov(logit1, df_female$pid)
clustered_se1<-sqrt(diag(cl.cov1))
## Then produce the table with standard errors as basic and clustered, respectively
stargazer(logit1, logit1, se=list(NULL, clustered_se1), type = "text") 

## An alternative way to look at the coefficients is with odds ratios, which
## are equal to exponentiated coefficients.
exp(coefficients(logit1))

## Average marginal affects 

# Two ways to do same thing:
me <- margins(logit1)
summary(me)
margins_summary(logit1)


##############################################################################
# To see marginal effects on the predicted probability at specified values 
# of certain variables, we need also to make sure the data is set to data.frame

# https://stackoverflow.com/questions/62330115/r-margins-incorrect-number-of-dimensions
# needs to be solved by making clear this is a data frame
df_female2 <- df_female

df_female2$lag.coh<-factor(df_female2$lag.coh)
df_female2$lag.anykids<-factor(df_female2$lag.anykids)

df_female2 = data.frame(df_female2)

logit2<-glm(FstMar~lag.age:lag.age+lag.coh+lag.anykids+lag.parentsHH+lag.enrolled+lag.qualifications+
              lag.london+lag.hrwk+lag.hlghq1+lag.claimant_count_pc, data=df_female2, family="binomial")


# Marginal effects of mental distress at different ages.
# (Which accounts for fact that age is interacted in the model)

#At the mean age:  
# Fairly raw output
  margins(logit2, variables="lag.hlghq1", at = list(lag.age = mean(df_female2$lag.age, na.rm = T)))
# Nicer, including standard errors
    summary(margins(logit2, variables="lag.hlghq1", at = list(lag.age = mean(df_female2$lag.age, na.rm = T))))

   
     # 'fivenum' produces marginal effects at five values.
 #   minimum value, lower-hinge value (first quartile /  median of lower half), median value, upper-hinge value and maximum value of the input data
  margins(logit2, variables="lag.hlghq1", at = list(lag.age = fivenum(df_female2$lag.age)))
  summary(margins(logit2, variables="lag.hlghq1", at = list(lag.age = fivenum(df_female2$lag.age))))
  
  
  
   ## Interactions of cohabition and presence of children:


  logit3<-glm(FstMar~lag.age:lag.age+lag.coh+lag.anykids+lag.coh:lag.anykids+lag.parentsHH+lag.enrolled+lag.qualifications+
                lag.london+lag.hrwk+lag.hlghq1+lag.claimant_count_pc, data=df_female2, family="binomial")
  summary(logit3)
  # Using factor notation for the interaction, the output is a bit different: We're shown
  # each category relative to the base (in this case, both cohabiting and having children)
  margins_summary(logit3)
  

  # We get an error message if we try to look at the marginal effects of those variables separately
  summary(margins(logit3, variables="lag.coh", at = list(lag.anykids = 1)))
  
 
  # To investigate the marginal effects of interaction terms using margins, need to treat these as continuous variables
  # (This is an inexact approximation!)
  
  df_female2$lag.coh<-as.numeric(df_female2$lag.coh)
  df_female2$lag.anykids<-as.numeric(df_female2$lag.anykids)
  
  
  # Check the values of cohabitation and anykids - want to be
  #sure they are numeric (0/1) rather than their factor values (1/2)
    table(df_female2$lag.coh, df_female2$lag.anykids)
  
  # If we need to change it back
  df_female2$lag.anykids[(df_female2$lag.anykids==1 )]<-0
  df_female2$lag.anykids[(df_female2$lag.anykids==2 )]<-1
  
  df_female2$lag.coh[(df_female2$lag.coh==1 )]<-0
  df_female2$lag.coh[(df_female2$lag.coh==2 )]<-1

  
  

  
  logit4<-glm(FstMar~lag.age:lag.age+lag.coh+lag.anykids+lag.coh:lag.anykids+lag.parentsHH+lag.enrolled+lag.qualifications+
                lag.london+lag.hrwk+lag.hlghq1+lag.claimant_count_pc, data=df_female2, family="binomial")
  summary(logit4)  
  # So a positive sign on cohabitation and anykids, but negative interaction
  
  
    summary(margins(logit4, variables="lag.coh", at = list(lag.anykids = 1)))
    summary(margins(logit4, variables="lag.coh", at = list(lag.anykids = 0)))
    # So on average, the effect of cohabitation on marriage probability is slightly
    #higher if there are no children
    
    summary(margins(logit4, variables="lag.anykids", at = list(lag.coh = 1)))
    summary(margins(logit4, variables="lag.anykids", at = list(lag.coh = 0)))
  # and the point estimate for effect of having children is bigger among those not
  # cohabiting - but not significantly different.

    
  # Plot effect of mental distress, by age
  cplot(logit4, "lag.age", dx="lag.hlghq1",  what = "effect", main = "Marginal effects of mental distress")
  # NB: As variables types have changed, have to estimate based on most recent model
  
  # Plot effect of cohabitation, by anykids
  cplot(logit4, "lag.anykids", dx="lag.coh",  what = "effect", main = "Marginal effects of cohabitation")
 
  # Plot effect of anykids, by cohabitation
   
  
## Other tasks: 
  #replicate all of the above, for men. 
  #Run everything on the pooled sample of men and women, and test whether
  # the marginal effects are different for the two.


sink()

