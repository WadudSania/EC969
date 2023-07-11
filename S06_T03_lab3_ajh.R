## Lab 3 worksheet
## Script by Sonkurt Sen / Tested and adapted by Angus Holford

## As in the previous lab, we will first set the working directory, clear the environment and open a log file

setwd("M:/EC969/R/EC969")

remove(list=ls())
sink(file = "lab3.log", split = T)   #open a log file or can use logr package

## You should have most of these packages from the previous week but in case you missed them, please download again 

install.packages(c("tidyverse","htmltools", "naniar", "haven", "survey", "labelled","Rmisc","expss","ggplot2", "AER","estimatr","margins","ggeffects","ivreg"))
## If you are asked "Do you want to install from sources the packages which need compilation", type "Yes".

##Let's activate the packages 
packages <-c("tidyverse","htmltools","naniar", "haven", "survey", "labelled","Rmisc","expss","ggplot2", "AER","estimatr","margins","ggeffects","ivreg")
lapply(packages, library, character.only = T)

## Let's start by merging wave 1 and 2
## In order to merge these two waves, we will first need to import both of them
## Let's keep only some of the variables like we did previous week
## While doing that, let's also create two dummy variables for the waves 

aindresp <- read_dta("//sernt2/ec969$/UKDA-5151-stata/stata/stata13/bhps_w1/aindresp.dta") %>%  
  select(ahid, pid, asex, amastat, aage, aqfachi, alknbrd, apaygu, aregion) %>%
  arrange(pid) %>% 
  mutate(wave1 = 1)

bindresp <- read_dta("//sernt2/ec969$/UKDA-5151-stata/stata/stata13/bhps_w2/bindresp.dta") %>%  
  select(bhid, pid, bsex, bmastat, bage, bqfachi, blknbrd, bpaygu, bregion) %>%
  arrange(pid) %>%
  mutate(wave2 = 1)

## Then, we will merge them
## As you can see under global environment, we have 10 variables each but the merged file has 19 variables
## as the pid variable is included in both waves and used for merging the do files.
wave12 <- merge(aindresp, bindresp, by = "pid", all = T) 

## Let's see if there is any overlap between the two levels by tabulating wave1 and wave2 dummies 
table(wave12$wave1, wave12$wave2, deparse.level = 2, useNA = "ifany") 
## Here we see that there are 8970 observations which are included in both waves, 
## 1294 which is included only in the first wave whle 875 is included only in the second wave 

## Merge command have specific options, for example lets see the following options. 
test1 <- merge(aindresp, bindresp)
view(test1)

## If we would like to merge using only one merging variable and keep only those whose data we have for both waves
test2 <- merge(aindresp, bindresp, by = "pid")
view(test2)
## AJH comment: I don't see how these are different


##If we would like to merge using two merging variables such as pid and hid
test3 <- merge(aindresp, bindresp, by.x = "pid", by.y = "hid") 
view(test3)
## This will provide no merge because hid changes between waves. 

## If we would like to restrict the observations to those whose data we have for both waves 
wave12<-test2

## Now that we have the file that we will use, we can get rid of extra data files 
rm(aindresp, bindresp, test1, test2, test3)

## Similar to what we have done in the previous week, let's recode the missing values 
missval <- c(-9, -8, -7, -2, -1)           # remember to remove missing values first
for (i in 1:length(missval)) {            
  wave12 <- wave12 %>%             
    mutate_all(., list(~na_if(., missval[i]))) 
}

## Summarize the wage variables in both waves
summarySE(wave12, measurevar = "apaygu", na.rm = T)
summarySE(wave12, measurevar = "bpaygu", na.rm = T)

## or if we would like to know about the quartiles, max, min
summary(wave12$apaygu)
summary(wave12$bpaygu)

## Compute changes between two waves 
wave12 <- wave12 %>%
  mutate(paych = bpaygu - apaygu)
## or 
wave12$change_pay=wave12$bpaygu - wave12$apaygu

## How does change in pay vary by gender (getting the descriptive stats by gender)
mean(wave12$paych, na.rm = T)
summarySE(wave12, measurevar = "paych", groupvars = "asex", na.rm = T)
View(wave12)
## Saving the dataset
write.csv(wave12, "Lab03_ab_wide.csv")
saveRDS(wave12, "Lab03_ab_wide.rds")

## Let's merge the files in long format 

## In wide format, the same variable measured at different points in time had a different 
## variable name (e.g. amastat for wave 1 and bmastat for wave 2)

## In long format, the different time periods are captured by the variable 'wave'
## but the other variables need the same name in order to be stacked on top of each other.

## Rename is therefore part of the process of reading each dataset.

aindresp <- read_dta("//sernt2/ec969$/UKDA-5151-stata/stata/stata13/bhps_w1/aindresp.dta") %>%  
  select(ahid, pid, asex, amastat, aage, aqfachi, alknbrd, apaygu, aregion) %>%
  dplyr::rename(hid = ahid,
         sex = asex,
         age = aage,
         mastat = amastat,
         qfachi = aqfachi,
         paygu = apaygu,
         lknbrd = alknbrd,
         region = aregion) %>%
  mutate(wave = 1)

bindresp <- read_dta("//sernt2/ec969$/UKDA-5151-stata/stata/stata13/bhps_w2/bindresp.dta") %>%  
  select(bhid, pid, bsex, bmastat, bage, bqfachi, blknbrd, bpaygu, bregion) %>%
  dplyr::rename(hid = bhid,
                sex = bsex,
                age = bage,
                mastat = bmastat,
                qfachi = bqfachi,
                paygu = bpaygu,
                lknbrd = blknbrd,
                region = bregion) %>%
  mutate(wave = 2)

## Now let's append the first wave with the second wave
## In order to append the datasets, we use bind_rows package
longwave <- bind_rows(aindresp, bindresp) 
## And, then get rid of the individual waves
rm(aindresp, bindresp)

## As usual, recode the missing variables 
missval <- c(-9, -8, -7, -2, -1)           # remember to remove missing values first
for (i in 1:length(missval)) {            
  longwave <- longwave %>%             
    mutate_all(., list(~na_if(., missval[i]))) 
}

## Let's look at the variable names
names(longwave)
## Then, get the descriptive statistics for pay (both wave 1 and 2)
summarySE(longwave, measurevar = "paygu", na.rm = T) 
## This time, get the descriptive statistics for pay by wave 
summarySE(longwave, measurevar = "paygu", groupvars = "wave", na.rm = T)

## We will now drop those who appear in the data only once (who attended only one wave)
## First calculate how many times someone has appeared in the data
N <- longwave %>% group_by(pid) %>% dplyr::summarise(N = n())  

## We first add N into the longwave dataset (ie. merge) and then filter to keep those whose N is equal to 2
longwave <- merge(longwave, N, by = "pid", all= T) %>% 
  filter(N == 2)
## Then we drop N
rm(N)

## We can also compute the change in pay using the lag function 
longwave <- longwave %>%
  arrange(pid, wave) %>%  ## Sorting the data by pid and wave
  group_by(pid) %>%   ## We group it by pid so that it can calculate the changes within pid (ie. per person)
  dplyr::mutate(pay_lag = lag(paygu, n = 1, default = NA)) %>%  ## lag creates a pay variable for the lagged wave (i.e., the first one). Note the specification of mutate from dplyr-without this, sometimes the function picks up mutate from a different package that is incompatible with lag()
  dplyr::mutate(paych = paygu - pay_lag) %>%  
  ungroup() # ungroup to return to long form


## Label this new variable
var_lab(longwave$paych) <- "Change in pay"
## Drop the original variable
rm(paych) 

## Change in the pay by gender
summarySE(longwave, measurevar = "paych", groupvars = "sex", na.rm = T)


## Save the dataset
write.csv(longwave, "Lab03_ab_long.csv")
saveRDS(longwave, "Lab03_ab_long.rds")

## Reshaping data from wide to long and from long to wide

## Make sure pid and wave is not a numeric value but a factor value 
longwave$pid <- factor(longwave$pid) 
longwave$wave <- factor(longwave$wave)

## data= name of the data, id_cols= personal id, names_from=survey wave, values_from= the values that need to be reshaped
widewave <- pivot_wider(data = longwave, id_cols = pid, names_from = wave, 
                        values_from = c(hid, sex:region)) 

## Reshaping from wide to long format is more complicated, because R tends to gather together everything 
##and doesn't easily identify which variable you want to repeat 
# Instead we filter by each wave and use the bind_rows() procedure to stack the two sets on top of each other.

wave12 <- wave12 %>% mutate(wave = ifelse(wave1 == 1, 1, 2))  # first we add back a wave identifier

## Select wave 1 variables and rename the variables by removing the prefix
long1 <- wave12 %>%          
  dplyr::select(pid:aregion, wave1) %>%
  rename_at(dplyr::vars(starts_with("a")), ~str_replace(., "a", ""))   

long2 <- wave12 %>%           # repeat for wave 2
  dplyr::select(pid, bhid:bregion, wave2) %>%
  rename_at(dplyr::vars(starts_with("b")), ~str_replace(., "b", ""))

long12 <- bind_rows(long1, long2) %>%    # bind them together
  arrange(pid)   
## Error message here about conflicting value labels - it takes the first.


## We can do the same on the widewave variable, with a bit of variation
wide.to.long1 <- widewave %>%
  dplyr::select(pid, hid_1, sex_1, mastat_1, age_1, qfachi_1, lknbrd_1, paygu_1, region_1) %>%
  rename_at(dplyr::vars(ends_with("1")), ~str_replace(., "_1", "")) %>%
  mutate(wave = 1)  # remember to add the wave identifier, as we removed it earlier!

wide.to.long2 <- widewave %>%
  dplyr::select(pid, hid_2, sex_2, mastat_2, age_2, qfachi_2, lknbrd_2, paygu_2, region_2) %>%
  rename_at(dplyr::vars(ends_with("2")), ~str_replace(., "_2", "")) %>%
  mutate(wave = 2)

wide.to.long <- bind_rows(wide.to.long1,wide.to.long2)

## Merging files into long format
## Let's merge 18 waves together

nwaves <- 18  
variables <- list("pid","hid","sex","age","mastat","qfachi","paygu","jbstat","lknbrd", "region2", "jbft", "jbbgy4", "jbsemp", "cjsten", "jbsoc_cc", "doim", "ivfio", "nchild", "hlghq1", "hgfno", "hgmno", "fenow", "scnow", "doim") # list of variables you want in the data 
## We need an empty data frame to load the data into
df <- data.frame()  

for (i in 1:nwaves){    
  letter <- letters[i]  ## We make use of the letters vector inbuilt into R to reference the letter for each wave number (i.e., 1 = a, 2 = b)
  vars <- paste(letter, variables[-1], sep = "") # We paste in the letter for each variable in our list, with the exception of pid 

  setwd(paste0("//sernt2/ec969$/UKDA-5151-stata/stata/stata13/bhps_w",i)) # Tell R where to look)
  data <- read_dta(file = paste0(letter,"indresp.dta")) %>% # paste in the letter for the data file name as well
    dplyr::select(pid, vars)
  colnames(data) <- variables   # rename all columns to the list you created before
  data$wave <- i # create a variable 'wave' that records which wave this dataset was from  
  df <- bind_rows(df, data)  # bind on the loaded data below the others
}
View(df)
df <- df %>%
  mutate(year = wave + 1990)

rm(variables,nwaves, data, vars, letter)

## If variable selection did not work, we can run the below line

## As usual recode the missing values 
missval <- c(-9, -8, -7, -2, -1)          
for (i in 1:length(missval)) {            
  df <- df %>%             
    mutate_all(., list(~na_if(., missval[i]))) 
}

summary(df)
summarySE(df, measurevar = "paygu", groupvars = "wave", na.rm = T)

## Let's see what regions are included and then let's drop Northern Ireland and Chanel Islands
unique(df$region2) 
df <- df %>%
  filter(region2 != 12 | region2 != 13 | is.na(region2)) %>%
  arrange(pid, wave)

df<- df %>%
  arrange(pid,wave)

setwd("M:/EC969/R/EC969")


saveRDS(df, "Lab03_FinalFile.rds")
write.csv(df, "Lab03_FinalFile.csv")

## Let's read data from an external source

unemp <- read_csv(file = "//sernt2/EC969$/ForLinkage/claimantcount_1991-2009.csv") 
colnames(unemp)
## Let's change the variable names
names(unemp)[names(unemp) == "...3"] <- "rate_region1" # This will match with the variable region2
names(unemp)[names(unemp) == "...5"] <- "rate_region2"
names(unemp)[names(unemp) == "...7"] <- "rate_region3"
names(unemp)[names(unemp) == "...9"] <- "rate_region4"
names(unemp)[names(unemp) == "...11"] <- "rate_region5"
names(unemp)[names(unemp) == "...13"] <- "rate_region6"
names(unemp)[names(unemp) == "...15"] <- "rate_region7"
names(unemp)[names(unemp) == "...17"] <- "rate_region8"
names(unemp)[names(unemp) == "...19"] <- "rate_region9"
names(unemp)[names(unemp) == "...21"] <- "rate_region10"
names(unemp)[names(unemp) == "...23"] <- "rate_region11"
names(unemp)[names(unemp) == "...25"] <- "rate_region12"
names(unemp)[names(unemp) == "Claimant Count - seasonally adjusted"] <- "Date"

## Because we are only interested in the rates and not the numbers
unemp <- unemp %>% select(Date, starts_with("rate_region"))

## Let's keep only if there is a non-missing values 
unemp <- unemp %>%
  filter(rate_region1>-0.1,
         is.na(Date)==0)

# Extract the values for each region, then stack them on top of each other

unemp_long <- data.frame()  # create an empty data frame container to bind the data to when we load it in

for (i in 1:12) {    
  var <- paste("rate_region", i, sep = "")
  data <- unemp %>% select(Date, var)
  data <- dplyr::rename(data, 
                        rate_region = var)
  data$region2 <- i
  unemp_long <- bind_rows(unemp_long, data)  # bind on the loaded data below the others
}

## Finding the number of columns after splitting by space
ncols <- max(stringr::str_count(unemp_long$Date, " ")) + 1
colmn <- paste("Date", 1:ncols)

unemp_long <-
  tidyr::separate(
    data = unemp_long,
    col = Date,
    sep = " ",
    into = colmn,
    remove = FALSE
  )

unemp_long <-   unemp_long  %>% select(-Date)

## Rename the variables 
names(unemp_long)[names(unemp_long) == "Date 1"] <- "month"
names(unemp_long)[names(unemp_long) == "Date 2"] <- "doiy4"

## Here we use ifelse 
unemp_long <- unemp_long %>%
  mutate(doim = ifelse(month =="January", 1, 
                       ifelse(month =="February", 2,
                              ifelse(month =="March", 3,
                                     ifelse(month =="April", 4,
                                            ifelse(month =="May", 5,
                                                   ifelse(month =="June", 6,
                                                          ifelse(month =="July", 7,
                                                                 ifelse(month =="August", 8,
                                                                        ifelse(month =="September", 9,
                                                                               ifelse(month =="October", 10,
                                                                                      ifelse(month =="November", 11,
                                                                                             ifelse(month =="December", 12, NA)))))))))))))

unemp_long <-   unemp_long  %>% select(-month)

## Let's save this dataset
saveRDS(unemp_long, "Lab03_unemp_long.rds")
write.csv(unemp_long, "Lab03_unemp_long.csv")

## Let's link our survey data
## First import the long data
df <- read_rds(file = "Lab03_FinalFile.rds") 

## Create a variable for the year of interview
df <- df %>%
  mutate(doiy4 = wave + 1990)

## If the interview was conducted late
df <- df %>%
  mutate(late_int = ifelse(doim>=1 & doim<=5, 1, 0  ))

## Replace the interview year 
df <- df %>%
  mutate(doiy4 = doiy4 + late_int) 

## Link BHPS Long with unemployment data 
Lab03_Temp <- merge(df, unemp_long, by= c("region2", "doim", "doiy4"), all = T)
Lab03_Temp <- Lab03_Temp %>%
    arrange(pid, wave)
Lab03_Temp$claimant_count_pc<-Lab03_Temp$rate_region

saveRDS(Lab03_Temp, "Lab03_FinalFile.rds")
write.csv(Lab03_Temp,"Lab03_FinalFile.csv")

## Now let's work with the new long data

## Let's start by computing the changes in wages and changes in marital status
df <- readRDS("Lab03_FinalFile.rds")
unique(df$mastat)
df <- df %>%
  mutate(ma = ifelse(mastat %in% c(1,2,7), 1,     ## the %in% c() does the same thing as mastat == 1 | mastat == 2, which you can also use
                     ifelse(mastat %in% c(3,4,5,8, 9, 10), 2,
                            ifelse(mastat == 6, 3, NA))))

val_lab(df$ma) <- make_labels("1 married, civil partnership, or living as couple
                              2 widowed, divorced or separated
                              3 never married")

## make new variable for transitions in marriage 
df <- df %>%
  arrange(pid, wave) %>%  # make sure data is sorted by waves
  group_by(pid) %>%   # group by pid so the lag function can identify changes across pid
  dplyr::mutate(ma_lag = lag(ma, 1, default = NA)) %>%  # lag creates a marital status variable for the lagged wave (i.e., the first one)
  dplyr::mutate(mach = ma_lag*10 + ma) %>%    # we create a two-digit variable that code earlier marital status and later marital status
  ungroup() # ungroup to return to long form

## Let's add labels
var_lab(df$mach) <- "marital change"

val_lab(df$mach) <- make_labels("
                                11 stayed in couple
                                12 partnership ended
                                13 partnered -> never married
                                21 expartner -> partnership
                                22 stayed ex-partner
                                23 ex-partner -> never married
                                31 never married -> partnership
                                32 never married -> ex-partner
                                33 stayed never married")

## We can now analyse transitions. How does marriage vary over time and by sex?
df %>% group_by(mach, sex)  %>% filter(sex==1) %>% dplyr::summarise(n = n()) %>% mutate(perc = n*100/sum(n),
                                                                                        cumperc = cumsum(freq = n / sum(n)) * 100)
df %>% group_by(mach, sex)  %>% filter(sex==2) %>% dplyr::summarise(n = n()) %>% mutate(perc = n*100/sum(n),
                                                                                        cumperc = cumsum(freq = n / sum(n)) * 100)

## Now we create the variable `paych`, which measures the change in a person's earnings from one wave to the next. How does the change in pay vary by sex?
df <- df %>%
  arrange(pid, wave) %>%
  group_by(pid) %>%
  dplyr::mutate(pay_lag = lag(paygu, 1, default = NA)) %>%
  dplyr::mutate(paych = paygu - pay_lag) %>%
  ungroup()

summarySE(df, measurevar = "paych", groupvars = "sex", na.rm = T)

##  How does the change in pay vary over time by type of marital change and sex?
summarySE(df, measurevar = "paych", groupvars = c("sex","mach"), na.rm = T)

sink() # close and save the log file

