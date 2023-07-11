## Lab 6 - Deriving extra data worksheet
## Script by Sonkurt Sen / Tested and adapted by Angus Holford

setwd("M:/EC969/R/EC969")

remove(list=ls())
sink(file = "lab6_extra.log", split = T)   #open a log file or can use logr package

nwaves <- 18  
variables <- list("jhstat", "jhbgm", "jhbgy4", "jhendm", "jhendy4" ) 
jobhist <- data.frame()  

for (i in 1:nwaves){    
  letter <- letters[i]
  vars <- paste(letter, variables, sep = "") 
  setwd(paste0("//sernt2/ec969$/UKDA-5151-stata/stata/stata13/bhps_w",i)) 
  data <- read_dta(file = paste0(letter,"jobhist.dta")) %>% 
    dplyr::select(pid, vars)
  colnames(data) <- c("pid", variables)  
  data$wave <- i 
  jobhist <- bind_rows(jobhist, data)  
}
View(jobhist)

setwd("M:/EC969/R/EC969")

xwave<- read_dta(file = "//sernt2/ec969$/UKDA-5151-stata/stata/stata13/bhps_wx/xwavedat.dta") %>%
  arrange(pid)

df_ex<-merge(jobhist, xwave, by="pid")
drop <- c("plbornc_cc", "yr2uk4", "race", "racel", "paju", "pasoc_cc", "pasemp", "paboss", "pamngr", "maju", "masoc_cc", "masemp", "maboss", "mamngr")
df_ex = df_ex[,!(names(df_ex) %in% drop)]
View(df_ex)


df_ex$age_v1 <- 1990 + df_ex$wave - df_ex$doby
df_ex$SpellLength_v1 <- ((df_ex$jhendy4 - df_ex$jhbgy4)*12 ) + df_ex$jhendm - df_ex$jhbgm
df_ex$SpellLength_v1[df_ex$SpellLength_v1>120 | df_ex$SpellLength_v1<0]<-NA

for (i in (1:nrow(df_ex))){
  df_ex$age_v2[i] <- 1990 + df_ex$wave[i] - df_ex$doby[i]
}

df_ex<- df_ex %>%
  dplyr::mutate(age = 1990 + wave - doby )
df_ex<- df_ex %>%
  dplyr::mutate(SpellLength = ((jhendy4 - jhbgy4)*12 ) + jhendm - jhbgm )
df_ex<- df_ex %>%
  dplyr::mutate(SpellLength = ifelse(SpellLength>120 | SpellLength < 0, NA, SpellLength) )

df_ex <- df_ex %>%
  arrange(pid, wave, jhendm, jhendy4) 

table(df_ex$SpellLength)


# Initial attempt to panel data set here didn't work (properly) because there's 
# multiple observations per person per wave - 
# This is those with more than one spell between waves. 

# So first i want to keep only the last one.
#df$keep[df$drop!=1 | is.na(df$drop)==1]<-1
df_ex$counter<-1

# To generate the running count:


df_ex <- df_ex %>% group_by(pid, wave) %>% dplyr::mutate(nth = cumsum(counter))
df_ex <- df_ex %>% group_by(pid, wave) %>% dplyr::mutate(N = sum(counter))

df_ex <- df_ex %>% filter(N == nth)


# To generate the total count
#N <- df_ex %>% group_by(pid, wave) %>% dplyr::summarise(N = n())  
## We first add N into the longwave dataset (ie. merge) and then filter to keep those whose N is equal to 2
#df <- merge(df_ex, N, by = c("pid", "wave"), all= T) %>% 
 # filter(N == nth)
## Then we drop N
#rm(N)

df_ex <- df_ex %>% select(pid, wave, jhstat, jhbgm, jhbgy4, jhendm, jhendy4,  SpellLength, sex, age)

df_ex <- pdata.frame(df_ex, index = c("pid", "wave"))

df_ex <- dplyr::rename(df_ex,  sex_jobhist = sex, age_jobhist = age) 

# Here need to keep only a set of variables
# jhstat jhbgm jhbgy4 jhendm jhendy4 pid wave SpellLength sex age

# And only keep one observation per person per wave. (i.e. remove duplicates)
#bys pid wave: gen N=_N
#bys pid wave: gen n=_n
#tab N if n==N
#keep  if n==N
#drop N n
#bys pid wave: gen N=_N
#tab N





View(df_ex)
setwd("M:/EC969/R/EC969")
saveRDS(df_ex, "Lab06_ExtraData.rds")

sink()









