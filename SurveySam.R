if (!require(forcats)) {install.packages('forcats')}
if (!require(tidyverse)) {install.packages('tidyverse')}
if (!require(ellipse)) {install.packages('ellipse')}
if (!require(RColorBrewer)) {install.packages('RColorBrewer')}
if (!require(corrgram)) {install.packages('corrgram')}

library(ggplot2)
library(readxl)
library(dplyr)
library(forcats)
library(tidyverse)
library(viridis)
library(ellipse)
library(RColorBrewer)
library(corrgram)


dati <- read.csv2("dataset.csv", sep=",")

#############################################################
# LOW INTENSITY SELF-ADMINISTERED MINDFULNESS INTERVENTION #
###########################################################

#removing not useful columns
dati <- dati[,-c(1:11,78:79)]
#View(dati)
#Create ID
dati$ID <-1:nrow(dati)

#rename variables
dati <- dati %>% 
  rename(
    Lduration = Q35,
    LdurationString = Q35_9_TEXT,
    Lfrequency = Q10,
    Lnames = Q11,
    Lexercise1 =Q11_1_TEXT,
    Lexercise2 = Q11_2_TEXT,
    Lexercise3 = Q11_3_TEXT,
    Lexercise4 = Q11_4_TEXT,
    Lexercise5 = Q11_5_TEXT,
    Lexercise6 = Q11_6_TEXT,
    Lexercise7 = Q11_7_TEXT,
    Lexercise8 = Q11_8_TEXT,
    Lexercise9 = Q11_9_TEXT,
    Lexercise10 = Q11_10_TEXT,
    Lorder =Q24,
    Lsource =Q28,
    LsourceString= Q28_5_TEXT
  )

#remore worthless columns
dati <- dati[,-4]
#remore worthless rows
dati <- dati[-c(1:2),]

#summary and histogram for duration
dati$Lduration <- as.numeric(dati$Lduration)
summary(dati$Lduration)
hist(dati$Lduration,main="duration",xlab="",ylab="")
#Be aware: it won't work now because lduration must be fixed manually before 
#running the script (e.g., weeks, must be converted in days)

#summary and histogram for frequency 
dati$Lfrequency <- as.numeric(dati$Lfrequency)
summary(dati$Lfrequency)
hist(dati$Lfrequency,main="frequency",xlab="",ylab="")

#Is the order relevant?
dati$Lorder <- as.character(dati$Lorder)
table(dati$Lorder)

#source
dati$Lsource <- as.character(dati$Lsource)
table(dati$Lsource)


#select columns for name of exercises
dati2 <-dati %>% select(Lexercise1:Lexercise10)

#Collapse into a single columns for name of exercises 
dati2 <-dati2 %>% gather(lexercise,lname,Lexercise1,Lexercise2,Lexercise3,Lexercise4,Lexercise5,Lexercise6,Lexercise7,Lexercise8,Lexercise9,Lexercise10)

#select columns for name of exercises, duration, frequency and time of the week
dati3 <- dati %>% select(Q41:Q41_10_TEXT)

#Collapse into a single columns for protocol
dati3 <-dati3 %>% gather(ID,Protocol,Q41,Q41_1_TEXT,Q41_2_TEXT,Q41_3_TEXT,Q41_4_TEXT,Q41_5_TEXT,Q41_6_TEXT,Q41_7_TEXT,Q41_8_TEXT,Q41_9_TEXT,Q41_10_TEXT)
demoinfo <- cbind(dati$Q14, dati$Q16, dati$Q58)
dati3 <-cbind(dati3, demoinfo)
dati3 <- dati3[-c(1:7),] #remove some not useful rows
dati3 <- dati3[,-43] #remore worthless columns

#Separate by name,duration,frequency 
dati3 <- separate(
  data = dati3, 
  col = Protocol,            # column to separate
  into = c("name", "minutes", "timeWeek", "numberWeek"), # new column names
  sep = ";"                       # where to separate
) 

?colnames
#############################################################
# HIGH INTENSITY SELF-ADMINISTERED MINDFULNESS INTERVENTION #
###########################################################

#rename variables
dati <- dati %>% 
  rename(
    Hduration = Q22,
    HdurationString = Q22_9_TEXT,
    Hfrequency = Q33,
    Hnames = Q34,
    Hexercise1 =Q34_1_TEXT,
    Hexercise2 = Q34_2_TEXT,
    Hexercise3 = Q34_3_TEXT,
    Hexercise4 = Q34_4_TEXT,
    Hexercise5 = Q34_5_TEXT,
    Hexercise6 = Q34_6_TEXT,
    Hexercise7 = Q34_7_TEXT,
    Hexercise8 = Q34_8_TEXT,
    Hexercise9 = Q34_9_TEXT,
    Hexercise10 = Q34_10_TEXT,
    Horder =Q38,
    Hsource =Q36,
    HsourceString= Q36_5_TEXT
  )

#summary and histogram for duration
dati$Hduration <- as.numeric(dati$Hduration)
summary(dati$Hduration)
hist(dati$Hduration,main="duration",xlab="",ylab="")
#Be aware: it won't work now because lduration must be fixed manually before 
#running the script (e.g., weeks, must be converted in days)

#summary and histogram for frequency 
dati$Hfrequency <- as.numeric(dati$Hfrequency)
summary(dati$Hfrequency)
hist(dati$Hfrequency,main="frequency",xlab="",ylab="")

#Is the order relevant?
dati$Lorder <- as.character(dati$Lorder)
table(dati$Lorder)

#source
dati$Lsource <- as.character(dati$Lsource)
table(dati$Lsource)

#select columns for name of exercises
dati4 <-dati %>% select(Hexercise1:Hexercise10)

#Collapse into a single columns for name of exercises 
dati4 <-dati4 %>% gather(Hexercise,Hname,Hexercise1,Hexercise2,Hexercise3,Hexercise4,Hexercise5,Hexercise6,Hexercise7,Hexercise8,Hexercise9,Hexercise10)

#select columns for name of exercises, duration, frequency and time of the week
dati5 <- dati %>% select(Q31:Q31_10_TEXT)

#Collapse into a single columns for protocol
dati5 <-dati5 %>% gather(ID,Protocol,Q31,Q31_1_TEXT,Q31_2_TEXT,Q31_3_TEXT,Q31_4_TEXT,Q31_5_TEXT,Q31_6_TEXT,Q31_7_TEXT,Q31_8_TEXT,Q31_9_TEXT,Q31_10_TEXT)
dati5 <- cbind(dati5,demoinfo)
dati5 <- dati5[-c(1:7),] #remove some not useful rows
dati5 <- dati5[,-43] #remore worthless columns

#Separate by name,duration,frequency 
dati5 <- separate(
  data = dati5, 
  col = Protocol,            # column to separate
  into = c("name", "minutes", "timeWeek", "numberWeek"), # new column names
  sep = ";"                       # where to separate
) 

