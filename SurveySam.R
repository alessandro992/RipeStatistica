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

dati <- dati

dati <- read.csv2("dataset.csv", sep=",")

#removing not useful columns
dati <- dati[,-c(1:11,78:79)]
View(dati)
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

View(dati)

dati <- separate(
  data = dati, 
  col = Q41_1_TEXT,            # column to separate
  into = c("Lname", "Lminutes", "LtimeWeek", "LnumberWeek"), # new column names
  sep = ";"                       # where to separate
) 

View(dati)
