###################################################################################
# IMPORT & PULIZIA #
###################################################################################

dati <- read.csv2("C:/Users/Filippo/Desktop/dataset.csv", sep=",")

# Pulizia 1
dati <- dati[,-c(1:11,78:79)]

# Rinomino variabili
dati <- dati %>% 
  rename(
    Lduration = Q35,LdurationString = Q35_9_TEXT,Lfrequency = Q10,Lnames = Q11,
    Lexercise1 =Q11_1_TEXT,Lexercise2 = Q11_2_TEXT,Lexercise3 = Q11_3_TEXT,
    Lexercise4 = Q11_4_TEXT,Lexercise5 = Q11_5_TEXT,Lexercise6 = Q11_6_TEXT,
    Lexercise7 = Q11_7_TEXT,Lexercise8 = Q11_8_TEXT,
    Lexercise9 = Q11_9_TEXT,Lexercise10 = Q11_10_TEXT,Lorder =Q24,Lsource =Q28,
    LsourceString= Q28_5_TEXT
  )

# Pulizia 2
dati <- dati[,-4]
dati <- dati[-c(1:2),]

# ID
dati$ID <-1:nrow(dati)


###################################################################################
# RAMO I: DATASET A CON CHIAVE ID#
###################################################################################

dati_A <-dati %>% select(ID,Lexercise1:Lexercise10)
dati_A <-dati_A %>% gather(lexercise,lname,Lexercise1,Lexercise2,Lexercise3,Lexercise4,
                               Lexercise5,Lexercise6,Lexercise7,Lexercise8,Lexercise9,
                               Lexercise10)
dati_A$ID=as.factor(dati_A$ID)
dati_A$lexercise=as.factor(dati_A$lexercise)

###################################################################################
# RAMO II : DATASET B CON CHIAVE ID#
###################################################################################

dati_B <- dati %>% select(ID,Q41:Q41_10_TEXT)
dati_B <-dati_B %>% gather(question,Protocol,Q41,Q41_1_TEXT,Q41_2_TEXT,Q41_3_TEXT,
                         Q41_4_TEXT,Q41_5_TEXT,Q41_6_TEXT,Q41_7_TEXT,Q41_8_TEXT,
                         Q41_9_TEXT,Q41_10_TEXT)
demoinfo <- cbind(Certification=dati$Q14, Certification_time=dati$Q16, Nation=dati$Q58)
dati_B <-cbind(dati_B, demoinfo)

# bisogna cambiare le , in ; da excel se no non separa tutto
dati_B <- separate(
  data = dati_B, 
  col = Protocol,            # column to separate
  into = c("name", "minutes", "timeWeek", "numberWeek"), # new column names
  sep = ";"                       # where to separate
) 

dati_B$ID=as.factor(dati_B$ID)
dati_B$question=as.factor(dati_B$question)
dati_B$minutes=as.factor(dati_B$minutes)
dati_B$timeWeek=as.factor(dati_B$timeWeek)
dati_B$numberWeek=as.factor(dati_B$numberWeek)
dati_B$Certification=as.factor(dati_B$Certification)
dati_B$Certification_time=as.factor(dati_B$Certification_time)
dati_B$Nation=as.factor(dati_B$Nation)

###################################################################################
# RAMO III: DATASET C CON CHIAVE ID #
###################################################################################

dati_C <- dati %>% select(ID,Q34:Q34_10_TEXT)
dati_C <-dati_C %>% gather(question,Protocol,Q34,Q34_1_TEXT,Q34_2_TEXT,Q34_3_TEXT,
                           Q34_4_TEXT,Q34_5_TEXT,Q34_6_TEXT,Q34_7_TEXT,Q34_8_TEXT,
                           Q34_9_TEXT,Q34_10_TEXT)

dati_C$ID=as.factor(dati_C$ID)
dati_C$question=as.factor(dati_C$question)


###################################################################################
# RAMO IV: DATASET D CON CHIAVE ID #
###################################################################################


dati_D <- dati %>% select(ID,Q31:Q31_10_TEXT)
dati_D <-dati_D %>% gather(question,Protocol,Q31,Q31_1_TEXT,Q31_2_TEXT,Q31_3_TEXT,
                           Q31_4_TEXT,Q31_5_TEXT,Q31_6_TEXT,Q31_7_TEXT,Q31_8_TEXT,
                           Q31_9_TEXT,Q31_10_TEXT)

# cambiare , con ; dall'excel o non separa bene
dati_D <- separate(
  data = dati_D, 
  col = Protocol,            # column to separate
  into = c("name", "minutes", "timeWeek", "numberWeek"), # new column names
  sep = ";"                       # where to separate ,!
) 

dati_D$ID=as.factor(dati_D$ID)
dati_D$question=as.factor(dati_D$question)
dati_D$minutes=as.factor(dati_D$minutes)
dati_D$timeWeek=as.factor(dati_D$timeWeek)
dati_D$numberWeek=as.factor(dati_D$numberWeek)


###################################################################################
# NUCLEO DEI RAMI #
###################################################################################

dati_NUCLEO=dati[,-c(4:13,15:25,31:41,43:53)]

# conversioni e formati


