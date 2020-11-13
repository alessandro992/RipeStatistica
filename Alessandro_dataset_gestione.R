library(ggplot2)
library(readxl)
library(dplyr)
library(forcats)
library(tidyverse)
library(viridis)
library(ellipse)
library(RColorBrewer)
library(corrgram)

##################################################################################
# GESTIONE BASE DEL DATASET + DATA VISUALIZATION#
##################################################################################

# Import file txt, excel, sistema (oppure dal wizard)
data_excel <- read_excel("C:/Users/Filippo/Desktop/data.xlsx", sheet = "2014")
dati <- read.csv2("C:/Users/Filippo/Desktop/dati.txt", sep="")
attach(mtcars)
attach(iris)
attach(airquality)

# Vedere dataset a video e osservare le sue info principali
View(dati)
str(dati)
head(dati)
summary(dati)

# Estrarre una colonna e/o assegnarla ad un vettore
dati[,1]
dati[,c(1,2)]
dati$auto
automobile = dati$auto
automobile[3]
vettore1 = c(1,2,3,4)
vettore2 = c("str","alfa","beta",3)
vettore3 = seq(1,100,1)

# Estrarre una riga 
dati[1,]
dati[c(1,2),]
dati[c(1:20),]
riga1 = dati[1,]

# Cancellare righe/colonne
dati_senza_riga1 = dati[-1,]
dati_senza_riga1a10 = dati[-c(1:10),]
dati_senza_colonna1 = dati[,-1]
data_no_missing=na.omit(dati)

# Trasformare formato di una colonna
dati$auto = as.factor(dati$auto) # diventa categoriale
levels(dati$auto) # controllo le categorie della variabile
dati$auto = as.character(dati$auto) # ritorna character
dati$cyl = as.character(dati$cyl)
dati$cyl = as.numeric(dati$cyl)

# Rinominare una colonna
colnames(dati)[1] = "B"

# Creare variabile
dummy = ifelse(dati$cyl==6,1,0)
cyl2=dati$cyl^2
expcyl=exp(dati$cyl)
sqrtcyl=sqrt(dati$cyl)

# Combinare dataset
dati2 = dati
dati_appesi = rbind(dati,dati2)
dati_affiancati = cbind(dati,dati2)

# Analizzare le variabili
table(iris$Species) # tabella di frequenze per variabile categoriale
table(iris$Species)/nrow(iris) # tabella di frequenze relative per variabile categoriale
summary(iris$Sepal.Length) # riepilogo variabile quantitativa
var(iris$Sepal.Length) # Varianza
sd(iris$Sepal.Length) # standard deviation

# Rappresentare le variabili univariate (basic)
boxplot(dati$wt,col="gold",main="boxplot",xlab="x",ylab="y")
hist(dati$wt,main="istogramma",xlab="",ylab="")
a=table(iris$Species)
pie(a, main="Grafico a torta")
barplot(a,main="Barplot")

# Rappresentare le variabili multivariate (basic)
plot(iris$Sepal.Length,iris$Sepal.Width,main="petalo",xlab="lunghezza",ylab="ampiezza")
pairs(iris)
boxplot(iris$Petal.Length ~ iris$Species) 


# Data visualization alternativa (ggplot2,dplyr)
library(ggplot2)
library(dplyr)
# Frequenze
f5=as.data.frame(table(iris$Species))
ggplot(f5, aes(x=Var1, y=Freq)) +
  geom_segment( aes(x=Var1, xend=Var1, y=0, yend=Freq), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(x=NULL,y=NULL,title="Frequenza delle specie")

#Boxplot Categoriale
ggplot(iris, aes(x=as.factor(Species), y=Sepal.Length)) + 
  geom_boxplot(fill="gold", alpha=0.2) + 
  labs(x=NULL,y=NULL,title="Lunghezza fiore per specie")

#Istogramma categoriale
ggplot(iris, aes(x=Sepal.Length))+
  geom_histogram(color="lightblue", fill="gold")+
  facet_grid(Species ~ .)+
  labs(title="Lunghezza fiore per specie",x="", y = "")

# Scatter plot 1
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point() + # Show dots
  facet_grid(iris$Species ~ .)+
  labs(title="Lunghezza v largehzza fiore per le specie",x="", y = "")

# Scatter plot 2
ggplot(airquality, aes(x=Ozone, y=Temp, label=Day)) +
  geom_text(size=1.75)+
  facet_wrap(~Month)+
  labs(title="Ozone v Temp tra mesi e giorni",x="", y = "")

# Scatter plot 3
ggplot(airquality,aes(x=Temp, y=Wind, size = Ozone)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Temp")+
  facet_wrap(~Month)+
  labs(title="Ozone v Temp v Wind nei giorni",x="", y = "")

# Grafico delle correlazioni
dati_cor=dati[,c(4:11)]
data <- cor(mtcars)
my_colors <- brewer.pal(5, "Spectral")
my_colors <- colorRampPalette(my_colors)(100)
ord <- order(data[1, ])
data_ord <- data[ord, ord]
plotcorr(data_ord , col=my_colors[data_ord*50+50] , mar=c(1,1,1,1))


