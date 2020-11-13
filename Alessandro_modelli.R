#################################################################################
# REGRESSIONE LINEARE SEMPLICE E MULTIPLA #
#################################################################################

modello=lm(hp~wt,data=mtcars)

summary(modello)
par(mfrow=c(2,2))
plot(modello)
par(mfrow=c(1,1))
#assunti: omoschedasticità (per qualunque Xi), normalità, non sistematicità degli errori
#g1:nuvola ad ampiezza costante senza trend media zero
#g2:normalità
#g4:influenza se alti nelle due coordinate
plot(mtcars$wt, mtcars$hp, pch = 16, cex = 1.3, col = "blue",xlab="",ylab="",main="regressione")
abline(modello,col="red")

modello2=lm(hp~wt+cyl,data=mtcars)
summary(modello2)

anova(modello,modello2) # confronto tra modelli nidificati
AIC(modello,modello2)
BIC(modello,modello2)

#################################################################################
# MODELLO ANOVA, ONE-WAY E KRSUKAL WALLIS #
#################################################################################

anova(lm(Sepal.Length~Species,data=iris)) #lunghezza è diversa nelle specie?
                                          #H0 medie provengono da popolazioni stessa media
summary(aov(Sepal.Length~Species,data=iris)) # stesso risultato diverso codice

summary(aov(Ozone~Month*Day,data=airquality)) #posso usare l'interazione k*j gruppi

#assunzioni: omogeneità varianze gruppi, normalità popolazione, indipendenza (non verif)

bartlett.test(iris$Sepal.Length,iris$Species) # H0 omogeneità varianza
shapiro.test(Sepal.Length[which(Species=="setosa")]) # H0 normalità
shapiro.test(Sepal.Length[which(Species=="virginica")]) # H0 normalità
shapiro.test(Sepal.Length[which(Species=="versicolor")]) # H0 normalità

pairwise.t.test(iris$Sepal.Length, iris$Species,p.adjust="BH",pool.sd=FALSE) # che differenze?


kruskal.test(Sepal.Length~Species,data=iris) # assunzioni violate: normalità
oneway.test(Sepal.Length~Species,data=iris) #assunzioni violate: omogeneità
pairwise.wilcox.test(iris$Sepal.Length, iris$Species,
                     p.adjust.method = "BH") # differenze tra quali categorie?

# SE LE MISURE SONO RIPETUTE: anova repeated mesures or mixed models?
library(readr)
sonno <- read_delim("sonno-long.csv", ";", escape_double = FALSE, trim_ws = TRUE)
sonno$sogg <- as.factor(sonno$sogg)
sonno$tempo <- as.factor(sonno$tempo)
with(sonno, tapply(ore, list(tratt,tempo), mean))
fit <- aov(ore ~ tratt * tempo + Error(sogg/tempo), data=sonno)
summary(fit)
library(lme4)
library(nlme)
mod <- lme(ore ~ tratt * tempo, random=~1|sogg, data=sonno)
model_mix=lmer(ore ~ tratt * tempo + (1|sogg),data=sonno)
summary(mod)
#################################################################################
# REGRESSIONE LOGISTICA #
#################################################################################
library(mlbench)
data("PimaIndiansDiabetes2", package = "mlbench")
dati <- na.omit(PimaIndiansDiabetes2)

model <- glm( diabetes ~., data = dati, family = binomial)
summary(model)

# glucose = 0.042 exp(glucose) = 1.04
# aumento unitario glucose aumenta odds di essere diabetico di 1.04 volte

#################################################################################
# CONFRONTI TRA MEDIE #
#################################################################################

# campioni indipendenti
t.test(dati$glucose~dati$diabetes,paired=FALSE,var.equal=TRUE)
var.test(glucose~diabetes,data=dati)
shapiro.test(dati$glucose[which(dati$diabetes=="neg")])
shapiro.test(dati$glucose[which(dati$diabetes=="pos")])

# campioni appaiati
t.test(dati$glucose~dati$diabetes,paired=TRUE,var.equal=TRUE)
