summary.data.frame(ECSA3)
cor(ECSA3, use = "complete.obs") # correlation entre les variables 
cor <-cor(ECSA3)
summary(cor)

q2<- lm(MaMerlot...2 ~ AN,data=ECSA3)
summary(q2)
library(ggplot2)
ggplot(ECSA3, aes(x = AN, y=MaMerlot...2)) + geom_point() + geom_line() + ggtitle('Maturit� du Merlot entre 1960 et 2000')+ xlab("Ann�e") + ylab("MaMerlot")
cor(ECSA3$MaMerlot...2,ECSA3$NbJ20_25JuS)
q3 <- lm(MaMerlot...2 ~ AN + NbJrs30JJ + NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ + SommePluviometrieJJ + SommeRayonnementJJ + NbJrs30JuS + SommeTs10JuS + SommePluviometrieJuS +SommeRayonnementJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS + NbJ10_15JuS + Veraison, data=ECSA3)
summary(q3)
names(q3)
residuals(q3)
coeff(q3)
confint(q3)
essai1 <- lm(MaMerlot...2 ~  NbJrs30JJ + SommeRayonnementJJ + NbJrs30JuS +SommeRayonnementJuS , data=ECSA3)
summary(essai1)
essai2 <-lm(MaMerlot...2~SommePluviometrieJJ+SommePluviometrieJuS, data=ECSA3)
summary(essai2)
essai3 <-lm(MaMerlot...2~SommePluviometrieJJ+SommePluviometrieJuS+SommeRayonnementJJ+SommeRayonnementJuS, data=ECSA3)
summary(essai3)
essai1 <- lm(MaMerlot...2 ~  NbJrs30JJ + SommeRayonnementJJ + NbJrs30JuS +SommeRayonnementJuS , data=ECSA3)
summary(essai1)
essai2 <-lm(MaMerlot...2~SommePluviometrieJJ+SommePluviometrieJuS, data=ECSA3)
summary(essai2)
essai3 <-lm(MaMerlot...2~SommePluviometrieJJ+SommePluviometrieJuS+SommeRayonnementJJ+SommeRayonnementJuS, data=ECSA3)
summary(essai3)
essai4 <-lm(MaMerlot...2~ NbJ20_25JJ + NbJ25_30JJ + NbJ20_25JuS+ NbJ25_30JuS, data=ECSA3)
summary(essai4)
essai5 <-lm(MaMerlot...2~ NbJrs30JuS + NbJrs30JJ + NbJ25_30JJ + NbJ25_30JuS, data=ECSA3)
summary(essai5)
essai6 <-lm(MaMerlot...2~ NbJrs30JuS + NbJrs30JJ + NbJ25_30JJ + NbJ25_30JuS + NbJ20_25JJ +NbJ20_25JuS, data=ECSA3)
summary(essai6)
install.packages("corrplot")
library(corrplot)
cor <-cor(ECSA3)
corrplot(cor, method="color",tl.col = 'black', type ='lower', tl.cex = 0.8,addCoef.col = "black" )
library(ggplot)

ggplot(ECSA3, aes(x = AN, y=MaMerlot...2)) + geom_point() + geom_line() + ggtitle('Maturité du Merlot entre 1960 & 2000')+ xlab("Année") + ylab("MaMerlot") +stat_smooth(method="lm", se=FALSE)
essai7 <-lm(MaMerlot...2~SommePluviometrieJJ+SommeRayonnementJuS, data=ECSA3)
summary(essai7)


# selection de variables

M1 <- lm(MaMerlot...2 ~ AN + NbJrs30JJ + NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ + SommePluviometrieJJ + SommeRayonnementJJ + NbJrs30JuS + SommeTs10JuS + SommePluviometrieJuS +SommeRayonnementJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS + NbJ10_15JuS + Veraison, data=ECSA3)
summary(M1)
M2 <- lm(MaMerlot...2 ~  NbJrs30JJ + NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ + SommePluviometrieJJ + SommeRayonnementJJ + NbJrs30JuS + SommeTs10JuS + SommePluviometrieJuS +SommeRayonnementJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS, data=ECSA3)
summary(M2)
M3 <- lm(MaMerlot...2 ~ NbJrs30JJ + NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ + SommePluviometrieJJ + SommeRayonnementJJ + NbJrs30JuS + SommeTs10JuS + SommePluviometrieJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS, data=ECSA3)
summary(M3)
M4 <- lm(MaMerlot...2 ~ NbJrs30JJ + NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ + SommePluviometrieJJ + SommeRayonnementJJ + NbJrs30JuS + SommePluviometrieJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS, data=ECSA3)
summary(M4)
M5 <- lm(MaMerlot...2 ~ NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ + SommePluviometrieJJ + SommeRayonnementJJ + NbJrs30JuS + SommePluviometrieJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS, data=ECSA3)
summary(M5)
M6<- lm(MaMerlot...2 ~ NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ + SommePluviometrieJJ + SommeRayonnementJJ + SommePluviometrieJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS, data=ECSA3)
summary(M6)

# M1 <- lm(MaMerlot...2 ~ AN + NbJrs30JJ + NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ + SommePluviometrieJJ + SommeRayonnementJJ + NbJrs30JuS + SommeTs10JuS + SommePluviometrieJuS +SommeRayonnementJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS + NbJ10_15JuS + Veraison, data=ECSA3)
# M2 <- lm(MaMerlot...2 ~ AN + NbJrs30JJ + NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommePluviometrieJJ + SommeRayonnementJJ + NbJrs30JuS + SommeTs10JuS + SommePluviometrieJuS +SommeRayonnementJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS +  Veraison, data=ECSA3)
# summary(M1)
# summary(M2)
# anova(M2,M1)
# M3 <- lm(MaMerlot...2 ~ AN + NbJrs30JJ + NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommePluviometrieJJ + NbJrs30JuS + SommeTs10JuS + SommePluviometrieJuS +SommeRayonnementJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS +  Veraison, data=ECSA3)
# summary(M3)
# anova(M3,M2)
# M4 <- lm(MaMerlot...2 ~ AN + NbJrs30JJ + NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommePluviometrieJJ + NbJrs30JuS + SommeTs10JuS +SommeRayonnementJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS +  Veraison, data=ECSA3)
# summary(M4)
# anova(M4,M3)
# M5 <- lm(MaMerlot...2 ~ AN + NbJrs30JJ + NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ NbJrs30JuS + SommeTs10JuS +SommeRayonnementJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS +  Veraison, data=ECSA3)
# summary(M5)
# anova(M5,M4)
# M6 <- lm(MaMerlot...2 ~ AN + NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ NbJrs30JuS + SommeTs10JuS +SommeRayonnementJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS +  Veraison, data=ECSA3)
# summary(M6)
# anova(M6,M5)
# M7 <- lm(MaMerlot...2 ~ AN + NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ NbJrs30JuS + SommeTs10JuS +SommeRayonnementJuS + NbJ25_30JuS + NbJ20_25JuS +  Veraison, data=ECSA3)
# summary(M7)
# anova(M7,M6)
# M8 <- lm(MaMerlot...2 ~ AN + NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ NbJrs30JuS + SommeTs10JuS + NbJ25_30JuS + NbJ20_25JuS +  Veraison, data=ECSA3)
# summary(M8)
# anova(M8,M7)

# MODELE AVEC JUIN SEULEMENT 
Mpre <- lm(MaMerlot...2 ~ NbJrs30JJ + NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ + SommePluviometrieJJ + SommeRayonnementJJ, data=ECSA3)
summary(Mpre)
Y <- predict(Mpre, newdata = ECSA3)
summary(Y)
 
ggplot(ECSA3) + geom_point(aes(x = AN, y=MaMerlot...2)) + geom_point(aes(x=AN, y=Y) , color = 'red') + ggtitle('Maturité du Merlot entre 1960 & 2000 (et prédiction)')+ xlab("Année") + ylab("MaMerlot et prédiction")

##########################################################################
# PREDICTIONS 2001 2007  AVEC JUIN SEULEMENT 
PreJuin <- predict(Mpre, newdata = donnees0107)
PreJuin
ggplot(donnees0107) + geom_point(aes(x = AN, y=MaMerlot)) + geom_point(aes(x=AN, y=PreJuin) , color = 'blue') + ggtitle('Maturité du Merlot et prédiction entre 2001 et 2007')+ xlab("Année") + ylab("MaMerlot et prédiction")

######################
# PREDICTIONS 2001 2007  AVEC M6 

PreM6 <- predict(M6, newdata = donnees0107)
PreM6
ggplot(donnees0107) + geom_point(aes(x = AN, y=MaMerlot)) + geom_point(aes(x=AN, y=PreM6) , color = 'purple') + ggtitle('Maturité du Merlot et prédiction entre 2001 et 2007')+ xlab("Année") + ylab("MaMerlot et prédiction")

################################################
# NOUVEAUX ESSAIS POUR COMPARAISON
Mnew<- lm(MaMerlot...2 ~ NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS, data=ECSA3)
summary(Mnew)
Prenew <- predict(Mnew, newdata = donnees0107)
Prenew
ggplot(donnees0107) + geom_point(aes(x = AN, y=MaMerlot)) + geom_point(aes(x=AN, y=Prenew) , color = 'magenta') + ggtitle('Maturité du Merlot et prédiction entre 2001 et 2007')+ xlab("Année") + ylab("MaMerlot et prédiction")

Mnew2<- lm(MaMerlot...2 ~ NbJ25_30JJ + NbJ20_25JJ + NbJ20_25JuS + NbJ15_20JuS, data=ECSA3)
summary(Mnew2)
Prenew2 <- predict(Mnew2, newdata = donnees0107)
Prenew2
ggplot(donnees0107) + geom_point(aes(x = AN, y=MaMerlot)) + geom_point(aes(x=AN, y=Prenew2) , color = 'orange') + ggtitle('Maturité du Merlot et prédiction entre 2001 et 2007')+ xlab("Année") + ylab("MaMerlot et prédiction")

#############################

M9 <- lm(MaMerlot...2 ~  NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ NbJrs30JuS + SommeTs10JuS + NbJ25_30JuS + NbJ20_25JuS , data=ECSA3)
summary(M9)
predict(M9, newdata = X20012007)

M10 <- lm(MaMerlot...2 ~  NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ NbJ25_30JuS + NbJ20_25JuS , data=ECSA3)
summary(M10)
predict(M10, newdata = X20012007)

########################
# M1 <- lm(MaMerlot...2 ~ AN + NbJrs30JJ + NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ + SommePluviometrieJJ + SommeRayonnementJJ + NbJrs30JuS + SommeTs10JuS + SommePluviometrieJuS +SommeRayonnementJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS + NbJ10_15JuS + Veraison, data=ECSA3)
# summary(M1)
# M2 <-  lm(MaMerlot...2 ~ NbJrs30JJ + NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ + SommePluviometrieJJ + SommeRayonnementJJ + NbJrs30JuS + SommeTs10JuS + SommePluviometrieJuS +SommeRayonnementJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS + Veraison , data=ECSA3)
# summary(M2)
# M3 <-  lm(MaMerlot...2 ~  NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ + SommePluviometrieJJ + SommeRayonnementJJ + NbJrs30JuS + SommeTs10JuS + SommePluviometrieJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS + Veraison , data=ECSA3)
# summary(M3)
# M4 <-  lm(MaMerlot...2 ~  NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ + SommePluviometrieJJ + SommeRayonnementJJ + NbJrs30JuS + SommeTs10JuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS + Veraison , data=ECSA3)
# summary(M4)
# M5 <-  lm(MaMerlot...2 ~  NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ  + SommeRayonnementJJ + NbJrs30JuS + SommeTs10JuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS + Veraison , data=ECSA3)
# summary(M5)
# M6 <-  lm(MaMerlot...2 ~  NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ  + SommeRayonnementJJ + NbJrs30JuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS + Veraison , data=ECSA3)
# summary(M6)
# 
# 
# 
# M6 <-  lm(MaMerlot...2 ~  NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ +SommePluviometrieJJ + SommeRayonnementJJ +SommePluviometrieJuS+ NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS , data=ECSA3)
# summary(M6)
# predict(M6, newdata = X20012007)
# M7 <-  lm(MaMerlot...2 ~  NbJ25_30JJ + NbJ20_25JJ + NbJ10_15JJ +SommePluviometrieJuS+ NbJ20_25JuS + NbJ15_20JuS , data=ECSA3)
# summary(M7)
# predict(M7, newdata = X20012007)

#___________________________________________
# PREDICTIONS 
PRE <- predict(M6, newdata = PREDICTIONS)

ggplot(P2) + geom_point(aes(x = AN, y=MaMerlot)) + geom_point(aes(x=AN, y=PRE) , color = 'red') + ggtitle('Maturité du Merlot entre 2001 et 2007 (et prédiction)')+ xlab("Année") + ylab("MaMerlot et prédiction")
 #AUTRES ESSAIS 

M1b <- lm(MaMerlot...2 ~ NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ + SommePluviometrieJJ + SommeRayonnementJJ + SommeTs10JuS + SommePluviometrieJuS +SommeRayonnementJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS, data=ECSA3)
summary(M1b)
predict(M1b, newdata = X20012007)

M2b <- lm(MaMerlot...2 ~ NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ + SommePluviometrieJJ + SommeRayonnementJJ  + SommePluviometrieJuS +SommeRayonnementJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS, data=ECSA3)
summary(M2b)
predict(M2b, newdata = X20012007)

M3b <- lm(MaMerlot...2 ~ NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ + SommePluviometrieJJ + SommeRayonnementJJ  + SommePluviometrieJuS  + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS, data=ECSA3)
summary(M3b)
predict(M3b, newdata = X20012007)
PREc <- predict(M3b, newdata = PREDICTIONS)
Pa <- predict(M3b, newdata = X20012007)


M4b <- lm(MaMerlot...2 ~ NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommeTs10JJ + SommeRayonnementJJ  + SommePluviometrieJuS  + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS, data=ECSA3)
summary(M4b)
PREb <- predict(M4b, newdata = PREDICTIONS)
PAP <- predict(M4b, newdata = X20012007)
ggplot(P2) + geom_point(aes(x = AN, y=MaMerlot)) + geom_point(aes(x=AN, y=PREc) , color = 'red') + ggtitle('Maturité du Merlot entre 2001 et 2007 (et prédiction)')+ xlab("Année") + ylab("MaMerlot et prédiction")
ggplot(ECSA3) + geom_point(aes(x = AN, y=MaMerlot)) + geom_point(aes(x=AN, y=PAP) , color = 'red') + ggtitle('Maturité du Merlot entre 2001 et 2007 (et prédiction)')+ xlab("Année") + ylab("MaMerlot et prédiction")
ggplot(ECSA3) + geom_point(aes(x = AN, y=MaMerlot)) + geom_point(aes(x=AN, y=Pa) , color = 'red') + ggtitle('Maturité du Merlot et prédiction')+ xlab("Année") + ylab("MaMerlot et prédiction") 

ggplot(P2) + geom_point(aes(x = AN, y=MaMerlot)) + geom_point(aes(x=AN, y=PREc) , color = 'red') + ggtitle('Maturité du Merlot entre 2001 et 2007 (et prédiction)')+ xlab("Année") + ylab("MaMerlot et prédiction")

#_________
# ESSAI C 

M1c <- lm(MaMerlot ~ NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ + NbJ10_15JJ+ SommePluviometrieJJ + SommeRayonnementJJ + SommePluviometrieJuS +SommeRayonnementJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS, data=ECSA3)
summary(M1c)
predict(M1c, newdata = X20012007)
M2c <- lm(MaMerlot ~ NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ+ SommePluviometrieJJ + SommeRayonnementJJ + SommePluviometrieJuS +SommeRayonnementJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS, data=ECSA3)
summary(M2c)
predict(M2c, newdata = X20012007)
M3c <- lm(MaMerlot ~ NbJ25_30JJ + NbJ20_25JJ + NbJ15_20JJ+ SommePluviometrieJJ + SommeRayonnementJJ + SommePluviometrieJuS +SommeRayonnementJuS + NbJ25_30JuS + NbJ20_25JuS + NbJ15_20JuS, data=ECSA3)
summary(M3c)
predict(M3c, newdata = X20012007)


