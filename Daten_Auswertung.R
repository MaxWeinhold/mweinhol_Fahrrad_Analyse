#SEMINAR ZUR ANALYSE VON FAHRRAD-VERKEHRSDATEN SOSE 2022, JAN WESSEL
#-------------------------------------------------------------------
#Eingereicht von: Max Weinhold--------------------------------------
#Matrikelnummer:505314----------------------------------------------

#Workspace setzen in:
#D:/ComicandSonsProductions/GameJam1/mweinhol_Fahrrad_Analyse/Daten_erzeugt

setwd("D:/ComicandSonsProductions/GameJam1/mweinhol_Fahrrad_Analyse")
getwd()

#Arbeitsseicher bereinigen
rm(list=ls())

#packages

#install.packages("sandwich")
library(sandwich)
#install.packages("fixest")
library(fixest)
library(tidyverse)
library(lubridate)
#install.packages("lmtest")
library(lmtest)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("scales")
library(scales)
#install.packages("purrr")
library(purrr)
#install.packages("dplyr")
library(dplyr)
#install.packages("modelr")
library(modelr)
#install.packages("caret")
library(caret)
#install.packages("sf")
library(sf)
#install.packages("mapview")
library(mapview)
#install.packages("raster")
library(raster)
#install.packages("rgeos")
library(rgeos)

load("datensatz.rdata")
datensatz=datensatz_omit
nrow(datensatz)
names(datensatz)

#WetterPlots
#ggplot(data=datensatz, aes(x=Monat,y=as.numeric(WertT2M)))+geom_point()
#ggplot(data=datensatz, aes(x=as.numeric(WertT2M),y=Zaehlstand))+geom_point()
#ggplot(data=datensatz, aes(x=as.numeric(WertRR),y=Zaehlstand))+geom_point()
#ggplot(data=datensatz, aes(x=as.numeric(WertF),y=Zaehlstand))+geom_point()
#ggplot(data=datensatz, aes(x=as.numeric(WertRF),y=Zaehlstand))+geom_point()
#ggplot(data=datensatz, aes(x=as.numeric(WertSD),y=Zaehlstand))+geom_point()
#ggplot(data=datensatz, aes(x=as.numeric(WertN),y=Zaehlstand))+geom_point()

#Zeit Plots
#ggplot(data=datensatz, aes(x=as.numeric(Stunde),y=Zaehlstand))+geom_point()
#ggplot(data=datensatz, aes(x=as.numeric(Monat),y=Zaehlstand))+geom_point()
#ggplot(data=datensatz, aes(x=as.numeric(Jahr),y=Zaehlstand))+geom_point()
#ggplot(data=datensatz, aes(x=Wochentag,y=Zaehlstand))+geom_point()
#ggplot(data=datensatz, aes(x=Zeit_neu,y=Zaehlstand))+geom_point()
#ggplot(data=datensatz, aes(x=as.factor(Sommer),y=Zaehlstand))+geom_boxplot()
#ggplot(data=datensatz, aes(x=Wochenende,y=Zaehlstand))+geom_boxplot()

#Ferien Plots
#ggplot(data=datensatz, aes(x=Wochenende,y=Zaehlstand))+geom_boxplot()

#Standort Plots
#ggplot(data=datensatz, aes(x=as.factor(FeiertagBW),y=Zaehlstand))+geom_boxplot()
#ggplot(data=datensatz, aes(x=as.factor(FeiertagRP),y=Zaehlstand))+geom_boxplot()
#ggplot(data=datensatz, aes(x=as.factor(SchulferienBW),y=Zaehlstand))+geom_boxplot()
#ggplot(data=datensatz, aes(x=uniMA_dist,y=Zaehlstand))+geom_point()
#ggplot(data=datensatz, aes(x=laengengrad,y=breitengrad))+geom_point()
    
nrow(datensatz)

datensatz = datensatz %>%
	filter(as.numeric(Stunde) %in% c(5:22))

datensatz = datensatz %>%
	mutate(Zaehlstand = ifelse(Zaehlstand == 0,1,Zaehlstand))

datensatz = datensatz %>%
	mutate(Wochenende = ifelse(Wochenende == "Wochenende",1,0))

names(datensatz)

regression_fixest1 = feols(log(Zaehlstand) ~ WertT2M + WertT2M^2 + WertT2M^3 + WertRR + WertRR^2 + WertF + WertF^2 + 
	WertRF + WertRF^2 + WertSD + WertSD^2 + WertN + WertN^2 +
	FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + Sommer + 
	laengengrad + laengengrad^2 + breitengrad + breitengrad^2 + laengengrad*breitengrad + 
	as.numeric(Jahr) + as.numeric(Jahr)^2 +uniMA_dist | Stunde + Wochentag, data=datensatz)

#coeftest(regression_fixest1, vcov = vcovHC(regression_fixest, type="HC0"))
summary(regression_fixest1,se="hetero")
AIC(regression_fixest1)
BIC(regression_fixest1)

regression_fixest2 = feols(log(Zaehlstand) ~ WertT2M + WertT2M^2 + WertT2M^3 + WertRR + WertRR^2 + WertF + WertF^2 + 
	WertRF + WertRF^2 + WertSD + WertSD^2 + WertN + WertN^2 +
	FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + Sommer
 	| Standort + Jahr + Stunde + Wochentag, data=datensatz)

#coeftest(regression_fixes2t, vcov = vcovHC(regression_fixest, type="HC0"))
summary(regression_fixest2,se="hetero")
AIC(regression_fixest2)
BIC(regression_fixest2)

#Cross Validation

# Split the data into training and test set
set.seed(123)
training.samples <- datensatz$Zaehlstand %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- datensatz[training.samples, ]
test.data <- datensatz[-training.samples, ]

# Build the model
model <- lm(Zaehlstand ~., data = train.data)
model1 <- feols(log(Zaehlstand) ~ WertT2M + WertT2M^2 + WertT2M^3 + WertRR + WertRR^2 + WertF + WertF^2 + 
	WertRF + WertRF^2 + WertSD + WertSD^2 + WertN + WertN^2 +
	FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + Sommer + 
	laengengrad + laengengrad^2 + breitengrad + breitengrad^2 + laengengrad*breitengrad + 
	as.numeric(Jahr) + as.numeric(Jahr)^2 +uniMA_dist | Stunde + Wochentag, data = train.data)
model2 <- feols(log(Zaehlstand) ~ WertT2M + WertT2M^2 + WertT2M^3 + WertRR + WertRR^2 + WertF + WertF^2 + 
	WertRF + WertRF^2 + WertSD + WertSD^2 + WertN + WertN^2 +
	FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + Sommer + 
	Standort + Jahr + Stunde + Wochentag, data = train.data)


# Make predictions and compute the R2, RMSE and MAE

predictions_train <- model %>% predict(train.data)
data.frame( R2 = R2(predictions_train, train.data$Zaehlstand),
            RMSE = RMSE(predictions_train, train.data$Zaehlstand),
            MAE = MAE(predictions_train, train.data$Zaehlstand))

predictions_test <- model %>% predict(test.data)
data.frame( R2 = R2(predictions_test, test.data$Zaehlstand),
            RMSE = RMSE(predictions_test, test.data$Zaehlstand),
            MAE = MAE(predictions_test, test.data$Zaehlstand))

predictions_train <- model1 %>% predict(train.data)
data.frame( R2 = R2(predictions_train, train.data$Zaehlstand),
            RMSE = RMSE(predictions_train, train.data$Zaehlstand),
            MAE = MAE(predictions_train, train.data$Zaehlstand))

predictions_test <- model1 %>% predict(test.data)
data.frame( R2 = R2(predictions_test, test.data$Zaehlstand),
            RMSE = RMSE(predictions_test, test.data$Zaehlstand),
            MAE = MAE(predictions_test, test.data$Zaehlstand))

predictions_train <- model2 %>% predict(train.data)
data.frame( R2 = R2(predictions_train, train.data$Zaehlstand),
            RMSE = RMSE(predictions_train, train.data$Zaehlstand),
            MAE = MAE(predictions_train, train.data$Zaehlstand))

predictions_test <- model2 %>% predict(test.data)
data.frame( R2 = R2(predictions_test, test.data$Zaehlstand),
            RMSE = RMSE(predictions_test, test.data$Zaehlstand),
            MAE = MAE(predictions_test, test.data$Zaehlstand))












datensatz$WertT2M2 = datensatz$WertT2M^2
datensatz$WertRR2 = datensatz$WertRR^2
datensatz$WertF2 = datensatz$WertF^2
datensatz$WertRF2 = datensatz$WertRF^2
datensatz$WertSD2 = datensatz$WertSD^2
datensatz$WertN2 = datensatz$WertN^2
datensatz$Jahr2 = as.numeric(datensatz$Jahr)^2
datensatz$uniMA_dist2 = datensatz$uniMA_dist^2
datensatz$laengengrad2 = datensatz$laengengrad^2
datensatz$breitengrad2 = datensatz$breitengrad^2

datensatz$laengenbreitengrad = datensatz$laengengrad*datensatz$breitengrad
datensatz$FeiertagBWRP = datensatz$FeiertagBW*datensatz$FeiertagRP

datensatz$WertT2M3 = datensatz$WertT2M^3
datensatz$WertRR3 = datensatz$WertRR^3
datensatz$WertF3 = datensatz$WertF^3
datensatz$WertRF3 = datensatz$WertRF^3
datensatz$WertSD3 = datensatz$WertSD^3
datensatz$WertN3 = datensatz$WertN^3
datensatz$Jahr3 = as.numeric(datensatz$Jahr)^3
datensatz$uniMA_dist3 = datensatz$uniMA_dist^3
datensatz$laengengrad3 = datensatz$laengengrad^3
datensatz$breitengrad3 = datensatz$breitengrad^3

names(datensatz)

# Split the data into training and test set
set.seed(123)
training.samples <- datensatz$Zaehlstand %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- datensatz[training.samples, ]
test.data <- datensatz[-training.samples, ]

#Experementiere

model1 <- lm(log(Zaehlstand) ~ WertT2M, data = train.data)

model2 <- lm(log(Zaehlstand) ~ WertT2M + WertRR, data = train.data)

model3 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF, data = train.data)

model4 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF, data = train.data)

model5 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD, data = train.data)

model6 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN , data = train.data)

model7 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN + FeiertagBW + SemesterferionUM, data = train.data)

model8 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN + FeiertagBW + SemesterferionUM + 
	SchulferienBW, data = train.data)

model9 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN + FeiertagBW + SemesterferionUM + 
	SchulferienBW + Sommer, data = train.data)

model10 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN + FeiertagBW + SemesterferionUM + 
	SchulferienBW + Sommer + as.numeric(Jahr), data = train.data)

model11 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN + FeiertagBW + SemesterferionUM + 
	SchulferienBW + Sommer + as.numeric(Jahr) + Stunde, data = train.data)

model12 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN + FeiertagBW + SemesterferionUM + 
	SchulferienBW + Sommer + as.numeric(Jahr) + Stunde + Wochentag, data = train.data)

model13 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN +
	FeiertagBW + SchulferienBW + SemesterferionUM + Sommer + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model14 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN +
	FeiertagBW + SchulferienBW + SemesterferionUM + Sommer + laengengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model15 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN +
	FeiertagBW + SchulferienBW + SemesterferionUM + Sommer + laengengrad + breitengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model16 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN +
	FeiertagBW + FeiertagRP + FeiertagBWRP + SemesterferionUM + SchulferienBW + Sommer + laengengrad + breitengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model17 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M2 + WertRR + WertF + 
	WertRF + WertSD + WertN +
	FeiertagBW + FeiertagRP + FeiertagBWRP + SemesterferionUM + SchulferienBW + Sommer + 
	laengengrad + breitengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model18 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M2 + WertRR + WertRR2 + WertF + 
	WertRF + WertSD + WertN +
	FeiertagBW + FeiertagRP + FeiertagBWRP + SchulferienBW + SemesterferionUM + Sommer + 
	laengengrad + breitengrad + laengenbreitengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model19 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M2 + WertRR + WertRR2 + WertF + WertF2 + 
	WertRF + WertSD + WertN +
	FeiertagBW + FeiertagRP + FeiertagBWRP + SchulferienBW + SemesterferionUM + Sommer + 
	laengengrad + breitengrad + laengenbreitengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model20 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M2 + WertRR + WertRR2 + WertF + WertF2 + 
	WertRF + WertRF2 + WertSD + WertN +
	FeiertagBW + FeiertagRP + FeiertagBWRP + SchulferienBW + SemesterferionUM + Sommer + 
	laengengrad + breitengrad + laengenbreitengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model21 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M2 + WertRR + WertRR2 + WertF + WertF2 + 
	WertRF + WertRF2 + WertSD + WertSD2 + WertN +
	FeiertagBW + FeiertagRP + FeiertagBWRP + SchulferienBW + SemesterferionUM + Sommer + 
	laengengrad + breitengrad + laengenbreitengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model22 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M2 + WertRR + WertRR2 + WertF + WertF2 + 
	WertRF + WertRF2 + WertSD + WertSD2 + WertN + WertN2 +
	FeiertagBW + FeiertagRP + FeiertagBWRP + SchulferienBW + SemesterferionUM + Sommer + 
	laengengrad + breitengrad + laengenbreitengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model23 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M2 + WertRR + WertRR2 + WertF + WertF2 + 
	WertRF + WertRF2 + WertSD + WertSD2 + WertN + WertN2 +
	FeiertagBW + FeiertagRP + FeiertagBWRP + SchulferienBW + SemesterferionUM + Sommer + 
	laengengrad + laengengrad2 + breitengrad + laengenbreitengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model24 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M2 + WertRR + WertRR2 + WertF + WertF2 + 
	WertRF + WertRF2 + WertSD + WertSD2 + WertN + WertN2 +
	FeiertagBW + FeiertagRP + FeiertagBWRP + SchulferienBW + SemesterferionUM + Sommer + 
	laengengrad + laengengrad2 + breitengrad + laengenbreitengrad + 
	as.numeric(Jahr) + uniMA_dist  + Stunde + Wochentag, data = train.data)

model25 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M2 + WertRR + WertRR2 + WertF + WertF2 + 
	WertRF + WertRF2 + WertSD + WertSD2 + WertN + WertN2 +
	FeiertagBW + FeiertagRP + FeiertagBWRP + SchulferienBW + SemesterferionUM + Sommer + 
	laengengrad + laengengrad2 + breitengrad + laengenbreitengrad + 
	as.numeric(Jahr) + as.numeric(Jahr2) + uniMA_dist + Stunde + Wochentag, data = train.data)

model26 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M2 + WertRR + WertRR2 + WertF + WertF2 + 
	WertRF + WertRF2 + WertSD + WertSD2 + WertN + WertN2 +
	FeiertagBW + FeiertagRP + FeiertagBWRP + SchulferienBW + SemesterferionUM + Sommer + 
	laengengrad + laengengrad2 + breitengrad + laengenbreitengrad + 
	as.numeric(Jahr) + as.numeric(Jahr2) + uniMA_dist+ Stunde + Wochentag, data = train.data)

model27 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M2 + WertT2M3 + WertRR + WertRR2 + WertRR3 + 
	WertF + WertF2 + WertF3 + WertRF + WertRF2 + WertRF3 + WertSD + WertSD2 + WertSD3 + 
	WertN + WertN2 + WertN3 + FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + SemesterferionUM + Sommer + 
	laengengrad + laengengrad2 + laengengrad3 + breitengrad + breitengrad3 + laengenbreitengrad + 
	as.numeric(Jahr) + as.numeric(Jahr2) + as.numeric(Jahr3) + 
	uniMA_dist + Stunde + Wochentag, data = train.data)

model28 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M2 + WertT2M3 + WertRR + WertRR2 + WertRR3 + 
	WertF + WertF2 + WertF3 + WertRF + WertRF2 + WertRF3 + WertSD + WertSD2 + WertSD3 + 
	WertN + WertN2 + WertN3 + FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + SemesterferionUM + Sommer + 
	laengengrad + laengengrad2 + laengengrad3 + breitengrad + breitengrad3 + laengenbreitengrad + 
	as.numeric(Jahr) + as.numeric(Jahr2) + as.numeric(Jahr3) + 
	uniMA_dist + Stunde + Wochentag + Standort, data = train.data)

model29 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M2 + WertT2M3 + WertRR + WertRR2 + WertRR3 + 
	WertF + WertF2 + WertF3 + WertRF + WertRF2 + WertRF3 + WertSD + WertSD2 + WertSD3 + 
	WertN + WertN2 + WertN3 + FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + SemesterferionUM + Sommer + 
	laengengrad + laengengrad2 + laengengrad3 + breitengrad + breitengrad3 + laengenbreitengrad + 
	as.numeric(Jahr) + as.numeric(Jahr2) + as.numeric(Jahr3) + 
	uniMA_dist + Stunde + Wochentag + Standort + QualitaetRR + QualitaetT2M +
	QualitaetF + QualitaetSD + QualitaetN, data = train.data)

model30 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M2 + WertT2M3 + WertRR + WertRR2 + WertRR3 + 
	WertF + WertF2 + WertF3 + WertRF + WertRF2 + WertRF3 + WertSD + WertSD2 + WertSD3 + 
	WertN + WertN2 + WertN3 + FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + SemesterferionUM + Sommer + 
	laengengrad + laengengrad2 + laengengrad3 + breitengrad + breitengrad3 + laengenbreitengrad + 
	as.numeric(Jahr) + as.numeric(Jahr2) + as.numeric(Jahr3) + 
	uniMA_dist + Stunde + Wochentag + Standort + QualitaetRR + QualitaetT2M +
	QualitaetF + QualitaetSD + QualitaetN + uniMA_dist2, data = train.data)

summary(model25)
summary(model26)
summary(model27)

models=c(1:30)
variables=c(1:30)
variables[1]=length(model1$coefficients)
variables[2]=length(model2$coefficients)
variables[3]=length(model3$coefficients)
variables[4]=length(model4$coefficients)
variables[5]=length(model5$coefficients)
variables[6]=length(model6$coefficients)
variables[7]=length(model7$coefficients)
variables[8]=length(model8$coefficients)
variables[9]=length(model9$coefficients)
variables[10]=length(model10$coefficients)
variables[11]=length(model11$coefficients)
variables[12]=length(model12$coefficients)
variables[13]=length(model13$coefficients)
variables[14]=length(model14$coefficients)
variables[15]=length(model15$coefficients)
variables[16]=length(model16$coefficients)
variables[17]=length(model17$coefficients)
variables[18]=length(model18$coefficients)
variables[19]=length(model19$coefficients)
variables[20]=length(model20$coefficients)
variables[21]=length(model21$coefficients)
variables[22]=length(model22$coefficients)
variables[23]=length(model23$coefficients)
variables[24]=length(model24$coefficients)
variables[25]=length(model25$coefficients)
variables[26]=length(model26$coefficients)
variables[27]=length(model27$coefficients)
variables[28]=length(model28$coefficients)
variables[29]=length(model29$coefficients)
variables[30]=length(model30$coefficients)


predictions1 <- model1 %>% predict(train.data)
predictions2 <- model2 %>% predict(train.data)
predictions3 <- model3 %>% predict(train.data)
predictions4 <- model4 %>% predict(train.data)
predictions5 <- model5 %>% predict(train.data)
predictions6 <- model6 %>% predict(train.data)
predictions7 <- model7 %>% predict(train.data)
predictions8 <- model8 %>% predict(train.data)
predictions9 <- model9 %>% predict(train.data)
predictions10 <- model10 %>% predict(train.data)
predictions11 <- model11 %>% predict(train.data)
predictions12 <- model12 %>% predict(train.data)
predictions13 <- model13 %>% predict(train.data)
predictions14 <- model14 %>% predict(train.data)
predictions15 <- model15 %>% predict(train.data)
predictions16 <- model16 %>% predict(train.data)
predictions17 <- model17 %>% predict(train.data)
predictions18 <- model18 %>% predict(train.data)
predictions19 <- model19 %>% predict(train.data)
predictions20 <- model20 %>% predict(train.data)
predictions21 <- model21 %>% predict(train.data)
predictions22 <- model22 %>% predict(train.data)
predictions23 <- model23 %>% predict(train.data)
predictions24 <- model24 %>% predict(train.data)
predictions25 <- model25 %>% predict(train.data)
predictions26 <- model26 %>% predict(train.data)
predictions27 <- model27 %>% predict(train.data)
predictions28 <- model28 %>% predict(train.data)
predictions29 <- model29 %>% predict(train.data)
predictions30 <- model30 %>% predict(train.data)

adjR2_train[1]=R2(predictions1, train.data$Zaehlstand)
adjR2_train[2]=R2(predictions2, train.data$Zaehlstand)
adjR2_train[3]=R2(predictions3, train.data$Zaehlstand)
adjR2_train[4]=R2(predictions4, train.data$Zaehlstand)
adjR2_train[5]=R2(predictions5, train.data$Zaehlstand)
adjR2_train[6]=R2(predictions6, train.data$Zaehlstand)
adjR2_train[7]=R2(predictions7, train.data$Zaehlstand)
adjR2_train[8]=R2(predictions8, train.data$Zaehlstand)
adjR2_train[9]=R2(predictions9, train.data$Zaehlstand)
adjR2_train[10]=R2(predictions10, train.data$Zaehlstand)
adjR2_train[11]=R2(predictions11, train.data$Zaehlstand)
adjR2_train[12]=R2(predictions12, train.data$Zaehlstand)
adjR2_train[13]=R2(predictions13, train.data$Zaehlstand)
adjR2_train[14]=R2(predictions14, train.data$Zaehlstand)
adjR2_train[15]=R2(predictions15, train.data$Zaehlstand)
adjR2_train[16]=R2(predictions16, train.data$Zaehlstand)
adjR2_train[17]=R2(predictions17, train.data$Zaehlstand)
adjR2_train[18]=R2(predictions18, train.data$Zaehlstand)
adjR2_train[19]=R2(predictions19, train.data$Zaehlstand)
adjR2_train[20]=R2(predictions20, train.data$Zaehlstand)
adjR2_train[21]=R2(predictions21, train.data$Zaehlstand)
adjR2_train[22]=R2(predictions22, train.data$Zaehlstand)
adjR2_train[23]=R2(predictions23, train.data$Zaehlstand)
adjR2_train[24]=R2(predictions24, train.data$Zaehlstand)
adjR2_train[25]=R2(predictions25, train.data$Zaehlstand)
adjR2_train[26]=R2(predictions26, train.data$Zaehlstand)
adjR2_train[27]=R2(predictions27, train.data$Zaehlstand)
adjR2_train[28]=R2(predictions28, train.data$Zaehlstand)
adjR2_train[29]=R2(predictions29, train.data$Zaehlstand)
adjR2_train[30]=R2(predictions30, train.data$Zaehlstand)

adjR2_train=c(1:30)
adjR2_train[1]=summary(model1)$adj.r.squared
adjR2_train[2]=summary(model2)$adj.r.squared
adjR2_train[3]=summary(model3)$adj.r.squared
adjR2_train[4]=summary(model4)$adj.r.squared
adjR2_train[5]=summary(model5)$adj.r.squared
adjR2_train[6]=summary(model6)$adj.r.squared
adjR2_train[7]=summary(model7)$adj.r.squared
adjR2_train[8]=summary(model8)$adj.r.squared
adjR2_train[9]=summary(model9)$adj.r.squared
adjR2_train[10]=summary(model10)$adj.r.squared
adjR2_train[11]=summary(model11)$adj.r.squared
adjR2_train[12]=summary(model12)$adj.r.squared
adjR2_train[13]=summary(model13)$adj.r.squared
adjR2_train[14]=summary(model14)$adj.r.squared
adjR2_train[15]=summary(model15)$adj.r.squared
adjR2_train[16]=summary(model16)$adj.r.squared
adjR2_train[17]=summary(model17)$adj.r.squared
adjR2_train[18]=summary(model18)$adj.r.squared
adjR2_train[19]=summary(model19)$adj.r.squared
adjR2_train[20]=summary(model20)$adj.r.squared
adjR2_train[21]=summary(model21)$adj.r.squared
adjR2_train[22]=summary(model22)$adj.r.squared
adjR2_train[23]=summary(model23)$adj.r.squared
adjR2_train[24]=summary(model24)$adj.r.squared
adjR2_train[25]=summary(model25)$adj.r.squared
adjR2_train[26]=summary(model26)$adj.r.squared
adjR2_train[27]=summary(model27)$adj.r.squared
adjR2_train[28]=summary(model28)$adj.r.squared
adjR2_train[29]=summary(model29)$adj.r.squared
adjR2_train[30]=summary(model30)$adj.r.squared

predictions1test <- model1 %>% predict(test.data)
predictions2test <- model2 %>% predict(test.data)
predictions3test <- model3 %>% predict(test.data)
predictions4test <- model4 %>% predict(test.data)
predictions5test <- model5 %>% predict(test.data)
predictions6test <- model6 %>% predict(test.data)
predictions7test <- model7 %>% predict(test.data)
predictions8test <- model8 %>% predict(test.data)
predictions9test <- model9 %>% predict(test.data)
predictions10test <- model10 %>% predict(test.data)
predictions11test <- model11 %>% predict(test.data)
predictions12test <- model12 %>% predict(test.data)
predictions13test <- model13 %>% predict(test.data)
predictions14test <- model14 %>% predict(test.data)
predictions15test <- model15 %>% predict(test.data)
predictions16test <- model16 %>% predict(test.data)
predictions17test <- model17 %>% predict(test.data)
predictions18test <- model18 %>% predict(test.data)
predictions19test <- model19 %>% predict(test.data)
predictions20test <- model20 %>% predict(test.data)
predictions21test <- model21 %>% predict(test.data)
predictions22test <- model22 %>% predict(test.data)
predictions23test <- model23 %>% predict(test.data)
predictions24test <- model24 %>% predict(test.data)
predictions25test <- model25 %>% predict(test.data)
predictions26test <- model26 %>% predict(test.data)
predictions27test <- model27 %>% predict(test.data)
predictions28test <- model28 %>% predict(test.data)
predictions29test <- model29 %>% predict(test.data)
predictions30test <- model30 %>% predict(test.data)

adjR2_test=c(1:30)
adjR2_test[1]=R2(predictions1test, test.data$Zaehlstand)
adjR2_test[2]=R2(predictions2test, test.data$Zaehlstand)
adjR2_test[3]=R2(predictions3test, test.data$Zaehlstand)
adjR2_test[4]=R2(predictions4test, test.data$Zaehlstand)
adjR2_test[5]=R2(predictions5test, test.data$Zaehlstand)
adjR2_test[6]=R2(predictions6test, test.data$Zaehlstand)
adjR2_test[7]=R2(predictions7test, test.data$Zaehlstand)
adjR2_test[8]=R2(predictions8test, test.data$Zaehlstand)
adjR2_test[9]=R2(predictions9test, test.data$Zaehlstand)
adjR2_test[10]=R2(predictions10test, test.data$Zaehlstand)
adjR2_test[11]=R2(predictions11test, test.data$Zaehlstand)
adjR2_test[12]=R2(predictions12test, test.data$Zaehlstand)
adjR2_test[13]=R2(predictions13test, test.data$Zaehlstand)
adjR2_test[14]=R2(predictions14test, test.data$Zaehlstand)
adjR2_test[15]=R2(predictions15test, test.data$Zaehlstand)
adjR2_test[16]=R2(predictions16test, test.data$Zaehlstand)
adjR2_test[17]=R2(predictions17test, test.data$Zaehlstand)
adjR2_test[18]=R2(predictions18test, test.data$Zaehlstand)
adjR2_test[19]=R2(predictions19test, test.data$Zaehlstand)
adjR2_test[20]=R2(predictions20test, test.data$Zaehlstand)
adjR2_test[21]=R2(predictions21test, test.data$Zaehlstand)
adjR2_test[22]=R2(predictions22test, test.data$Zaehlstand)
adjR2_test[23]=R2(predictions23test, test.data$Zaehlstand)
adjR2_test[24]=R2(predictions24test, test.data$Zaehlstand)
adjR2_test[25]=R2(predictions25test, test.data$Zaehlstand)
adjR2_test[26]=R2(predictions26test, test.data$Zaehlstand)
adjR2_test[27]=R2(predictions27test, test.data$Zaehlstand)
adjR2_test[28]=R2(predictions28test, test.data$Zaehlstand)
adjR2_test[29]=R2(predictions29test, test.data$Zaehlstand)
adjR2_test[30]=R2(predictions30test, test.data$Zaehlstand)

ggplot() +
	geom_point(aes(x = variables, y = adjR2_train, color = "Train")) +
	geom_line(aes(x = variables, y = adjR2_train, color = "Train")) +
	geom_point(aes(x = variables, y = adjR2_test, color = "Test")) +
	geom_line(aes(x = variables, y = adjR2_test, color = "Test")) +
  	scale_color_manual(values = c("blue4", "darkorange1")) +
  	theme_classic() + ggtitle("Model Selektion") +
  	xlab("Anzahl der Koeffizienten") + ylab("Adjusted R^2")

RMSE(predictions1, train.data$Zaehlstand)
sqrt(mean(model1$residuals^2))

RMSE_train=c(1:30)
RMSE_train[1]=sqrt(mean(model1$residuals^2))
RMSE_train[2]=sqrt(mean(model2$residuals^2))
RMSE_train[3]=sqrt(mean(model3$residuals^2))
RMSE_train[4]=sqrt(mean(model4$residuals^2))
RMSE_train[5]=sqrt(mean(model5$residuals^2))
RMSE_train[6]=sqrt(mean(model6$residuals^2))
RMSE_train[7]=sqrt(mean(model7$residuals^2))
RMSE_train[8]=sqrt(mean(model8$residuals^2))
RMSE_train[9]=sqrt(mean(model9$residuals^2))
RMSE_train[10]=sqrt(mean(model10$residuals^2))
RMSE_train[11]=sqrt(mean(model11$residuals^2))
RMSE_train[12]=sqrt(mean(model12$residuals^2))
RMSE_train[13]=sqrt(mean(model13$residuals^2))
RMSE_train[14]=sqrt(mean(model14$residuals^2))
RMSE_train[15]=sqrt(mean(model15$residuals^2))
RMSE_train[16]=sqrt(mean(model16$residuals^2))
RMSE_train[17]=sqrt(mean(model17$residuals^2))
RMSE_train[18]=sqrt(mean(model18$residuals^2))
RMSE_train[19]=sqrt(mean(model19$residuals^2))
RMSE_train[20]=sqrt(mean(model20$residuals^2))
RMSE_train[21]=sqrt(mean(model21$residuals^2))
RMSE_train[22]=sqrt(mean(model22$residuals^2))
RMSE_train[23]=sqrt(mean(model23$residuals^2))
RMSE_train[24]=sqrt(mean(model24$residuals^2))
RMSE_train[25]=sqrt(mean(model25$residuals^2))
RMSE_train[26]=sqrt(mean(model26$residuals^2))
RMSE_train[27]=sqrt(mean(model27$residuals^2))
RMSE_train[28]=sqrt(mean(model28$residuals^2))
RMSE_train[29]=sqrt(mean(model29$residuals^2))
RMSE_train[30]=sqrt(mean(model30$residuals^2))

RMSE_train=c(1:30)
RMSE_train[1]= RMSE(predictions1, train.data$Zaehlstand)
RMSE_train[2]= RMSE(predictions2, train.data$Zaehlstand)
RMSE_train[3]= RMSE(predictions3, train.data$Zaehlstand)
RMSE_train[4]= RMSE(predictions4, train.data$Zaehlstand)
RMSE_train[5]= RMSE(predictions5, train.data$Zaehlstand)
RMSE_train[6]= RMSE(predictions6, train.data$Zaehlstand)
RMSE_train[7]= RMSE(predictions7, train.data$Zaehlstand)
RMSE_train[8]= RMSE(predictions8, train.data$Zaehlstand)
RMSE_train[9]= RMSE(predictions9, train.data$Zaehlstand)
RMSE_train[10]= RMSE(predictions10, train.data$Zaehlstand)
RMSE_train[11]= RMSE(predictions11, train.data$Zaehlstand)
RMSE_train[12]= RMSE(predictions12, train.data$Zaehlstand)
RMSE_train[13]= RMSE(predictions13, train.data$Zaehlstand)
RMSE_train[14]= RMSE(predictions14, train.data$Zaehlstand)
RMSE_train[15]= RMSE(predictions15, train.data$Zaehlstand)
RMSE_train[16]= RMSE(predictions16, train.data$Zaehlstand)
RMSE_train[17]= RMSE(predictions17, train.data$Zaehlstand)
RMSE_train[18]= RMSE(predictions18, train.data$Zaehlstand)
RMSE_train[19]= RMSE(predictions19, train.data$Zaehlstand)
RMSE_train[20]= RMSE(predictions20, train.data$Zaehlstand)
RMSE_train[21]= RMSE(predictions21, train.data$Zaehlstand)
RMSE_train[22]= RMSE(predictions22, train.data$Zaehlstand)
RMSE_train[23]= RMSE(predictions23, train.data$Zaehlstand)
RMSE_train[24]= RMSE(predictions24, train.data$Zaehlstand)
RMSE_train[25]= RMSE(predictions25, train.data$Zaehlstand)
RMSE_train[26]= RMSE(predictions26, train.data$Zaehlstand)
RMSE_train[27]= RMSE(predictions27, train.data$Zaehlstand)
RMSE_train[28]= RMSE(predictions28, train.data$Zaehlstand)
RMSE_train[29]= RMSE(predictions29, train.data$Zaehlstand)
RMSE_train[30]= RMSE(predictions30, train.data$Zaehlstand)


RMSE_test=c(1:30)
RMSE_test[1]= RMSE(predictions1test, test.data$Zaehlstand)
RMSE_test[2]= RMSE(predictions2test, test.data$Zaehlstand)
RMSE_test[3]= RMSE(predictions3test, test.data$Zaehlstand)
RMSE_test[4]= RMSE(predictions4test, test.data$Zaehlstand)
RMSE_test[5]= RMSE(predictions5test, test.data$Zaehlstand)
RMSE_test[6]= RMSE(predictions6test, test.data$Zaehlstand)
RMSE_test[7]= RMSE(predictions7test, test.data$Zaehlstand)
RMSE_test[8]= RMSE(predictions8test, test.data$Zaehlstand)
RMSE_test[9]= RMSE(predictions9test, test.data$Zaehlstand)
RMSE_test[10]= RMSE(predictions10test, test.data$Zaehlstand)
RMSE_test[11]= RMSE(predictions11test, test.data$Zaehlstand)
RMSE_test[12]= RMSE(predictions12test, test.data$Zaehlstand)
RMSE_test[13]= RMSE(predictions13test, test.data$Zaehlstand)
RMSE_test[14]= RMSE(predictions14test, test.data$Zaehlstand)
RMSE_test[15]= RMSE(predictions15test, test.data$Zaehlstand)
RMSE_test[16]= RMSE(predictions16test, test.data$Zaehlstand)
RMSE_test[17]= RMSE(predictions17test, test.data$Zaehlstand)
RMSE_test[18]= RMSE(predictions18test, test.data$Zaehlstand)
RMSE_test[19]= RMSE(predictions19test, test.data$Zaehlstand)
RMSE_test[20]= RMSE(predictions20test, test.data$Zaehlstand)
RMSE_test[21]= RMSE(predictions21test, test.data$Zaehlstand)
RMSE_test[22]= RMSE(predictions22test, test.data$Zaehlstand)
RMSE_test[23]= RMSE(predictions23test, test.data$Zaehlstand)
RMSE_test[24]= RMSE(predictions24test, test.data$Zaehlstand)
RMSE_test[25]= RMSE(predictions25test, test.data$Zaehlstand)
RMSE_test[26]= RMSE(predictions26test, test.data$Zaehlstand)
RMSE_test[27]= RMSE(predictions27test, test.data$Zaehlstand)
RMSE_test[28]= RMSE(predictions28test, test.data$Zaehlstand)
RMSE_test[29]= RMSE(predictions29test, test.data$Zaehlstand)
RMSE_test[30]= RMSE(predictions30test, test.data$Zaehlstand)



ggplot() +
	#geom_point(aes(x = variables, y = RMSE_train, color = "Train")) +
	#geom_line(aes(x = variables, y = RMSE_train, color = "Train")) +
	geom_point(aes(x = variables, y = RMSE_test, color = "Test")) +
	geom_line(aes(x = variables, y = RMSE_test, color = "Test")) +
  	scale_color_manual(values = c("blue4", "darkorange1")) +
  	theme_classic() + ggtitle("Model Selektion") +
  	xlab("Anzahl der Koeffizienten") + ylab("RMSE")



#Unser ausgewähltes Model ist:
summary(model25)

ds_test = datensatz %>%
		filter(Jahr=="2018" & Monat == "06" & Tag == "01" & Stunde == "12")


names(datensatz)
#mapview(ds_test, xcol = "laengengrad", ycol = "breitengrad", crs = 4269, grid = FALSE)

#ds_test=datensatz[1:20,]

if (!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if (!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}
if (!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if (!require(sp)){install.packages("sp"); library(sp)}
if (!require(ggmap)){install.packages("ggmap"); library(ggmap)}

myLocation<-c(8.452, 49.472,    8.49, 49.5) # ~Mannheim

#myMap <- get_stamenmap(bbox=myLocation, maptype="watercolor", crop=TRUE)
#ggmap(myMap)

myMap <- get_stamenmap(bbox=myLocation, maptype="toner-lite", zoom=15)
ggmap(myMap)+
geom_point(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=ds_test, size = 5) + 
scale_color_gradient(low = "green", high = "red",limits=c(0,800))




