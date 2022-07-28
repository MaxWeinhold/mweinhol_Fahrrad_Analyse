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

#Validation

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


#Nun die Daten zu den Straßenpunkten laden

names(datensatz)

strassen = read_csv("Strassenpunkte.csv")
strassen[100,3]


names(strassen)[2]="breitengrad"
names(strassen)[3]="laengengrad"

strassen$Jahr="2022"
strassen$Monat="06"
strassen$Tag="15"
strassen$Stunde="12"
strassen$Standort=""
strassen$Zaehlstand=1

uniMA_breiteng=49.483397
uniMA_laengeng=8.462194
strassen$uniMA_dist=1
plot(strassen$breitengrad)
library(geosphere)
for(i in 1:length(strassen$uniMA_dist)) {
	strassen$uniMA_dist[i] = distm(c(uniMA_laengeng, uniMA_breiteng), c(strassen$laengengrad[i], strassen$breitengrad[i]), fun = distHaversine)
}


#plot(datensatz$WertSD)
strassen$Wochentag="Do"
strassen$Wochenende=0
strassen$Sommer=1
strassen$FeiertagBW=0
strassen$FeiertagRP=0
strassen$SchulferienBW=1
strassen$SemesterferionUM=0
strassen$WertRR=0
strassen$WertT2M=22
strassen$WertF=2
strassen$WertRF=70
strassen$WertSD=50
strassen$WertN=1

nrow(datensatz)
ncollect =310891

testzeitraum=c(25033,25035,25037,25039,25041,25043,25045,25047,25049,25051,25053,25055,25057,25059,25061,25063)

paste("Fahradfahrer am ", datensatz$Tag[ncollect],".", datensatz$Monat[ncollect],".", datensatz$Jahr[ncollect], " um ",datensatz$Stunde[ncollect], " Uhr", sep="")

strassen$Jahr=datensatz$Jahr[ncollect]
strassen$Monat=datensatz$Monat[ncollect]
strassen$Tag=datensatz$Tag[ncollect]
strassen$Stunde=datensatz$Stunde[ncollect]
strassen$Standort=datensatz$Standort[ncollect]
strassen$Zaehlstand=1
strassen$Wochentag=datensatz$Wochentag[ncollect]
strassen$Wochenende=datensatz$Wochenende[ncollect]
strassen$Sommer=datensatz$Sommer[ncollect]
strassen$FeiertagBW=datensatz$FeiertagBW[ncollect]
strassen$FeiertagRP=datensatz$FeiertagRP[ncollect]
strassen$SchulferienBW=datensatz$SchulferienBW[ncollect]
strassen$SemesterferionUM=datensatz$SemesterferionUM[ncollect]
strassen$WertRR=datensatz$WertRR[ncollect]
strassen$WertT2M=datensatz$WertT2M[ncollect]
strassen$WertF=datensatz$WertF[ncollect]
strassen$WertRF=datensatz$WertRF[ncollect]
strassen$WertSD=datensatz$WertSD[ncollect]
strassen$WertN=datensatz$WertN[ncollect]

strassen$WertT2M2=strassen$WertT2M^2
strassen$WertRR2=strassen$WertRR^2
strassen$WertF2=strassen$WertF^2
strassen$WertRF2=strassen$WertRF^2
strassen$WertSD2=strassen$WertSD^2
strassen$WertN2=strassen$WertN^2
strassen$Jahr2=as.numeric(strassen$Jahr)^2
strassen$laengengrad2=strassen$laengengrad^2
strassen$breitengrad2=strassen$breitengrad^2
strassen$laengenbreitengrad=strassen$laengengrad*strassen$breitengrad
strassen$FeiertagBWRP=strassen$FeiertagBW*strassen$FeiertagRP
strassen$WertT2M3=strassen$WertT2M^3
strassen$WertRR3=strassen$WertRR^3
strassen$WertF3=strassen$WertF^3
strassen$WertT2M3=strassen$WertT2M^3
strassen$WertRR3=strassen$WertRR^3
strassen$WertF3=strassen$WertF^3
strassen$WertRF3=strassen$WertRF^3
strassen$WertRF3=strassen$WertRF^3
strassen$WertN3=strassen$WertN^3
strassen$Jahr3=as.numeric(strassen$Jahr)^3
strassen$laengengrad3=strassen$laengengrad^3
strassen$breitengrad3=strassen$breitengrad^3

strassen
names(strassen)
summary(model25)$coefficients

prediction_fuer_strassen <- model25 %>% predict(strassen)

plot(exp(prediction_fuer_strassen))

summary(datensatz$Zaehlstand)
summary(predictions25test)
summary(exp(predictions25test))
summary(prediction_fuer_strassen)

#https://www.r-bloggers.com/2013/08/forecasting-from-log-linear-regressions/
se <- function(x) sqrt(var(x) / length(x))
se(prediction_fuer_strassen)

strassen$Zaehlstand=exp(prediction_fuer_strassen)

#strassen$Zaehlstand=(exp(prediction_fuer_strassen)+(se(prediction_fuer_strassen)^2/2))

#exp(prediction_fuer_strassen + sigma^2/2)

#strassen$Zaehlstand=prediction_fuer_strassen
summary(strassen$Zaehlstand)

strassen$Zaehlstand=if_else(strassen$Zaehlstand > max(datensatz$Zaehlstand)
	, max(datensatz$Zaehlstand) + (strassen$Zaehlstand-max(datensatz$Zaehlstand))*0.01
	, strassen$Zaehlstand)

strassen$Zaehlstand=if_else(strassen$Zaehlstand > 1200
	, 1200
	, strassen$Zaehlstand)

plot(strassen$Zaehlstand)

names(strassen)
strassenA <- subset(strassen, Punkt == "A")
strassenB <- subset(strassen, Punkt == "B")
strassenA[1,]

strassenAB1=rbind(strassenA[1,],strassenB[1,])
strassenAB2=rbind(strassenA[2,],strassenB[2,])
strassenAB3=rbind(strassenA[3,],strassenB[3,])
strassenAB4=rbind(strassenA[4,],strassenB[4,])
strassenAB5=rbind(strassenA[5,],strassenB[5,])
strassenAB6=rbind(strassenA[6,],strassenB[6,])
strassenAB7=rbind(strassenA[7,],strassenB[7,])
strassenAB8=rbind(strassenA[8,],strassenB[8,])
strassenAB9=rbind(strassenA[9,],strassenB[9,])
strassenAB10=rbind(strassenA[10,],strassenB[10,])
strassenAB11=rbind(strassenA[11,],strassenB[11,])
strassenAB12=rbind(strassenA[12,],strassenB[12,])
strassenAB13=rbind(strassenA[13,],strassenB[13,])
strassenAB14=rbind(strassenA[14,],strassenB[14,])
strassenAB15=rbind(strassenA[15,],strassenB[15,])
strassenAB16=rbind(strassenA[16,],strassenB[16,])
strassenAB17=rbind(strassenA[17,],strassenB[17,])
strassenAB18=rbind(strassenA[18,],strassenB[18,])
strassenAB19=rbind(strassenA[19,],strassenB[19,])
strassenAB20=rbind(strassenA[20,],strassenB[20,])
strassenAB21=rbind(strassenA[21,],strassenB[21,])
strassenAB22=rbind(strassenA[22,],strassenB[22,])
strassenAB23=rbind(strassenA[23,],strassenB[23,])
strassenAB24=rbind(strassenA[24,],strassenB[24,])
strassenAB25=rbind(strassenA[25,],strassenB[25,])
strassenAB26=rbind(strassenA[26,],strassenB[26,])
strassenAB27=rbind(strassenA[27,],strassenB[27,])
strassenAB28=rbind(strassenA[28,],strassenB[28,])
strassenAB29=rbind(strassenA[29,],strassenB[29,])
strassenAB30=rbind(strassenA[30,],strassenB[30,])
strassenAB31=rbind(strassenA[31,],strassenB[31,])
strassenAB32=rbind(strassenA[32,],strassenB[32,])
strassenAB33=rbind(strassenA[33,],strassenB[33,])
strassenAB34=rbind(strassenA[34,],strassenB[34,])
strassenAB35=rbind(strassenA[35,],strassenB[35,])
strassenAB36=rbind(strassenA[36,],strassenB[36,])
strassenAB37=rbind(strassenA[37,],strassenB[37,])
strassenAB38=rbind(strassenA[38,],strassenB[38,])
strassenAB39=rbind(strassenA[39,],strassenB[39,])
strassenAB40=rbind(strassenA[40,],strassenB[40,])
strassenAB41=rbind(strassenA[41,],strassenB[41,])
strassenAB42=rbind(strassenA[42,],strassenB[42,])
strassenAB43=rbind(strassenA[43,],strassenB[43,])
strassenAB44=rbind(strassenA[44,],strassenB[44,])
strassenAB45=rbind(strassenA[45,],strassenB[45,])
strassenAB46=rbind(strassenA[46,],strassenB[46,])
strassenAB47=rbind(strassenA[47,],strassenB[47,])
strassenAB48=rbind(strassenA[48,],strassenB[48,])
strassenAB49=rbind(strassenA[49,],strassenB[49,])
strassenAB50=rbind(strassenA[50,],strassenB[50,])
strassenAB51=rbind(strassenA[51,],strassenB[51,])
strassenAB52=rbind(strassenA[52,],strassenB[52,])
strassenAB53=rbind(strassenA[53,],strassenB[53,])
strassenAB54=rbind(strassenA[54,],strassenB[54,])
strassenAB55=rbind(strassenA[55,],strassenB[55,])
strassenAB56=rbind(strassenA[56,],strassenB[56,])
strassenAB57=rbind(strassenA[57,],strassenB[57,])
strassenAB58=rbind(strassenA[58,],strassenB[58,])
strassenAB59=rbind(strassenA[59,],strassenB[59,])
strassenAB60=rbind(strassenA[60,],strassenB[60,])
strassenAB61=rbind(strassenA[61,],strassenB[61,])
strassenAB62=rbind(strassenA[62,],strassenB[62,])
strassenAB63=rbind(strassenA[63,],strassenB[63,])
strassenAB64=rbind(strassenA[64,],strassenB[64,])
strassenAB65=rbind(strassenA[65,],strassenB[65,])
strassenAB66=rbind(strassenA[66,],strassenB[66,])
strassenAB67=rbind(strassenA[67,],strassenB[67,])
strassenAB68=rbind(strassenA[68,],strassenB[68,])
strassenAB69=rbind(strassenA[69,],strassenB[69,])
strassenAB70=rbind(strassenA[70,],strassenB[70,])
strassenAB71=rbind(strassenA[71,],strassenB[71,])
strassenAB72=rbind(strassenA[72,],strassenB[72,])
strassenAB73=rbind(strassenA[73,],strassenB[73,])
strassenAB74=rbind(strassenA[74,],strassenB[74,])
strassenAB75=rbind(strassenA[75,],strassenB[75,])
strassenAB76=rbind(strassenA[76,],strassenB[76,])
strassenAB77=rbind(strassenA[77,],strassenB[77,])
strassenAB78=rbind(strassenA[78,],strassenB[78,])
strassenAB79=rbind(strassenA[79,],strassenB[79,])
strassenAB80=rbind(strassenA[80,],strassenB[80,])
strassenAB81=rbind(strassenA[81,],strassenB[81,])
strassenAB82=rbind(strassenA[82,],strassenB[82,])
strassenAB83=rbind(strassenA[83,],strassenB[83,])
strassenAB84=rbind(strassenA[84,],strassenB[84,])
strassenAB85=rbind(strassenA[85,],strassenB[85,])
strassenAB86=rbind(strassenA[86,],strassenB[86,])
strassenAB87=rbind(strassenA[87,],strassenB[87,])
strassenAB88=rbind(strassenA[88,],strassenB[88,])
strassenAB89=rbind(strassenA[89,],strassenB[89,])
strassenAB90=rbind(strassenA[90,],strassenB[90,])
strassenAB91=rbind(strassenA[91,],strassenB[91,])
strassenAB92=rbind(strassenA[92,],strassenB[92,])
strassenAB93=rbind(strassenA[93,],strassenB[93,])
strassenAB94=rbind(strassenA[94,],strassenB[94,])
strassenAB95=rbind(strassenA[95,],strassenB[95,])
strassenAB96=rbind(strassenA[96,],strassenB[96,])
strassenAB97=rbind(strassenA[97,],strassenB[97,])
strassenAB98=rbind(strassenA[98,],strassenB[98,])
strassenAB99=rbind(strassenA[99,],strassenB[99,])
strassenAB100=rbind(strassenA[100,],strassenB[100,])
strassenAB101=rbind(strassenA[101,],strassenB[101,])
strassenAB102=rbind(strassenA[102,],strassenB[102,])
strassenAB103=rbind(strassenA[103,],strassenB[103,])
strassenAB104=rbind(strassenA[104,],strassenB[104,])
strassenAB105=rbind(strassenA[105,],strassenB[105,])
strassenAB106=rbind(strassenA[106,],strassenB[106,])
strassenAB107=rbind(strassenA[107,],strassenB[107,])
strassenAB108=rbind(strassenA[108,],strassenB[108,])
strassenAB109=rbind(strassenA[109,],strassenB[109,])
strassenAB110=rbind(strassenA[110,],strassenB[110,])
strassenAB111=rbind(strassenA[111,],strassenB[111,])
strassenAB112=rbind(strassenA[112,],strassenB[112,])
strassenAB113=rbind(strassenA[113,],strassenB[113,])
strassenAB114=rbind(strassenA[114,],strassenB[114,])
strassenAB115=rbind(strassenA[115,],strassenB[115,])
strassenAB116=rbind(strassenA[116,],strassenB[116,])
strassenAB117=rbind(strassenA[117,],strassenB[117,])
strassenAB118=rbind(strassenA[118,],strassenB[118,])





strassenAB=c(strassenAB1,strassenAB2)
class(strassenAB)

strassenAB[2]

for(i in 1:nrow(strassenA)) {
	strassenAB[i]=rbind(strassenA[i,],strassenB[i,])
}
warnings()



names(datensatz)
ncollect
monat=datensatz$Monat[ncollect]
tag=datensatz$Tag[ncollect]
zeit=datensatz$Stunde[ncollect]
jahr=datensatz$Jahr[ncollect]
d = subset(datensatz, Monat == monat & Tag == tag & Stunde == zeit & Jahr == jahr)


mid <- median(strassen$Zaehlstand)
mid = min(strassen$Zaehlstand) +(max(strassen$Zaehlstand)-min(strassen$Zaehlstand))/2
myMap <- get_stamenmap(bbox=myLocation, maptype="toner-lite", zoom=15)
ggmap(myMap)+
ggtitle(paste("Fahradfahrer am ", strassen$Tag[1],".", strassen$Monat[1],".", strassen$Jahr[1], " um ",strassen$Stunde[1], " Uhr", sep="")) +
#geom_point(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassen, size = 2) + 
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB1, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB2, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB3, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB4, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB5, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB6, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB7, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB8, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB9, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB10, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB11, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB12, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB13, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB14, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB15, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB16, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB17, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB18, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB19, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB20, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB21, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB22, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB23, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB24, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB25, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB26, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB27, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB28, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB29, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB30, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB31, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB32, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB33, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB34, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB35, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB36, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB37, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB38, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB39, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB40, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB41, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB42, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB43, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB44, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB45, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB46, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB47, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB48, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB49, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB50, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB51, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB52, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB53, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB54, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB55, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB56, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB57, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB58, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB59, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB60, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB61, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB62, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB63, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB64, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB65, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB66, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB67, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB68, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB69, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB70, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB71, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB72, size = 2) +
#geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB73, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB74, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB75, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB76, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB77, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB78, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB79, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB80, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB81, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB82, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB83, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB84, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB85, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB86, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB87, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB88, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB89, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB90, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB91, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB92, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB93, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB94, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB95, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB96, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB97, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB98, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB99, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB100, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB101, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB102, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB103, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB104, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB105, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB106, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB107, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB108, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB109, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB110, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB111, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB112, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB113, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB114, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB115, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB116, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB117, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB118, size = 2) +
geom_point(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=d, size = 6) + 
 scale_color_gradient2(midpoint = mid, low = "green", mid = "red",
                            high = "blue", limits = c(0, 1200), space = "Lab" )



for(i in 1:length(strassen$uniMA_dist)) {
	strassen$uniMA_dist[i] = distm(c(uniMA_laengeng, uniMA_breiteng), c(strassen$laengengrad[i], strassen$breitengrad[i]), fun = distHaversine)
}

testzeitraum
for(i in 1:length(testzeitraum)) {

ncollect=testzeitraum[i]
strassen$Jahr=datensatz$Jahr[ncollect]
strassen$Monat=datensatz$Monat[ncollect]
strassen$Tag=datensatz$Tag[ncollect]
strassen$Stunde=datensatz$Stunde[ncollect]
strassen$Standort=datensatz$Standort[ncollect]
strassen$Zaehlstand=1
strassen$Wochentag=datensatz$Wochentag[ncollect]
strassen$Wochenende=datensatz$Wochenende[ncollect]
strassen$Sommer=datensatz$Sommer[ncollect]
strassen$FeiertagBW=datensatz$FeiertagBW[ncollect]
strassen$FeiertagRP=datensatz$FeiertagRP[ncollect]
strassen$SchulferienBW=datensatz$SchulferienBW[ncollect]
strassen$SemesterferionUM=datensatz$SemesterferionUM[ncollect]
strassen$WertRR=datensatz$WertRR[ncollect]
strassen$WertT2M=datensatz$WertT2M[ncollect]
strassen$WertF=datensatz$WertF[ncollect]
strassen$WertRF=datensatz$WertRF[ncollect]
strassen$WertSD=datensatz$WertSD[ncollect]
strassen$WertN=datensatz$WertN[ncollect]

strassen$WertT2M2=strassen$WertT2M^2
strassen$WertRR2=strassen$WertRR^2
strassen$WertF2=strassen$WertF^2
strassen$WertRF2=strassen$WertRF^2
strassen$WertSD2=strassen$WertSD^2
strassen$WertN2=strassen$WertN^2
strassen$Jahr2=as.numeric(strassen$Jahr)^2
strassen$laengengrad2=strassen$laengengrad^2
strassen$breitengrad2=strassen$breitengrad^2
strassen$laengenbreitengrad=strassen$laengengrad*strassen$breitengrad
strassen$FeiertagBWRP=strassen$FeiertagBW*strassen$FeiertagRP
strassen$WertT2M3=strassen$WertT2M^3
strassen$WertRR3=strassen$WertRR^3
strassen$WertF3=strassen$WertF^3
strassen$WertT2M3=strassen$WertT2M^3
strassen$WertRR3=strassen$WertRR^3
strassen$WertF3=strassen$WertF^3
strassen$WertRF3=strassen$WertRF^3
strassen$WertRF3=strassen$WertRF^3
strassen$WertN3=strassen$WertN^3
strassen$Jahr3=as.numeric(strassen$Jahr)^3
strassen$laengengrad3=strassen$laengengrad^3
strassen$breitengrad3=strassen$breitengrad^3

prediction_fuer_strassen <- model25 %>% predict(strassen)
strassen$Zaehlstand=exp(prediction_fuer_strassen)

#strassen$Zaehlstand=if_else(strassen$Zaehlstand > max(datensatz$Zaehlstand)
#	, max(datensatz$Zaehlstand) + (strassen$Zaehlstand-max(datensatz$Zaehlstand))*0.01
#	, strassen$Zaehlstand)

#strassen$Zaehlstand=if_else(strassen$Zaehlstand > 1200
#	, 1200
#	, strassen$Zaehlstand)

strassen$Zaehlstand=if_else(strassen$Zaehlstand < 0
	, 0
	, strassen$Zaehlstand)

strassenA <- subset(strassen, Punkt == "A")
strassenB <- subset(strassen, Punkt == "B")
strassenAB1=rbind(strassenA[1,],strassenB[1,])
strassenAB2=rbind(strassenA[2,],strassenB[2,])
strassenAB3=rbind(strassenA[3,],strassenB[3,])
strassenAB4=rbind(strassenA[4,],strassenB[4,])
strassenAB5=rbind(strassenA[5,],strassenB[5,])
strassenAB6=rbind(strassenA[6,],strassenB[6,])
strassenAB7=rbind(strassenA[7,],strassenB[7,])
strassenAB8=rbind(strassenA[8,],strassenB[8,])
strassenAB9=rbind(strassenA[9,],strassenB[9,])
strassenAB10=rbind(strassenA[10,],strassenB[10,])
strassenAB11=rbind(strassenA[11,],strassenB[11,])
strassenAB12=rbind(strassenA[12,],strassenB[12,])
strassenAB13=rbind(strassenA[13,],strassenB[13,])
strassenAB14=rbind(strassenA[14,],strassenB[14,])
strassenAB15=rbind(strassenA[15,],strassenB[15,])
strassenAB16=rbind(strassenA[16,],strassenB[16,])
strassenAB17=rbind(strassenA[17,],strassenB[17,])
strassenAB18=rbind(strassenA[18,],strassenB[18,])
strassenAB19=rbind(strassenA[19,],strassenB[19,])
strassenAB20=rbind(strassenA[20,],strassenB[20,])
strassenAB21=rbind(strassenA[21,],strassenB[21,])
strassenAB22=rbind(strassenA[22,],strassenB[22,])
strassenAB23=rbind(strassenA[23,],strassenB[23,])
strassenAB24=rbind(strassenA[24,],strassenB[24,])
strassenAB25=rbind(strassenA[25,],strassenB[25,])
strassenAB26=rbind(strassenA[26,],strassenB[26,])
strassenAB27=rbind(strassenA[27,],strassenB[27,])
strassenAB28=rbind(strassenA[28,],strassenB[28,])
strassenAB29=rbind(strassenA[29,],strassenB[29,])
strassenAB30=rbind(strassenA[30,],strassenB[30,])
strassenAB31=rbind(strassenA[31,],strassenB[31,])
strassenAB32=rbind(strassenA[32,],strassenB[32,])
strassenAB33=rbind(strassenA[33,],strassenB[33,])
strassenAB34=rbind(strassenA[34,],strassenB[34,])
strassenAB35=rbind(strassenA[35,],strassenB[35,])
strassenAB36=rbind(strassenA[36,],strassenB[36,])
strassenAB37=rbind(strassenA[37,],strassenB[37,])
strassenAB38=rbind(strassenA[38,],strassenB[38,])
strassenAB39=rbind(strassenA[39,],strassenB[39,])
strassenAB40=rbind(strassenA[40,],strassenB[40,])
strassenAB41=rbind(strassenA[41,],strassenB[41,])
strassenAB42=rbind(strassenA[42,],strassenB[42,])
strassenAB43=rbind(strassenA[43,],strassenB[43,])
strassenAB44=rbind(strassenA[44,],strassenB[44,])
strassenAB45=rbind(strassenA[45,],strassenB[45,])
strassenAB46=rbind(strassenA[46,],strassenB[46,])
strassenAB47=rbind(strassenA[47,],strassenB[47,])
strassenAB48=rbind(strassenA[48,],strassenB[48,])
strassenAB49=rbind(strassenA[49,],strassenB[49,])
strassenAB50=rbind(strassenA[50,],strassenB[50,])
strassenAB51=rbind(strassenA[51,],strassenB[51,])
strassenAB52=rbind(strassenA[52,],strassenB[52,])
strassenAB53=rbind(strassenA[53,],strassenB[53,])
strassenAB54=rbind(strassenA[54,],strassenB[54,])
strassenAB55=rbind(strassenA[55,],strassenB[55,])
strassenAB56=rbind(strassenA[56,],strassenB[56,])
strassenAB57=rbind(strassenA[57,],strassenB[57,])
strassenAB58=rbind(strassenA[58,],strassenB[58,])
strassenAB59=rbind(strassenA[59,],strassenB[59,])
strassenAB60=rbind(strassenA[60,],strassenB[60,])
strassenAB61=rbind(strassenA[61,],strassenB[61,])
strassenAB62=rbind(strassenA[62,],strassenB[62,])
strassenAB63=rbind(strassenA[63,],strassenB[63,])
strassenAB64=rbind(strassenA[64,],strassenB[64,])
strassenAB65=rbind(strassenA[65,],strassenB[65,])
strassenAB66=rbind(strassenA[66,],strassenB[66,])
strassenAB67=rbind(strassenA[67,],strassenB[67,])
strassenAB68=rbind(strassenA[68,],strassenB[68,])
strassenAB69=rbind(strassenA[69,],strassenB[69,])
strassenAB70=rbind(strassenA[70,],strassenB[70,])
strassenAB71=rbind(strassenA[71,],strassenB[71,])
strassenAB72=rbind(strassenA[72,],strassenB[72,])
strassenAB73=rbind(strassenA[73,],strassenB[73,])
strassenAB74=rbind(strassenA[74,],strassenB[74,])
strassenAB75=rbind(strassenA[75,],strassenB[75,])
strassenAB76=rbind(strassenA[76,],strassenB[76,])
strassenAB77=rbind(strassenA[77,],strassenB[77,])
strassenAB78=rbind(strassenA[78,],strassenB[78,])
strassenAB79=rbind(strassenA[79,],strassenB[79,])
strassenAB80=rbind(strassenA[80,],strassenB[80,])
strassenAB81=rbind(strassenA[81,],strassenB[81,])
strassenAB82=rbind(strassenA[82,],strassenB[82,])
strassenAB83=rbind(strassenA[83,],strassenB[83,])
strassenAB84=rbind(strassenA[84,],strassenB[84,])
strassenAB85=rbind(strassenA[85,],strassenB[85,])
strassenAB86=rbind(strassenA[86,],strassenB[86,])
strassenAB87=rbind(strassenA[87,],strassenB[87,])
strassenAB88=rbind(strassenA[88,],strassenB[88,])
strassenAB89=rbind(strassenA[89,],strassenB[89,])
strassenAB90=rbind(strassenA[90,],strassenB[90,])
strassenAB91=rbind(strassenA[91,],strassenB[91,])
strassenAB92=rbind(strassenA[92,],strassenB[92,])
strassenAB93=rbind(strassenA[93,],strassenB[93,])
strassenAB94=rbind(strassenA[94,],strassenB[94,])
strassenAB95=rbind(strassenA[95,],strassenB[95,])
strassenAB96=rbind(strassenA[96,],strassenB[96,])
strassenAB97=rbind(strassenA[97,],strassenB[97,])
strassenAB98=rbind(strassenA[98,],strassenB[98,])
strassenAB99=rbind(strassenA[99,],strassenB[99,])
strassenAB100=rbind(strassenA[100,],strassenB[100,])
strassenAB101=rbind(strassenA[101,],strassenB[101,])
strassenAB102=rbind(strassenA[102,],strassenB[102,])
strassenAB103=rbind(strassenA[103,],strassenB[103,])
strassenAB104=rbind(strassenA[104,],strassenB[104,])
strassenAB105=rbind(strassenA[105,],strassenB[105,])
strassenAB106=rbind(strassenA[106,],strassenB[106,])
strassenAB107=rbind(strassenA[107,],strassenB[107,])
strassenAB108=rbind(strassenA[108,],strassenB[108,])
strassenAB109=rbind(strassenA[109,],strassenB[109,])
strassenAB110=rbind(strassenA[110,],strassenB[110,])
strassenAB111=rbind(strassenA[111,],strassenB[111,])
strassenAB112=rbind(strassenA[112,],strassenB[112,])
strassenAB113=rbind(strassenA[113,],strassenB[113,])
strassenAB114=rbind(strassenA[114,],strassenB[114,])
strassenAB115=rbind(strassenA[115,],strassenB[115,])
strassenAB116=rbind(strassenA[116,],strassenB[116,])
strassenAB117=rbind(strassenA[117,],strassenB[117,])
strassenAB118=rbind(strassenA[118,],strassenB[118,])

monat=datensatz$Monat[ncollect]
tag=datensatz$Tag[ncollect]
zeit=datensatz$Stunde[ncollect]
jahr=datensatz$Jahr[ncollect]
d = subset(datensatz, Monat == monat & Tag == tag & Stunde == zeit & Jahr == jahr)

mid <- 600
mid = min(strassen$Zaehlstand) +(max(strassen$Zaehlstand)-min(strassen$Zaehlstand))/2
myMap <- get_stamenmap(bbox=myLocation, maptype="toner-lite", zoom=15)
map = ggmap(myMap)+
ggtitle(paste("Fahradfahrer am ", strassen$Tag[1],".", strassen$Monat[1],".", strassen$Jahr[1], " um ",strassen$Stunde[1], " Uhr", sep="")) +
#geom_point(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassen, size = 2) + 
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB1, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB2, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB3, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB4, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB5, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB6, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB7, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB8, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB9, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB10, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB11, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB12, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB13, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB14, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB15, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB16, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB17, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB18, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB19, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB20, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB21, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB22, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB23, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB24, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB25, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB26, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB27, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB28, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB29, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB30, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB31, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB32, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB33, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB34, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB35, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB36, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB37, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB38, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB39, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB40, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB41, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB42, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB43, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB44, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB45, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB46, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB47, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB48, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB49, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB50, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB51, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB52, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB53, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB54, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB55, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB56, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB57, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB58, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB59, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB60, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB61, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB62, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB63, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB64, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB65, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB66, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB67, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB68, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB69, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB70, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB71, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB72, size = 2) +
#geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB73, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB74, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB75, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB76, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB77, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB78, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB79, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB80, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB81, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB82, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB83, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB84, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB85, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB86, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB87, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB88, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB89, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB90, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB91, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB92, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB93, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB94, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB95, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB96, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB97, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB98, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB99, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB100, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB101, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB102, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB103, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB104, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB105, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB106, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB107, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB108, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB109, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB110, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB111, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB112, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB113, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB114, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB115, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB116, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB117, size = 2) +
geom_line(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=strassenAB118, size = 2) +
geom_point(aes(x=laengengrad, y=breitengrad, color = Zaehlstand), data=d, size = 6) + 
 scale_color_gradient2(midpoint = mid, low = "green", mid = "red",
                            high = "blue", limits = c(0, 40000), space = "Lab" )
map
save(map, file = paste("HeatMaps/HeatMap", i, ".Rdata", sep=""))
ggsave(plot = map,filename = paste("HeatMaps/HeatMap", i, ".png", sep=""))
}

