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

load("datensatz.rdata")
datensatz=datensatz_omit
nrow(datensatz)
names(datensatz)

#WetterPlots
ggplot(data=datensatz, aes(x=Monat,y=as.numeric(WertT2M)))+geom_point()
ggplot(data=datensatz, aes(x=as.numeric(WertT2M),y=Zaehlstand))+geom_point()
ggplot(data=datensatz, aes(x=as.numeric(WertRR),y=Zaehlstand))+geom_point()
ggplot(data=datensatz, aes(x=as.numeric(WertF),y=Zaehlstand))+geom_point()
ggplot(data=datensatz, aes(x=as.numeric(WertRF),y=Zaehlstand))+geom_point()
ggplot(data=datensatz, aes(x=as.numeric(WertSD),y=Zaehlstand))+geom_point()
ggplot(data=datensatz, aes(x=as.numeric(WertN),y=Zaehlstand))+geom_point()

#Zeit Plots
ggplot(data=datensatz, aes(x=as.numeric(Stunde),y=Zaehlstand))+geom_point()
ggplot(data=datensatz, aes(x=as.numeric(Monat),y=Zaehlstand))+geom_point()
ggplot(data=datensatz, aes(x=as.numeric(Jahr),y=Zaehlstand))+geom_point()
ggplot(data=datensatz, aes(x=Wochentag,y=Zaehlstand))+geom_point()
ggplot(data=datensatz, aes(x=Zeit_neu,y=Zaehlstand))+geom_point()
ggplot(data=datensatz, aes(x=as.factor(Sommer),y=Zaehlstand))+geom_boxplot()
ggplot(data=datensatz, aes(x=Wochenende,y=Zaehlstand))+geom_boxplot()

#Ferien Plots
ggplot(data=datensatz, aes(x=Wochenende,y=Zaehlstand))+geom_boxplot()

#Standort Plots
ggplot(data=datensatz, aes(x=as.factor(FeiertagBW),y=Zaehlstand))+geom_boxplot()
ggplot(data=datensatz, aes(x=as.factor(FeiertagRP),y=Zaehlstand))+geom_boxplot()
ggplot(data=datensatz, aes(x=as.factor(SchulferienBW),y=Zaehlstand))+geom_boxplot()
    
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
predictions <- model %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$Zaehlstand),
            RMSE = RMSE(predictions, test.data$Zaehlstand),
            MAE = MAE(predictions, test.data$Zaehlstand))

predictions <- model1 %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$Zaehlstand),
            RMSE = RMSE(predictions, test.data$Zaehlstand),
            MAE = MAE(predictions, test.data$Zaehlstand))

predictions <- model2 %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$Zaehlstand),
            RMSE = RMSE(predictions, test.data$Zaehlstand),
            MAE = MAE(predictions, test.data$Zaehlstand))

#Leave one out cross validation - LOOCV

# Define training control
train.control <- trainControl(method = "LOOCV")
# Train the model
model <- train(Zaehlstand ~., data = datensatz, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
