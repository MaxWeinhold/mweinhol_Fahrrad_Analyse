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














#Leave one out cross validation - LOOCV

# Define training control
train.control <- trainControl(method = "LOOCV")
# Train the model
model <- train(Zaehlstand ~., data = datensatz, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
summary(model)










#Experementiere

model1 <- lm(log(Zaehlstand) ~ WertT2M, data = train.data)

model2 <- lm(log(Zaehlstand) ~ WertT2M + WertRR, data = train.data)

model3 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF, data = train.data)

model4 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF, data = train.data)

model5 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD, data = train.data)

model6 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN , data = train.data)

model7 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN + FeiertagBW, data = train.data)

model8 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN + FeiertagBW + 
	SchulferienBW, data = train.data)

model9 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN + FeiertagBW + 
	SchulferienBW + Sommer, data = train.data)

model10 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN + FeiertagBW + 
	SchulferienBW + Sommer + as.numeric(Jahr), data = train.data)

model11 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN + FeiertagBW + 
	SchulferienBW + Sommer + as.numeric(Jahr) + Stunde, data = train.data)

model12 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN + FeiertagBW + 
	SchulferienBW + Sommer + as.numeric(Jahr) + Stunde + Wochentag, data = train.data)

model13 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN +
	FeiertagBW + SchulferienBW + Sommer + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model14 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN +
	FeiertagBW + SchulferienBW + Sommer + laengengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model15 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN +
	FeiertagBW + SchulferienBW + Sommer + laengengrad + breitengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model16 <- lm(log(Zaehlstand) ~ WertT2M + WertRR + WertF + WertRF + WertSD + WertN +
	FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + Sommer + laengengrad + breitengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model17 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M^2 + WertRR + WertF + 
	WertRF + WertSD + WertN +
	FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + Sommer + 
	laengengrad + breitengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model18 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M^2 + WertRR + WertRR^2 + WertF + 
	WertRF + WertSD + WertN +
	FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + Sommer + 
	laengengrad + breitengrad + laengengrad*breitengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model19 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M^2 + WertRR + WertRR^2 + WertF + WertF^2 + 
	WertRF + WertSD + WertN +
	FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + Sommer + 
	laengengrad + breitengrad + laengengrad*breitengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model20 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M^2 + WertRR + WertRR^2 + WertF + WertF^2 + 
	WertRF + WertRF^2 + WertSD + WertN +
	FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + Sommer + 
	laengengrad + breitengrad + laengengrad*breitengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model21 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M^2 + WertRR + WertRR^2 + WertF + WertF^2 + 
	WertRF + WertRF^2 + WertSD + WertSD^2 + WertN +
	FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + Sommer + 
	laengengrad + breitengrad + laengengrad*breitengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model22 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M^2 + WertRR + WertRR^2 + WertF + WertF^2 + 
	WertRF + WertRF^2 + WertSD + WertSD^2 + WertN + WertN^2 +
	FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + Sommer + 
	laengengrad + breitengrad + laengengrad*breitengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model23 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M^2 + WertRR + WertRR^2 + WertF + WertF^2 + 
	WertRF + WertRF^2 + WertSD + WertSD^2 + WertN + WertN^2 +
	FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + Sommer + 
	laengengrad + laengengrad^2 + breitengrad + laengengrad*breitengrad + 
	as.numeric(Jahr) + uniMA_dist + Stunde + Wochentag, data = train.data)

model24 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M^2 + WertRR + WertRR^2 + WertF + WertF^2 + 
	WertRF + WertRF^2 + WertSD + WertSD^2 + WertN + WertN^2 +
	FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + Sommer + 
	laengengrad + laengengrad^2 + breitengrad + breitengrad^2 + laengengrad*breitengrad + 
	as.numeric(Jahr) + uniMA_dist  + Stunde + Wochentag, data = train.data)

model25 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M^2 + WertRR + WertRR^2 + WertF + WertF^2 + 
	WertRF + WertRF^2 + WertSD + WertSD^2 + WertN + WertN^2 +
	FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + Sommer + 
	laengengrad + laengengrad^2 + breitengrad + breitengrad^2 + laengengrad*breitengrad + 
	as.numeric(Jahr) + as.numeric(Jahr)^2 + uniMA_dist + Stunde + Wochentag, data = train.data)

model26 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M^2 + WertRR + WertRR^2 + WertF + WertF^2 + 
	WertRF + WertRF^2 + WertSD + WertSD^2 + WertN + WertN^2 +
	FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + Sommer + 
	laengengrad + laengengrad^2 + breitengrad + breitengrad^2 + laengengrad*breitengrad + 
	as.numeric(Jahr) + as.numeric(Jahr)^2 + uniMA_dist + uniMA_dist^2 + Stunde + Wochentag, data = train.data)





model27 <- lm(log(Zaehlstand) ~ WertT2M + WertT2M^2 + WertT2M^3 + WertRR + WertRR^2 + WertRR^3 + 
	WertF + WertF^2 + WertF^3 + WertRF + WertRF^2 + WertRF^3 + WertSD + WertSD^2 + WertSD^3 + 
	WertN + WertN^2 + WertN^3 + FeiertagBW + FeiertagRP + FeiertagRP*FeiertagBW + SchulferienBW + Sommer + 
	laengengrad + laengengrad^2 + laengengrad^3 + breitengrad + breitengrad^2 + breitengrad^3 + laengengrad*breitengrad + 
	as.numeric(Jahr) + as.numeric(Jahr)^2 + as.numeric(Jahr)^3 + 
	uniMA_dist + uniMA_dist^2 + uniMA_dist^3 + Stunde + Wochentag, data = train.data)

models=c(1:27)
variables=c(1:27)
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
adjR2_train=c(1:27)
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

adjR2_test=c(1:27)
adjR2_test[1]=R2(predictions1, train.data$Zaehlstand)
adjR2_test[2]=R2(predictions2, train.data$Zaehlstand)
adjR2_test[3]=R2(predictions3, train.data$Zaehlstand)
adjR2_test[4]=R2(predictions4, train.data$Zaehlstand)
adjR2_test[5]=R2(predictions5, train.data$Zaehlstand)
adjR2_test[6]=R2(predictions6, train.data$Zaehlstand)
adjR2_test[7]=R2(predictions7, train.data$Zaehlstand)
adjR2_test[8]=R2(predictions8, train.data$Zaehlstand)
adjR2_test[9]=R2(predictions9, train.data$Zaehlstand)
adjR2_test[10]=R2(predictions10, train.data$Zaehlstand)
adjR2_test[11]=R2(predictions11, train.data$Zaehlstand)
adjR2_test[12]=R2(predictions12, train.data$Zaehlstand)
adjR2_test[13]=R2(predictions13, train.data$Zaehlstand)
adjR2_test[14]=R2(predictions14, train.data$Zaehlstand)
adjR2_test[15]=R2(predictions15, train.data$Zaehlstand)
adjR2_test[16]=R2(predictions16, train.data$Zaehlstand)
adjR2_test[17]=R2(predictions17, train.data$Zaehlstand)
adjR2_test[18]=R2(predictions18, train.data$Zaehlstand)
adjR2_test[19]=R2(predictions19, train.data$Zaehlstand)
adjR2_test[20]=R2(predictions20, train.data$Zaehlstand)
adjR2_test[21]=R2(predictions21, train.data$Zaehlstand)
adjR2_test[22]=R2(predictions22, train.data$Zaehlstand)
adjR2_test[23]=R2(predictions23, train.data$Zaehlstand)
adjR2_test[24]=R2(predictions24, train.data$Zaehlstand)
adjR2_test[25]=R2(predictions25, train.data$Zaehlstand)
adjR2_test[26]=R2(predictions26, train.data$Zaehlstand)
adjR2_test[27]=R2(predictions27, train.data$Zaehlstand)

ggplot() +
	geom_point(aes(x = models, y = adjR2_train, color = "Train")) +
	geom_line(aes(x = models, y = adjR2_train, color = "Train")) +
	geom_point(aes(x = models, y = adjR2_test, color = "Test")) +
	geom_line(aes(x = models, y = adjR2_test, color = "Test")) +
  	scale_color_manual(values = c("blue4", "darkorange1")) +
  	theme_classic()






















