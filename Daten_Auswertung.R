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

library(tidyverse)
library(lubridate)

install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("scales")
library(scales)

load("datensatz.rdata")
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

regression = lm(log(Zaehlstand) ~ WertT2M + WertRR + 
	WertF + WertRF + WertSD + WertN +
	Wochentag + Wochenende + Wochenende + 
	as.factor(FeiertagBW) + as.factor(FeiertagRP) + as.factor(FeiertagBW*FeiertagRP) +
	as.factor(SchulferienBW) + as.factor(Sommer) + Monat + Stunde + Standort, data=datensatz)

regression = lm(log(Zaehlstand) ~ WertT2M + WertRR + 
	WertF + WertRF + WertSD + WertN, data=datensatz)


coeftest(regression_dummy, vcov = vcovHC(regression_dummy, type="HC0"))

summary(regression)











