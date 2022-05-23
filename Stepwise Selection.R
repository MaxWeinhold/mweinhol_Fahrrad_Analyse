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
#install.packages("olsrr")
library(olsrr)

load("datensatz.rdata")
datensatz=datensatz_omit

datensatz = datensatz %>%
	filter(as.numeric(Stunde) %in% c(5:22))

datensatz = datensatz %>%
	mutate(Zaehlstand = ifelse(Zaehlstand == 0,1,Zaehlstand))

datensatz = datensatz %>%
	mutate(Wochenende = ifelse(Wochenende == "Wochenende",1,0))

nrow(datensatz)

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

model <- lm(Zaehlstand ~ ., data = datensatz)
k <- ols_step_forward_p(model)
plot(k)



