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

library(dplyr)
library(rpart)
library(rpart.plot)

load("datensatz.rdata")
datensatz=datensatz_omit
nrow(datensatz)
names(datensatz)

#Data Set Preparation
 
	nrow(datensatz)

	datensatz = datensatz %>%
		filter(as.numeric(Stunde) %in% c(5:22))

	datensatz = datensatz %>%
		mutate(Zaehlstand = ifelse(Zaehlstand == 0,1,Zaehlstand))

	datensatz = datensatz %>%
		mutate(Wochenende = ifelse(Wochenende == "Wochenende",1,0))

	#Nonlinear effects and intercepts

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

#Niederschlag kategoresieren nach Wessel

	# Wessel (2020)
	datensatz$niederschlag_factor_wes <- NULL
      datensatz$niederschlag_factor_wes <- "Kein Regen"
      datensatz$niederschlag_factor_wes[datensatz$WertRR > 0 & datensatz$WertRR < 0.5] <- "Leichter Nieselregen"
      datensatz$niederschlag_factor_wes[datensatz$WertRR >= 0.5 & datensatz$WertRR < 1] <- "Starker Nieselregen"
      datensatz$niederschlag_factor_wes[datensatz$WertRR >= 1 & datensatz$WertRR < 2] <- "Leichter Regen"
      datensatz$niederschlag_factor_wes[datensatz$WertRR >= 2 & datensatz$WertRR < 5] <- "Moderater Regen"
      datensatz$niederschlag_factor_wes[datensatz$WertRR >= 5 & datensatz$WertRR < 10] <- "Starker Regen"
      datensatz$niederschlag_factor_wes[datensatz$WertRR >= 10] <- "Heftiger Regen"
	datensatz$niederschlag_factor_wes <- factor(datensatz$niederschlag_factor_wes, levels = c("Kein Regen", "Leichter Nieselregen", "Starker Nieselregen", "Leichter Regen", "Moderater Regen", "Starker Regen", "Heftiger Regen"))

#Decision Tree

	names(datensatz)

	tree <- rpart(Zaehlstand~., data=datensatz, cp=.01)
	rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)

	summary(tree)
