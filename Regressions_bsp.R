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
library(RColorBrewer)

load("datensatz.rdata")
datensatz
nrow(datensatz)
names(datensatz)

datensatz = datensatz %>%
	filter(as.numeric(Stunde) %in% c(5:22))

datensatz = datensatz %>%
	mutate(Zaehlstand = ifelse(Zaehlstand == 0,1,Zaehlstand))

datensatz = datensatz %>%
	mutate(Wochenende = ifelse(Wochenende == "Wochenende",1,0))

names(datensatz)

plot(c(3,2,5,6,8))
plot(datensatz$WertT2M,datensatz$Zaehlstand)

ggplot(datensatz,aes(x = WertT2M, y = Zaehlstand)) +
	geom_point(size=0.05) +
	ggtitle("Fahrradfahrer und Temperatur") +
	xlab("Temperatur in 2 Meter Höhe in C°") +
	ylab("Fahrradauufkommen") +
	theme_bw() + 
  	stat_smooth(method = "lm", formula= y ~ x, aes(color="linear"), size = 1.5) +
	stat_smooth(method = "lm", formula = y ~ x + I(x^2), aes(color="quadratisch"), size = 1.5) +
	stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), aes(color="kubisch"), size = 1.5) +
	scale_colour_manual(name="legend", values=c("green", "red", "blue"))





