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
library(ggmap)

load("datensatz.rdata")
datensatz=datensatz_omit
nrow(datensatz)
names(datensatz)

datensatz = datensatz %>%
	filter(as.numeric(Stunde) %in% c(5:22))

datensatz = datensatz %>%
	mutate(Zaehlstand = ifelse(Zaehlstand == 0,1,Zaehlstand))

datensatz = datensatz %>%
	mutate(Wochenende = ifelse(Wochenende == "Wochenende",1,0))

names(datensatz)

unique(datensatz$Standort)

boxplot(data=datensatz,
	datensatz$Zaehlstand~datensatz$Standort,
	main="Fahrradaufkommen nach Standort",
	ylab="Fahrradfahrer je Stunde",
	xlab="",
      border="black",
	axes=FALSE)
axis(2)
axis(1, at = c(1,2,3,4,5,6,7,8),labels=c("Jungbuschbr.",
	"Konrad Adenauer Br.",
	"Kurpfalzbr.",
	"Kurt Schuhm. Br.",
	"Lindenhof Ueberf.",
	"Neckarauer Ueberw.",
	"Renzstrasse",
	"Schlosspark"),
	las=1)



datensatz_Renzstrasse = datensatz %>%
	filter(Standort =="Renzstraße")

datensatz_Kurpfalzbruecke = datensatz %>%
	filter(Standort =="Kurpfalzbrücke gesamt")

datensatz_Jungbuschbruecke = datensatz %>%
	filter(Standort =="Jungbuschbrücke")

datensatz_KABruecke = datensatz %>%
	filter(Standort =="Konrad-Adenauer-Brücke (Süd)")

datensatz_KSbruecke = datensatz %>%
	filter(Standort =="Kurt-Schumacher-Brücke Süd (Hafenstr.)")

datensatz_NeckarauerUebergang = datensatz %>%
	filter(Standort =="Neckarauer Übergang -Schwetzinger Str.")

datensatz_Lindenhof = datensatz %>%
	filter(Standort =="Lindenhofüberführung")

datensatz_Schlosspark = datensatz %>%
	filter(Standort =="Schlosspark Lindenhof (Richtung Jugendherberge)")

names(datensatz_Renzstrasse)

A=c("Renzstr.",
	datensatz_Renzstrasse$laengengrad[1],
	datensatz_Renzstrasse$breitengrad[1],
	min(datensatz_Renzstrasse$Zaehlstand),
	mean(datensatz_Renzstrasse$Zaehlstand),
	max(datensatz_Renzstrasse$Zaehlstand),
	min(datensatz_Renzstrasse$Jahr),
	mean(datensatz_Renzstrasse$QualitaetRR),
	mean(datensatz_Renzstrasse$QualitaetT2M),
	mean(datensatz_Renzstrasse$QualitaetF),
	mean(datensatz_Renzstrasse$QualitaetRF),
	mean(datensatz_Renzstrasse$QualitaetSD),
	mean(datensatz_Renzstrasse$QualitaetN)
)

B=c("Kurpfalzbr.",
	datensatz_Kurpfalzbruecke$laengengrad[1],
	datensatz_Kurpfalzbruecke$breitengrad[1],
	min(datensatz_Kurpfalzbruecke$Zaehlstand),
	mean(datensatz_Kurpfalzbruecke$Zaehlstand),
	max(datensatz_Kurpfalzbruecke$Zaehlstand),
	min(datensatz_Kurpfalzbruecke$Jahr),
	mean(datensatz_Kurpfalzbruecke$QualitaetRR),
	mean(datensatz_Kurpfalzbruecke$QualitaetT2M),
	mean(datensatz_Kurpfalzbruecke$QualitaetF),
	mean(datensatz_Kurpfalzbruecke$QualitaetRF),
	mean(datensatz_Kurpfalzbruecke$QualitaetSD),
	mean(datensatz_Kurpfalzbruecke$QualitaetN)
)

C=c("Jungbuschbr.",
	datensatz_Jungbuschbruecke$laengengrad[1],
	datensatz_Jungbuschbruecke$breitengrad[1],
	min(datensatz_Jungbuschbruecke$Zaehlstand),
	mean(datensatz_Jungbuschbruecke$Zaehlstand),
	max(datensatz_Jungbuschbruecke$Zaehlstand),
	min(datensatz_Jungbuschbruecke$Jahr),
	mean(datensatz_Jungbuschbruecke$QualitaetRR),
	mean(datensatz_Jungbuschbruecke$QualitaetT2M),
	mean(datensatz_Jungbuschbruecke$QualitaetF),
	mean(datensatz_Jungbuschbruecke$QualitaetRF),
	mean(datensatz_Jungbuschbruecke$QualitaetSD),
	mean(datensatz_Jungbuschbruecke$QualitaetN)
)

D=c("K Adenauer Br.",
	datensatz_KABruecke$laengengrad[1],
	datensatz_KABruecke$breitengrad[1],
	min(datensatz_KABruecke$Zaehlstand),
	mean(datensatz_KABruecke$Zaehlstand),
	max(datensatz_KABruecke$Zaehlstand),
	min(datensatz_KABruecke$Jahr),
	mean(datensatz_KABruecke$QualitaetRR),
	mean(datensatz_KABruecke$QualitaetT2M),
	mean(datensatz_KABruecke$QualitaetF),
	mean(datensatz_KABruecke$QualitaetRF),
	mean(datensatz_KABruecke$QualitaetSD),
	mean(datensatz_KABruecke$QualitaetN)
)

E=c("K Schuhm. Br.",
	datensatz_KSbruecke$laengengrad[1],
	datensatz_KSbruecke$breitengrad[1],
	min(datensatz_KSbruecke$Zaehlstand),
	mean(datensatz_KSbruecke$Zaehlstand),
	max(datensatz_KSbruecke$Zaehlstand),
	min(datensatz_KSbruecke$Jahr),
	mean(datensatz_KSbruecke$QualitaetRR),
	mean(datensatz_KSbruecke$QualitaetT2M),
	mean(datensatz_KSbruecke$QualitaetF),
	mean(datensatz_KSbruecke$QualitaetRF),
	mean(datensatz_KSbruecke$QualitaetSD),
	mean(datensatz_KSbruecke$QualitaetN)
)

F=c("NeckarauerUeberg",
	datensatz_NeckarauerUebergang$laengengrad[1],
	datensatz_NeckarauerUebergang$breitengrad[1],
	min(datensatz_NeckarauerUebergang$Zaehlstand),
	mean(datensatz_NeckarauerUebergang$Zaehlstand),
	max(datensatz_NeckarauerUebergang$Zaehlstand),
	min(datensatz_NeckarauerUebergang$Jahr),
	mean(datensatz_NeckarauerUebergang$QualitaetRR),
	mean(datensatz_NeckarauerUebergang$QualitaetT2M),
	mean(datensatz_NeckarauerUebergang$QualitaetF),
	mean(datensatz_NeckarauerUebergang$QualitaetRF),
	mean(datensatz_NeckarauerUebergang$QualitaetSD),
	mean(datensatz_NeckarauerUebergang$QualitaetN)
)

G=c("Lindenhof",
	datensatz_Lindenhof$laengengrad[1],
	datensatz_Lindenhof$breitengrad[1],
	min(datensatz_Lindenhof$Zaehlstand),
	mean(datensatz_Lindenhof$Zaehlstand),
	max(datensatz_Lindenhof$Zaehlstand),
	min(datensatz_Lindenhof$Jahr),
	mean(datensatz_Lindenhof$QualitaetRR),
	mean(datensatz_Lindenhof$QualitaetT2M),
	mean(datensatz_Lindenhof$QualitaetF),
	mean(datensatz_Lindenhof$QualitaetRF),
	mean(datensatz_Lindenhof$QualitaetSD),
	mean(datensatz_Lindenhof$QualitaetN)
)

H=c("Schlosspark",
	datensatz_Schlosspark$laengengrad[1],
	datensatz_Schlosspark$breitengrad[1],
	min(datensatz_Schlosspark$Zaehlstand),
	mean(datensatz_Schlosspark$Zaehlstand),
	max(datensatz_Schlosspark$Zaehlstand),
	min(datensatz_Schlosspark$Jahr),
	mean(datensatz_Schlosspark$QualitaetRR),
	mean(datensatz_Schlosspark$QualitaetT2M),
	mean(datensatz_Schlosspark$QualitaetF),
	mean(datensatz_Schlosspark$QualitaetRF),
	mean(datensatz_Schlosspark$QualitaetSD),
	mean(datensatz_Schlosspark$QualitaetN)
)
datensatz_Renzstra�e[1:10,]

A
B
C
D
E
F
G
H

Station=rbind(A,B)
Station=rbind(Station,C)
Station=rbind(Station,D)
Station=rbind(Station,E)
Station=rbind(Station,F)
Station=rbind(Station,G)
Station=rbind(Station,H)

Station = as.data.frame(Station)
class(Station)

names(Station)=c("Standort",
	"laengengrad",
	"breitengrad",
	"Min",
	"Mean",
	"Max",
	"Jahr",
	"QualitaetRR",
	"QualitaetT2M",
	"QualitaetF",
	"QualitaetRF",
	"QualitaetSD",
	"QualitaetN")

citation("ggmap")

class(Station$laengengrad)


myLocation<-c(8.451, 49.472,    8.49, 49.5) # ~Mannheim

myMap <- get_stamenmap(bbox=myLocation, maptype="terrain-lines", zoom=15)

ggmap(myMap)+
ggtitle(label="Durchschnitssauslastung an allen Standorten") +
geom_point(aes(x=as.numeric(laengengrad), y=as.numeric(breitengrad), color = as.numeric(Mean)), data=Station, size = 5) + 
scale_color_gradient(low = "green", high = "red",name="Durchschnitt") +
geom_text(data = Station, aes(x = as.numeric(laengengrad), y = as.numeric(breitengrad), label = Standort), 
          check_overlap = T,nudge_y = 0.0006,size=5) +
geom_text(data = Station, aes(x = as.numeric(laengengrad), y = as.numeric(breitengrad), label = Jahr), 
          check_overlap = T,nudge_y = -0.0001,size=4)
)








