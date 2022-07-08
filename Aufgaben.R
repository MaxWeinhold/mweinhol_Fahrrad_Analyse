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
#install.packages("xtable")
library(xtable)
library(ggmap)

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

unique(datensatz$Standort)

#3. Beschreibung der Daten

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

#4. Deskriptive Analyse

#4.1 Tabelle mit deskriptiven Statistiken

names(datensatz)

sd(datensatz$Zaehlstand)

cor(datensatz$Zaehlstand, datensatz$uniMA_dist, method = "pearson")

A = c("Zaehlstand",
	round(min(datensatz$Zaehlstand), 2),
	round(mean(datensatz$Zaehlstand), 2),
	round(max(datensatz$Zaehlstand), 2),
	round(sd(datensatz$Zaehlstand), 2),
	round(cor(datensatz$Zaehlstand, datensatz$Zaehlstand, method = "pearson"), 2)
)

B = c("Stunde",
	round(min(datensatz$Stunde), 2),
	round(mean(datensatz$Stunde), 2),
	round(max(datensatz$Stunde), 2),
	round(sd(datensatz$Stunde), 2),
	round(cor(datensatz$Zaehlstand, datensatz$Stunde, method = "pearson"), 2)
)

C = c("Distanz zur Uni MA in m",
	round(min(datensatz$uniMA_dist), 2),
	round(mean(datensatz$uniMA_dist), 2),
	round(max(datensatz$uniMA_dist), 2),
	round(sd(datensatz$uniMA_dist), 2),
	round(cor(datensatz$Zaehlstand, datensatz$uniMA_dist, method = "pearson"), 2)
)

D = c("Datum",
	round(min(datensatz$Datum), 2),
	round(mean(datensatz$Datum), 2),
	round(max(datensatz$Datum), 2),
	round(sd(datensatz$Datum), 2),
	round(cor(datensatz$Zaehlstand, datensatz$Datum, method = "pearson"), 2)
)

E = c("Wochenende",
	round(min(datensatz$Wochenende), 2),
	round(mean(datensatz$Wochenende), 2),
	round(max(datensatz$Wochenende), 2),
	round(sd(datensatz$Wochenende), 2),
	round(cor(datensatz$Zaehlstand, datensatz$Wochenende, method = "pearson"), 2)
)

F = c("Sommer",
	round(min(datensatz$Sommer), 2),
	round(mean(datensatz$Sommer), 2),
	round(max(datensatz$Sommer), 2),
	round(sd(datensatz$Sommer), 2),
	round(cor(datensatz$Zaehlstand, datensatz$Sommer, method = "pearson"), 2)
)

G = c("Feiertag BW",
	round(min(datensatz$FeiertagBW), 2),
	round(mean(datensatz$FeiertagBW), 2),
	round(max(datensatz$FeiertagBW), 2),
	round(sd(datensatz$FeiertagBW), 2),
	round(cor(datensatz$Zaehlstand, datensatz$FeiertagBW, method = "pearson"), 2)
)

H = c("Feiertag RP",
	round(min(datensatz$FeiertagRP), 2),
	round(mean(datensatz$FeiertagRP), 2),
	round(max(datensatz$FeiertagRP), 2),
	round(sd(datensatz$FeiertagRP), 2),
	round(cor(datensatz$Zaehlstand, datensatz$FeiertagRP, method = "pearson"), 2)
)

I = c("Schulferien BW",
	round(min(datensatz$SchulferienBW), 2),
	round(mean(datensatz$SchulferienBW), 2),
	round(max(datensatz$SchulferienBW), 2),
	round(sd(datensatz$SchulferienBW), 2),
	round(cor(datensatz$Zaehlstand, datensatz$SchulferienBW, method = "pearson"), 2)
)

J = c("Semesterferien",
	round(min(datensatz$SemesterferionUM), 2),
	round(mean(datensatz$SemesterferionUM), 2),
	round(max(datensatz$SemesterferionUM), 2),
	round(sd(datensatz$SemesterferionUM), 2),
	round(cor(datensatz$Zaehlstand, datensatz$SemesterferionUM, method = "pearson"), 2)
)

K = c("WertRR",
	round(min(datensatz$WertRR), 2),
	round(mean(datensatz$WertRR), 2),
	round(max(datensatz$WertRR), 2),
	round(sd(datensatz$WertRR), 2),
	round(cor(datensatz$Zaehlstand, datensatz$WertRR, method = "pearson"), 2)
)

L = c("QualitaetRR",
	round(min(datensatz$QualitaetRR), 2),
	round(mean(datensatz$QualitaetRR), 2),
	round(max(datensatz$QualitaetRR), 2),
	round(sd(datensatz$QualitaetRR), 2),
	round(cor(datensatz$Zaehlstand, datensatz$QualitaetRR, method = "pearson"), 2)
)

M = c("WertT2M",
	round(min(datensatz$WertT2M), 2),
	round(mean(datensatz$WertT2M), 2),
	round(max(datensatz$WertT2M), 2),
	round(sd(datensatz$WertT2M), 2),
	round(cor(datensatz$Zaehlstand, datensatz$WertT2M, method = "pearson"), 2)
)

N = c("QualitaetT2M",
	round(min(datensatz$QualitaetT2M), 2),
	round(mean(datensatz$QualitaetT2M), 2),
	round(max(datensatz$QualitaetT2M), 2),
	round(sd(datensatz$QualitaetT2M), 2),
	round(cor(datensatz$Zaehlstand, datensatz$QualitaetT2M, method = "pearson"), 2)
)

O = c("WertF",
	round(min(datensatz$WertF), 2),
	round(mean(datensatz$WertF), 2),
	round(max(datensatz$WertF), 2),
	round(sd(datensatz$WertF), 2),
	round(cor(datensatz$Zaehlstand, datensatz$WertF, method = "pearson"), 2)
)

P = c("QualitaetF",
	round(min(datensatz$QualitaetF), 2),
	round(mean(datensatz$QualitaetF), 2),
	round(max(datensatz$QualitaetF), 2),
	round(sd(datensatz$QualitaetF), 2),
	round(cor(datensatz$Zaehlstand, datensatz$QualitaetF, method = "pearson"), 2)
)

Q = c("WertRF",
	round(min(datensatz$WertRF), 2),
	round(mean(datensatz$WertRF), 2),
	round(max(datensatz$WertRF), 2),
	round(sd(datensatz$WertRF), 2),
	round(cor(datensatz$Zaehlstand, datensatz$WertRF, method = "pearson"), 2)
)

R = c("QualitaetRF",
	round(min(datensatz$QualitaetRF), 2),
	round(mean(datensatz$QualitaetRF), 2),
	round(max(datensatz$QualitaetRF), 2),
	round(sd(datensatz$QualitaetRF), 2),
	round(cor(datensatz$Zaehlstand, datensatz$QualitaetRF, method = "pearson"), 2)
)

S = c("WertSD",
	round(min(datensatz$WertSD), 2),
	round(mean(datensatz$WertSD), 2),
	round(max(datensatz$WertSD), 2),
	round(sd(datensatz$WertSD), 2),
	round(cor(datensatz$Zaehlstand, datensatz$WertSD, method = "pearson"), 2)
)

T = c("QualitaetSD",
	round(min(datensatz$QualitaetSD), 2),
	round(mean(datensatz$QualitaetSD), 2),
	round(max(datensatz$QualitaetSD), 2),
	round(sd(datensatz$QualitaetSD), 2),
	round(cor(datensatz$Zaehlstand, datensatz$QualitaetSD, method = "pearson"), 2)
)

U = c("WertN",
	round(min(datensatz$WertN), 2),
	round(mean(datensatz$WertN), 2),
	round(max(datensatz$WertN), 2),
	round(sd(datensatz$WertN), 2),
	round(cor(datensatz$Zaehlstand, datensatz$WertN, method = "pearson"), 2)
)

V = c("QualitaetN",
	round(min(datensatz$QualitaetN), 2),
	round(mean(datensatz$QualitaetN), 2),
	round(max(datensatz$QualitaetN), 2),
	round(sd(datensatz$QualitaetN), 2),
	round(cor(datensatz$Zaehlstand, datensatz$QualitaetN, method = "pearson"), 2)
)

W = c("Corona",
	round(min(datensatz$Corona), 2),
	round(mean(datensatz$Corona), 2),
	round(max(datensatz$Corona), 2),
	round(sd(datensatz$Corona), 2),
	round(cor(datensatz$Zaehlstand, datensatz$Corona, method = "pearson"), 2)
)

X = c("Kontaktbeschr",
	round(min(datensatz$Kontaktbeschr), 2),
	round(mean(datensatz$Kontaktbeschr), 2),
	round(max(datensatz$Kontaktbeschr), 2),
	round(sd(datensatz$Kontaktbeschr), 2),
	round(cor(datensatz$Zaehlstand, datensatz$Kontaktbeschr, method = "pearson"), 2)
)

Y = c("TagesAusbr",
	round(min(datensatz$TagesAusbr), 2),
	round(mean(datensatz$TagesAusbr), 2),
	round(max(datensatz$TagesAusbr), 2),
	round(sd(datensatz$TagesAusbr), 2),
	round(cor(datensatz$Zaehlstand, datensatz$TagesAusbr, method = "pearson"), 2)
)

Uebersicht=rbind(A,B)
Uebersicht=rbind(Uebersicht,C)
Uebersicht=rbind(Uebersicht,D)
Uebersicht=rbind(Uebersicht,E)
Uebersicht=rbind(Uebersicht,F)
Uebersicht=rbind(Uebersicht,G)
Uebersicht=rbind(Uebersicht,H)
Uebersicht=rbind(Uebersicht,I)
Uebersicht=rbind(Uebersicht,J)
Uebersicht=rbind(Uebersicht,K)
Uebersicht=rbind(Uebersicht,L)
Uebersicht=rbind(Uebersicht,M)
Uebersicht=rbind(Uebersicht,N)
Uebersicht=rbind(Uebersicht,O)
Uebersicht=rbind(Uebersicht,P)
Uebersicht=rbind(Uebersicht,Q)
Uebersicht=rbind(Uebersicht,R)
Uebersicht=rbind(Uebersicht,S)
Uebersicht=rbind(Uebersicht,T)
Uebersicht=rbind(Uebersicht,U)
Uebersicht=rbind(Uebersicht,V)
Uebersicht=rbind(Uebersicht,W)
Uebersicht=rbind(Uebersicht,X)
Uebersicht=rbind(Uebersicht,Y)
Uebersicht

Uebersicht = as.data.frame(Uebersicht)
class(Uebersicht)

names(Uebersicht)=c("Name",
	"Minimum",
	"Durchschnitt",
	"Maximum",
	"Stand.abw.",
	"Korrelation")

round(Uebersicht, 2)

xtable(Uebersicht)

#2. Korrelationen zwischen den Variablen
#Im Anhang bearbeiten

#install.packages("corrplot")
library(corrplot)

names(datensatz)
d=datensatz

d <- datensatz %>%
     	dplyr::select(Zaehlstand, uniMA_dist, Wochenende, Sommer, FeiertagBW,
	FeiertagRP, SchulferienBW, SemesterferionUM, WertRR, WertT2M, WertF,
	WertRF, WertSD, WertN, Corona, Kontaktbeschr, TagesAusbr)

corrplot(cor(d), method="color", type="lower")
corrplot(cor(d), method="circle", type="lower")

#3. Niederschlag und Temperatur

names(datensatz)

datensatz_2018 = datensatz %>%
	filter(Jahr == "2018")

datensatz_2018_morgens = datensatz_2018 %>%
	filter(Stunde <= "10")
datensatz_2018_mittags = datensatz_2018 %>%
	filter(Stunde > "10" & Stunde <= "14")
datensatz_2018_nachmittags = datensatz_2018 %>%
	filter(Stunde > "14" & Stunde <= "19")
datensatz_2018_abends = datensatz_2018 %>%
	filter(Stunde > "19")

MonatsRegen = datensatz_2018 %>%
    group_by(Monat) %>%
    slice(which.max(WertRR))
MonatsRegen_morgens = datensatz_2018_morgens %>%
    group_by(Monat) %>%
    slice(which.max(WertRR))
MonatsRegen_mittags = datensatz_2018_mittags %>%
    group_by(Monat) %>%
    slice(which.max(WertRR))
MonatsRegen_nachmittags = datensatz_2018_nachmittags %>%
    group_by(Monat) %>%
    slice(which.max(WertRR))
MonatsRegen_abends = datensatz_2018_abends %>%
    group_by(Monat) %>%
    slice(which.max(WertRR))

MonatsTemp=aggregate(datensatz_2018$WertT2M, list(datensatz_2018$Monat), FUN=mean) 

rm(df)

df=cbind(MonatsTemp,MonatsRegen_morgens$WertRR)
df=cbind(df,MonatsRegen_mittags$WertRR)
df=cbind(df,MonatsRegen_nachmittags$WertRR)
df=cbind(df,MonatsRegen_abends$WertRR)

names(df)[1]="Monat"
names(df)[2]="Temp"
names(df)[3]="Regen1"
names(df)[4]="Regen2"
names(df)[5]="Regen3"
names(df)[6]="Regen4"

ggplot() + 
	geom_line(mapping = aes(x = df$Monat, y = df$Temp, group = 1), color = "red")

ggplot() + 
	geom_bar(mapping = aes(x = df$Monat, y = df$Regen1), stat = "identity", fill = "blue") +
	geom_bar(mapping = aes(x = df$Monat, y = df$Regen2), stat = "identity", fill = "green") +
	geom_bar(mapping = aes(x = df$Monat, y = df$Regen3), stat = "identity", fill = "orange") +
	geom_bar(mapping = aes(x = df$Monat, y = df$Regen4), stat = "identity", fill = "yellow") +
	geom_line(mapping = aes(x = df$Monat, y = df$Temp, group = 1), size = 2 , color = "red") +
  	scale_y_continuous(name = "Temperatur", 
    		sec.axis = sec_axis(~./5, name = "Niederschlag")) + 
  	theme(
      	axis.title.y = element_text(color = "blue"),
      	axis.title.y.right = element_text(color = "red"))

ggplot() + 
	geom_bar(mapping = aes(x = df$Monat, y = df$Regen1), stat = "identity", fill = "blue") +
  	geom_line(mapping = aes(x = df$Monat, y = df$Temp), size = 2, color = "blue") + 
  	scale_y_continuous(name = "Interruptions/day", 
    		sec.axis = sec_axis(~./5, name = "Productivity % of best", 
      	labels = function(b) { paste0(round(b * 100, 0), "%")})) + 
  	theme(
      	axis.title.y = element_text(color = "grey"),
      	axis.title.y.right = element_text(color = "blue"))












