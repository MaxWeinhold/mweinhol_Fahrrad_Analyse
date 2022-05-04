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

#Libraries laden (Wenn nötig vorher installiere)

#install.packages("readxl")
#install.packages("lubridate")
#install.packages("Rcpp")
install.packages("tidyverse")
library(readxl)
library(tidyverse)
library(lubridate)

#-------------------------------------------------------------------
#Zählstationsdaten einladen-----------------------------------------

	zaehlstelle_jungbuschbrucke 			= read.csv(file = "Daten/jungbuschbrucke-eco-counter-daten-kopieren.csv",sep=";")
	zaehlstelle_konrad_adenau_bruecke_sued 	= read.csv(file = "Daten/konrad_adenau_bruecke_sued-gesamt-eco-counter-daten.csv",sep=";")
	zaehlstelle_kurpfalzbrucke 	 		= read.csv(file = "Daten/kurpfalzbrucke-gesamt-eco-counter-daten.csv",sep=";")
	zaehlstelle_kurt_schumacher_brucke_sud 	= read.csv(file = "Daten/kurt-schumacher-brucke-sud-hafenstr-eco-counter-daten.csv",sep=";")
	zaehlstelle_lindenhofuberfuhrung 		= read.csv(file = "Daten/lindenhofuberfuhrung-eco-counter-verkehrszahler.csv",sep=";")
	zaehlstelle_neckarauer_ubergang 		= read.csv(file = "Daten/neckarauer-ubergang-eco-counter-verkehrszahler.csv",sep=";")
	zaehlstelle_renzstrasse 			= read.csv(file = "Daten/renzstr-eco-counter-daten.csv",sep=";")
	zaehlstelle_schlosspark_lindenhof 		= read.csv(file = "Daten/schlosspark-lindenhof-eco-counter-verkehrszahler.csv",sep=";")
	
	summary(zaehlstelle_jungbuschbrucke)
	summary(zaehlstelle_konrad_adenau_bruecke_sued)
	summary(zaehlstelle_urpfalzbrucke)
	summary(zaehlstelle_kurt_schumacher_brucke_sud)
	summary(zaehlstelle_lindenhofuberfuhrung)
	summary(zaehlstelle_neckarauer_ubergang)
	summary(zaehlstelle_renzstrasse)
	summary(zaehlstelle_schlosspark_lindenhof)

#Daten miteinander verbinden, Spalten umbenennen und unbenötigte Spalten löschen

	zaehlstellen = rbind(zaehlstelle_jungbuschbrucke,
		zaehlstelle_konrad_adenau_bruecke_sued,
		zaehlstelle_kurpfalzbrucke,
		zaehlstelle_kurt_schumacher_brucke_sud,
		zaehlstelle_lindenhofuberfuhrung,
		zaehlstelle_neckarauer_ubergang,
		zaehlstelle_renzstrasse,
		zaehlstelle_schlosspark_lindenhof)

	names(zaehlstellen)
	names(zaehlstellen)[2]="Zaehlstand"
	names(zaehlstellen)[3]="Zaehlernummer"
	names(zaehlstellen)

	zaehlstellen <- zaehlstellen %>%
     		select(Datum.und.Uhrzeit, Zaehlstand, Standort, Koordinaten)

	names(zaehlstellen)

#Wandle Datumsformate um in Uhrzeit, Tag, Monat und Jahr etc.

	t=zaehlstellen$Datum.und.Uhrzeit
	t[1]
	
	zaehlstellen$Jahr		= format(as.Date(t, format="%Y-%m-%d"),"%Y") 	#Jahr
	zaehlstellen$Monat	= format(as.Date(t, format="%Y-%m-%d"),"%m") 	#Monat
	zaehlstellen$Tag		= format(as.Date(t, format="%Y-%m-%d"),"%d") 	#Tag im Monat
	zaehlstellen$Wochentag	= format(as.Date(t, format="%Y-%m-%d"),"%a") 	#Wochentag
	zaehlstellen$Zeit_neu	= as.POSIXct(t, format="%Y-%m-%d T%H:%M:%S")	#Gesamtdatum
	zaehlstellen$Stunde	= format(zaehlstellen$Zeit_neu, format = "%H")	#Nur Stunde
	
	zaehlstellen <- zaehlstellen %>%
     		select(Standort, Zaehlstand, Koordinaten, Zeit_neu, Stunde, Jahr, Monat, Tag, Wochentag)
	
#Wochenende, Sommer, Feiertage, Schul- und Semesterferien

	datensatz = zaehlstellen

    	# Wochenende
    	datensatz$Wochenende <- ifelse(datensatz$Wochentag == "So" | datensatz$Wochentag == "Sa", "Wochenende", "Wochentag")
    	table(datensatz$Wochenende) 
    
    	# Sommer
    	datensatz$Sommer <- ifelse(datensatz$Monat %in% c(4:9), 1, 0)
    	table(datensatz$Sommer)

	#https://kalender-2018.net/feiertage-nordrhein-westfalen/
	#https://www.schulferien.org/Kalender_mit_Ferien/kalender_2018_ferien_Nordrhein_Westfalen.html
	#https://www.uni-muenster.de/studium/orga/termine_archiv.html

	names(datensatz)


