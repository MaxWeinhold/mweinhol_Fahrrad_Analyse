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
#install.packages("tidyverse")
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
	zaehlstellen$Zeit_neu	= as.POSIXct(t, format="%Y-%m-%d T%H:%M:%S")	#Datum_Uhrzeit
	zaehlstellen$Datum	= as.POSIXct(t, format="%Y-%m-%d")			#Nur_Datum
	zaehlstellen$Stunde	= format(zaehlstellen$Zeit_neu, format = "%H")	#Nur Stunde
	
	zaehlstellen <- zaehlstellen %>%
     		select(Standort, Zaehlstand, Koordinaten, Zeit_neu, Datum, Stunde, Jahr, Monat, Tag, Wochentag)
	
#Wochenende, Sommer, Feiertage, Schul- und Semesterferien

	datensatz = zaehlstellen
	
    	# Wochenende
    	datensatz$Wochenende <- ifelse(datensatz$Wochentag == "So" | datensatz$Wochentag == "Sa", "Wochenende", "Wochentag")
    	table(datensatz$Wochenende) 
    
    	# Sommer
    	datensatz$Sommer <- ifelse(as.numeric(datensatz$Monat) %in% c(4:9), 1, 0)
    	table(datensatz$Sommer)

	min(datensatz$Jahr)
	max(datensatz$Jahr)

	#https://kalender-2018.net/feiertage-nordrhein-westfalen/

	feiertage_bw_14_22=c("1.1.2014","6.1.2014","18.4.2014","21.4.2014",
		"1.5.2014","29.5.2014","9.6.2014","19.6.2014","3.10.2014",
		"1.11.2014","25.12.2014","26.12.2014","1.1.2015","6.1.2015",
		"3.4.2015","6.4.2015","1.5.2015","14.5.2015","25.5.2015",
		"4.6.2015","3.10.2015","1.11.2015","25.12.2015","26.12.2015",
		"1.1.2016","6.1.2016","25.3.2016","28.3.2016","1.5.2016",
		"5.5.2016","16.5.2016","26.5.2016","3.10.2016","1.11.2016",
		"25.12.2016","26.12.2016","1.1.2017","6.1.2017","14.4.2017",
		"17.4.2017","1.5.2017","25.5.2017","5.6.2017","15.6.2017",
		"3.10.2017","31.10.2017","1.11.2017","25.12.2017","26.12.2017",
		"1.1.2018","6.1.2018","30.3.2018","2.4.2018","1.5.2018",
		"10.5.2018","21.5.2018","31.5.2018","3.10.2018","1.11.2018",
		"25.12.2018","26.12.2018","1.1.2019","6.1.2019","19.4.2019",
		"22.4.2019","1.5.2019","30.5.2019","10.6.2019","20.6.2019",
		"3.10.2019","1.11.2019","25.12.2019","26.12.2019","1.1.2020",
		"6.1.2020","10.4.2020","13.4.2020","1.5.2020","21.5.2020",
		"1.6.2020","11.6.2020","3.10.2020","1.11.2020","25.12.2020",
		"26.12.2020","1.1.2021","6.1.2021","2.4.2021","5.4.2021",
		"1.5.2021","13.5.2021","24.5.2021","3.6.2021","3.10.2021",
		"1.11.2021","25.12.2021","26.12.2021","1.1.2022","6.1.2022",
		"15.4.2022","18.4.2022","1.5.2022","26.5.2022","6.6.2022",
		"16.6.2022","3.10.2022","1.11.2022","25.12.2022","26.12.2022")

	Feiertage_bw = as.POSIXct(feiertage_bw_14_22, format="%d.%m.%Y")

	feiertage_rp_14_22=c("1.1.2014","18.4.2014","21.4.2014","1.5.2014",
		"29.5.2014","9.6.2014","19.6.2014","3.10.2014","1.11.2014",
		"25.12.2014","26.12.2014","1.1.2015","3.4.2015","6.4.2015",
		"1.5.2015","14.5.2015","25.5.2015","4.6.2015","3.10.2015",
		"1.11.2015","25.12.2015","26.12.2015","1.1.2016","15.3.2016",
		"28.3.2016","1.5.2016","5.5.2016","16.5.2016","26.5.2016",
		"3.10.2016","1.11.2016","25.12.2016","26.12.2016","1.1.2017",
		"14.4.2017","17.4.2017","1.5.2017","25.5.2017","5.6.2017",
		"15.6.2017","3.10.2017","31.10.2017","1.11.2017","25.12.2017",
		"26.12.2017","1.1.2018","30.3.2018","2.4.2018","1.5.2018",
		"10.5.2018","21.5.2018","31.5.2018","3.10.2018","1.11.2018",
		"25.12.2018","26.12.2018","1.1.2019","19.4.2019","22.4.2019",
		"1.5.2019","30.5.2019","10.6.2019","20.6.2019","3.10.2019",
		"1.11.2019","25.12.2019","26.12.2019","1.1.2020","10.4.2020",
		"13.4.2020","1.5.2020","21.5.2020","1.6.2020","11.6.2020",
		"3.10.2020","1.11.2020","25.12.2020","26.12.2020","1.1.2021",
		"2.4.2021","5.4.2021","1.5.2021","13.5.2021","24.5.2021",
		"3.6.2021","3.10.2021","1.11.2021","25.12.2021","26.12.2021",
		"1.1.2022","15.4.2022","18.4.2022","1.5.2022","26.5.2022",
		"6.6.2022","16.6.20022","3.10.2022","1.11.2022","25.12.2022",
		"26.12.2022")

	Feiertage_rp = as.POSIXct(feiertage_rp_14_22, format="%d.%m.%Y")

	#https://www.schulferien.org/Kalender_mit_Ferien/kalender_2018_ferien_Nordrhein_Westfalen.html
	
	schulferien_bw_14_22=c(20140101:20140104,20140414:20140425,20140610:20140621,20140731,
		20140801:20140831,20140901:20140921,20141027:20141031,20141222:20141231,20150101:20150105,
		20150330,20150331,20150401:20150410,20150526:20150531,20150601:20150606,20150730,20150731,
		20150801:20150831,20150901:20150912,20151031,20151102:20151106,20151223:20151231,
		20160101:20160109,20160329:20160331,20160401:20160403,20160517:20160528,20160728:20160731,
		20160801:20160831,20160901:20160910,20161102:20161104,20161226:20161231,20170101:20170107,
		20170410:20170421,20170606:20170616,20170727:20170731,20170801:20170831,20170801:20170809,
		20171030,20171031,20171101:20171103,20171222:20171231,20180101:20180105,20180326:20180231,
		20180401:20180406,20180522:20180531,20180601,20180602,20180726:20170731,20180801:20180831,
		20180901:20180908,20181029:20181031,20281101,20181102,20181224:20181231,20190101:20190105,
		20190415:20190427,20190611:20190621,20190729:20190731,20190801:20190831,20190901:20190910,
		20191028:20191031,20191223:20191231,20200101:20200104,20200406:20200418,20200602:20200613,
		20200731,20200631,20200801:20200831,20200901:20200912,20201026:20201031,20201223:20201231,
		20210101:20210109,20210401,20210406:20210410,20210525:20210531,20210601:20210605,
		20210729:20210731,20210801:20210831,20210901:20210911,20211031,20211102:20211106,
		20211223:20211231,20220101:20220108,20220414,20220419:20220423,20220607:20220618,
		20220728:20220731,20220801:20220831,20220901:20220910,20221031,20221102:20221104,
		20221221:20221231
	)

	SchulferienBW = as.POSIXct(as.character(schulferien_bw_14_22), format="%Y%m%d")
	#https://www.uni-muenster.de/studium/orga/termine_archiv.html
	#Termine Uni Mannheim und DHBW Mannheim. Uni Mannheim bereits angefragt

	datensatz$FeiertagBW = ifelse(datensatz$Datum %in% Feiertage_bw,1,0)
	datensatz$FeiertagRP = ifelse(datensatz$Datum %in% Feiertage_rp,1,0)
	datensatz$SchulferienBW = ifelse(datensatz$Datum %in% SchulferienBW,1,0)
	#datensatz$semesterferien = ifelse(datensatz$Datum %in% c(semesterferien_2018,semesterferien_2019),1,0

	summary(datensatz)

###
#Wetterdaten einladen
###

	#niederschlag = read_csv("Daten/Wetter_Daten/data_OBS_DEU_PT1H_RR.csv")
	#lufttemperatur = read_csv("Daten/Wetter_Daten/data_OBS_DEU_PT1H_T2M_1766.csv")
	#wind = read_csv("Daten/Wetter_Daten/data_OBS_DEU_PT1H_F_1766.csv")



