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
#install.packages("geosphere")
library(readxl)
library(tidyverse)
library(lubridate)
library(geosphere)

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
     		dplyr::select(Datum.und.Uhrzeit, Zaehlstand, Standort, Koordinaten)

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
     		dplyr::select(Standort, Zaehlstand, Koordinaten, Zeit_neu, Datum, Stunde, Jahr, Monat, Tag, Wochentag)

#Koordinaten

	names(zaehlstellen)
	zaehlstellen$Koordinaten[1]
	zaehlstellen$laengengrad = as.numeric(str_split_fixed(zaehlstellen$Koordinaten,',', 2)[,2])
	zaehlstellen$breitengrad = as.numeric(str_split_fixed(zaehlstellen$Koordinaten,',', 2)[,1])

	uniMA_breiteng=49.483397
	uniMA_laengeng=8.462194
	zaehlstellen$uniMA_dist=1

	for(i in 1:length(zaehlstellen$uniMA_dist)) {
		zaehlstellen$uniMA_dist[i] = distm(c(uniMA_laengeng, uniMA_breiteng), c(zaehlstellen$laengengrad[i], zaehlstellen$breitengrad[i]), fun = distHaversine)
	}

	zaehlstellen <- zaehlstellen %>%
     		dplyr::select(Standort, Zaehlstand, laengengrad, breitengrad, uniMA_dist, Zeit_neu, Datum, Stunde, Jahr, Monat, Tag, Wochentag)

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

	semesterferien_UniMA=c(20140101:20140104,20140101:20143101,20140201:20140210,20140414:20140425,20140531,
		20140601:20140631,20140701:20140731,20140801:20140831,20141206:20140631,20150101:20150131,
		20150201:20150208,20150330,20150331,20150401:20140410,20150530,20150531,20150601:20150631,
		20150701:20150731,20150801:20150831,20151212:20151231,20160101:20160131,20160201:20160214,
		20160321:20160331,20160401,20160603:20160631,20160701:20160731,20160801:20160831,
		20161222:20161231,20170101:20170131,20170201:20170212,20170410:20170423,20170605:20170631,
		20170701:20170731,20170801:20170831,20171211:20171231,20180101:20180131,20180201:20180211,
		20180324:20180331,20180401:20180407,20180604:20180631,20180701:20180731,20180801:20180831,
		20181209:20181231,20190101:20190131,20190201:20190210,20190415:20190428,20190601:20190631,
		20190701:20190731,20190801:20190831,20191208:20191231,20200101:20200131,20200201:20200209,
		20200406:20200419,20200601:20200631,20200701:20200731,20200801:20200831,20201209:20201231,
		20210101:20210131,20210201:20210209,20210322:20210331,20210401:20210405,20210601:20210631,
		20210701:20210731,20210801:20210831,20211209:20211231,20220101:20220131,20220201,
		20220411:20220422,20220603:20220631,20220701:20220731,20220801:20220831
)

	SchulferienBW = as.POSIXct(as.character(schulferien_bw_14_22), format="%Y%m%d")
	SemesterferienMA = as.POSIXct(as.character(semesterferien_UniMA), format="%Y%m%d")
	#https://www.uni-muenster.de/studium/orga/termine_archiv.html
	#Termine Uni Mannheim und DHBW Mannheim. Uni Mannheim bereits angefragt

	datensatz$FeiertagBW 		= ifelse(datensatz$Datum %in% Feiertage_bw,1,0)
	datensatz$FeiertagRP 		= ifelse(datensatz$Datum %in% Feiertage_rp,1,0)
	datensatz$SchulferienBW 	= ifelse(datensatz$Datum %in% SchulferienBW,1,0)
	datensatz$SemesterferionUM 	= ifelse(datensatz$Datum %in% SemesterferienMA,1,0)

	summary(datensatz)
	
###
#Wetterdaten einladen
###


	niederschlag 	= read_csv("Daten/Wetter_Daten/data_OBS_DEU_PT1H_RR.csv")
	lufttemperatur 	= read_csv("Daten/Wetter_Daten/data_OBS_DEU_PT1H_T2M.csv")
	wind 			= read_csv("Daten/Wetter_Daten/data_OBS_DEU_PT1H_F.csv")
	relativefeuchte 	= read_csv("Daten/Wetter_Daten/data_OBS_DEU_PT1H_RF.csv")
	sonne 		= read_csv("Daten/Wetter_Daten/data_OBS_DEU_PT1H_SD.csv")
	bedeckung		= read_csv("Daten/Wetter_Daten/data_OBS_DEU_PT1H_N.csv")

	summary(niederschlag)
	summary(lufttemperatur)
	summary(wind)
	summary(relativefeuchte)
	summary(sonne)
	summary(bedeckung)
	
	#Niederschlag---

	niederschlag$Zeit_neu	= as.POSIXct(niederschlag$Zeitstempel, format="%Y-%s-%b %H:%M:%S")
	niederschlag$Datum	= as.POSIXct(niederschlag$Zeit_neu, format="%Y-%m-%d")			#Nur_Datum
	niederschlag$Jahr		= format(as.Date(niederschlag$Zeit_neu, format="%Y-%m-%d"),"%Y") 	#Jahr
	niederschlag$Monat	= format(as.Date(niederschlag$Zeit_neu, format="%Y-%m-%d"),"%m") 	#Monat
	niederschlag$Tag		= format(as.Date(niederschlag$Zeit_neu, format="%Y-%m-%d"),"%d") 	#Tag im Monat
	
	niederschlag = niederschlag %>%
		subset(Jahr>2013)

	niederschlag$Stunde	= format(niederschlag$Zeit_neu, format = "%H")	#Nur Stunde
	summary(niederschlag$Zeit_neu)

	names(niederschlag)
	niederschlag <- niederschlag %>%
     		dplyr::select(Wert, Qualitaet_Niveau, Jahr, Monat, Tag, Stunde)
	names(niederschlag)[1]="WertRR"
	names(niederschlag)[2]="QualitaetRR"

	datensatz = merge(x = datensatz,y = niederschlag,
		by = c("Jahr","Monat","Tag","Stunde"),
		all = TRUE)

	#Temperatur---

	lufttemperatur$Zeit_neu	= as.POSIXct(lufttemperatur$Zeitstempel, format="%Y-%s-%b %H:%M:%S")
	lufttemperatur$Datum	= as.POSIXct(lufttemperatur$Zeit_neu, format="%Y-%m-%d")			#Nur_Datum
	lufttemperatur$Jahr	= format(as.Date(lufttemperatur$Zeit_neu, format="%Y-%m-%d"),"%Y") 	#Jahr
	lufttemperatur$Monat	= format(as.Date(lufttemperatur$Zeit_neu, format="%Y-%m-%d"),"%m") 	#Monat
	lufttemperatur$Tag	= format(as.Date(lufttemperatur$Zeit_neu, format="%Y-%m-%d"),"%d") 	#Tag im Monat
	lufttemperatur$Stunde	= format(lufttemperatur$Zeit_neu, format = "%H")	#Nur Stunde

	lufttemperatur = lufttemperatur %>%
		subset(Jahr>2013)

	names(lufttemperatur)
	lufttemperatur <- lufttemperatur %>%
     		dplyr::select(Wert, Qualitaet_Niveau, Jahr, Monat, Tag, Stunde)
	names(lufttemperatur)[1]="WertT2M"
	names(lufttemperatur)[2]="QualitaetT2M"

	datensatz = merge(x = datensatz,y = lufttemperatur,
		by = c("Jahr","Monat","Tag","Stunde"),
		all = TRUE)

	#Wind---

	wind$Zeit_neu	= as.POSIXct(wind$Zeitstempel, format="%Y-%s-%b %H:%M:%S")
	wind$Datum		= as.POSIXct(wind$Zeit_neu, format="%Y-%m-%d")			#Nur_Datum
	wind$Jahr		= format(as.Date(wind$Zeit_neu, format="%Y-%m-%d"),"%Y") 	#Jahr
	wind$Monat		= format(as.Date(wind$Zeit_neu, format="%Y-%m-%d"),"%m") 	#Monat
	wind$Tag		= format(as.Date(wind$Zeit_neu, format="%Y-%m-%d"),"%d") 	#Tag im Monat
	wind$Stunde		= format(wind$Zeit_neu, format = "%H")	#Nur Stunde

	wind = wind %>%
		subset(Jahr>2013)

	wind  <- wind  %>%
     		dplyr::select(Wert, Qualitaet_Niveau, Jahr, Monat, Tag, Stunde)
	names(wind)[1]="WertF"
	names(wind)[2]="QualitaetF"

	datensatz = merge(x = datensatz,y = wind,
		by = c("Jahr","Monat","Tag","Stunde"),
		all = TRUE)

	#Feuchte---

	relativefeuchte$Zeit_neu	= as.POSIXct(relativefeuchte$Zeitstempel, format="%Y-%s-%b %H:%M:%S")
	relativefeuchte$Datum		= as.POSIXct(relativefeuchte$Zeit_neu, format="%Y-%m-%d")			#Nur_Datum
	relativefeuchte$Jahr		= format(as.Date(relativefeuchte$Zeit_neu, format="%Y-%m-%d"),"%Y") 	#Jahr
	relativefeuchte$Monat		= format(as.Date(relativefeuchte$Zeit_neu, format="%Y-%m-%d"),"%m") 	#Monat
	relativefeuchte$Tag		= format(as.Date(relativefeuchte$Zeit_neu, format="%Y-%m-%d"),"%d") 	#Tag im Monat
	relativefeuchte$Stunde		= format(relativefeuchte$Zeit_neu, format = "%H")	#Nur Stunde

	relativefeuchte = relativefeuchte %>%
		subset(Jahr>2013)

	relativefeuchte <- relativefeuchte  %>%
     		dplyr::select(Wert, Qualitaet_Niveau, Jahr, Monat, Tag, Stunde)
	names(relativefeuchte)[1]="WertRF"
	names(relativefeuchte)[2]="QualitaetRF"

	datensatz = merge(x = datensatz,y = relativefeuchte,
		by = c("Jahr","Monat","Tag","Stunde"),
		all = TRUE)

	#Sonne---

	sonne$Zeit_neu	= as.POSIXct(sonne$Zeitstempel, format="%Y-%s-%b %H:%M:%S")
	sonne$Datum		= as.POSIXct(sonne$Zeit_neu, format="%Y-%m-%d")			#Nur_Datum
	sonne$Jahr		= format(as.Date(sonne$Zeit_neu, format="%Y-%m-%d"),"%Y") 	#Jahr
	sonne$Monat		= format(as.Date(sonne$Zeit_neu, format="%Y-%m-%d"),"%m") 	#Monat
	sonne$Tag		= format(as.Date(sonne$Zeit_neu, format="%Y-%m-%d"),"%d") 	#Tag im Monat
	sonne$Stunde	= format(sonne$Zeit_neu, format = "%H")	#Nur Stunde

	sonne = sonne %>%
		subset(Jahr>2013)

	sonne <- sonne %>%
     		dplyr::select(Wert, Qualitaet_Niveau, Jahr, Monat, Tag, Stunde)
	names(sonne)[1]="WertSD"
	names(sonne)[2]="QualitaetSD"

	datensatz = merge(x = datensatz,y = sonne,
		by = c("Jahr","Monat","Tag","Stunde"),
		all = TRUE)

	#Bedeckungsgrad---

	bedeckung$Zeit_neu	= as.POSIXct(bedeckung$Zeitstempel, format="%Y-%s-%b %H:%M:%S")
	bedeckung$Datum		= as.POSIXct(bedeckung$Zeit_neu, format="%Y-%m-%d")			#Nur_Datum
	bedeckung$Jahr		= format(as.Date(bedeckung$Zeit_neu, format="%Y-%m-%d"),"%Y") 	#Jahr
	bedeckung$Monat		= format(as.Date(bedeckung$Zeit_neu, format="%Y-%m-%d"),"%m") 	#Monat
	bedeckung$Tag		= format(as.Date(bedeckung$Zeit_neu, format="%Y-%m-%d"),"%d") 	#Tag im Monat
	bedeckung$Stunde		= format(bedeckung$Zeit_neu, format = "%H")	#Nur Stunde

	bedeckung = bedeckung %>%
		subset(Jahr>2013)

	bedeckung <- bedeckung %>%
     		dplyr::select(Wert, Qualitaet_Niveau, Jahr, Monat, Tag, Stunde)
	names(bedeckung)[1]="WertN"
	names(bedeckung)[2]="QualitaetN"

	datensatz = merge(x = datensatz,y = bedeckung,
		by = c("Jahr","Monat","Tag","Stunde"),
		all = TRUE)

	table(datensatz$WertN)
	datensatz = datensatz %>%
		mutate(WertN = ifelse(WertN == -1,8,WertN))

	table(datensatz$QualitaetRR)
	table(datensatz$QualitaetT2M)
	table(datensatz$QualitaetF)
	table(datensatz$QualitaetRF)
	table(datensatz$QualitaetSD)
	table(datensatz$QualitaetN)

	table(datensatz$WertRR)
	table(datensatz$WertT2M)
	table(datensatz$WertF)
	table(datensatz$WertRF)
	table(datensatz$WertSD)
	table(datensatz$WertN)

	length(datensatz$WertN)
	length(datensatz$WertSD)
	
	names(datensatz)

#Daten prüfen

	#ggplot(data=datensatz, aes(x=Monat,y=as.numeric(WertT2M)))+geom_point()

	#Noch zu viele Na's vorhanden, woran liegt das?

	min(datensatz$Datum)
	#Wie man sieht, habe ich versehentlich Wetterdaten von 1948 mit in den Datensatz aufgenommen. Das ändern wir.

	datensatz = datensatz %>%
		subset(is.na(Jahr)==FALSE)

	#ggplot(data=datensatz, aes(x=Monat,y=as.numeric(WertT2M)))+geom_point()
	min(datensatz$Datum)
	#Frühestes Datum ist jetzt aus dem März 2014

	names(datensatz)
	#ggplot(data=datensatz, aes(x=as.numeric(WertT2M),y=Zaehlstand))+geom_point()
	#ggplot(data=datensatz, aes(x=Stunde,y=Zaehlstand))+geom_point()

	summary(datensatz)

	#nrow(datensatz)
	#datensatz <- datensatz %>%
	#	drop_na(Zaehlstand)
	#nrow(datensatz_omit)

	nrow(datensatz)
	datensatz <- na.omit(datensatz)
	nrow(datensatz_omit)

#Corona Variabeln

	#Corona wurde am 11. März 2020 zur Pandemie erklärt
	corona = as.POSIXct(as.character(20200311), format="%Y%m%d")

	datensatz$Corona = ifelse(datensatz$Datum < corona,0,1)

	#Lockdowns
	#Erster Lockdown bundesweit 22. März bis zum 6. Mai 2020. Danach Landkreise Abweichungen
	#https://www.mannheim.de/de/presse/58-aktuelle-meldung-zu-corona-02-05-2020
	#Auch in BW keine Verlängerung
	#Zweiter Lockdown Light 2. November 2020 bis 15. Januar 2021
	#Bundesnotbremse vom 24. April bis zum 30. Juni 2021

	kontaktbeschr=c(20200322:20200331,20200401:20200431,20200501:20200506,
	20201102:20201131,20201201:20201231,20210101:20210115,20210424:20210431,
	20210501:20210531,20210601:20210630)

	Kontaktbeschr = as.POSIXct(as.character(kontaktbeschr), format="%Y%m%d")
	datensatz$Kontaktbeschr	= ifelse(datensatz$Datum %in% Kontaktbeschr,1,0)

	datensatz$TagesAusbr = ifelse(datensatz$Datum < corona,0, difftime(datensatz$Datum ,corona , units = c("days")))

#Strassen Arten

	#Brücken Dummy Variable

# Station an Renszstraße ist von Dez 2020 bis Dez 2021 ausgefallen. Diese Daten müssen wir trennen

	datensatz_ohneRS = datensatz %>%
		filter(Standort != "RenzstraÃŸe")

	datensatz_nurRS = datensatz %>%
		filter(as.numeric(Jahr)<2020 & Standort == "RenzstraÃŸe")

	datensatz_nurRS2020 = datensatz %>%
		filter(Jahr=="2020" & as.numeric(Monat)<12 & Standort == "RenzstraÃŸe")

	datensatz_nurRS2022 = datensatz %>%
		filter(Jahr=="2022" & Standort == "RenzstraÃŸe")

	datensatz_final = rbind(datensatz_ohneRS,datensatz_nurRS)
	datensatz_final = rbind(datensatz_final,datensatz_nurRS2020)
	datensatz_final = rbind(datensatz_final,datensatz_nurRS2022)
	nrow(datensatz)-nrow(datensatz_final)

	datensatz_RS202012 = datensatz %>%
		filter(Jahr=="2020" & Monat=="12" & Standort == "RenzstraÃŸe")
	datensatz_RS2021 = datensatz %>%
		filter(Jahr=="2021" & Standort == "RenzstraÃŸe")

	datensatz_RSAusfall = rbind(datensatz_RS202012,datensatz_RS2021)
	nrow(datensatz_RSAusfall)

# Speichere Daten

	names(datensatz)

	write.csv(datensatz,file="datensatz.csv")
	save(datensatz,file="datensatz.rdata")

	write.csv(datensatz_RSAusfall,file="datensatz_RSAusfall.csv")
	save(datensatz_RSAusfall,file="datensatz_RSAusfall.rdata")