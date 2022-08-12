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

	library(caret)

	# Split the data into training and test set
	set.seed(123)
	training.samples <- datensatz$Zaehlstand %>%
 		createDataPartition(p = 0.9, list = FALSE)
	train.data  <- datensatz[training.samples, ]
	test.data <- datensatz[-training.samples, ]

names(datensatz)

ggplot(test.data,aes(x = uniMA_dist, y = Zaehlstand)) +
	geom_point(size=0.05) +
	ggtitle("Fahrradfahrer in Entfernung zur Universität") +
	xlab("Entfernung zur Universität Mannheim in m") +
	ylab("Fahrradauufkommen") +
	theme_bw() + 
  	stat_smooth(method = "lm", formula= y ~ x, aes(color="linear"), size = 1.5) +
	stat_smooth(method = "lm", formula = y ~ x + I(x^2), aes(color="quadratisch"), size = 1.5) +
	stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), aes(color="kubisch"), size = 1.5) +
	scale_colour_manual(name="legend", values=c("green", "red", "blue"))





