#' --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#' 
#' REPORT BUSINESS DATA ANALYTICS
#' 
#' TITOLO: ITALIA 4.0
#' 
#' CREATOR: NICOLA SALTARELLI
#' 
#' SCRIPT: Questo script è stato sviluppato per generare grafici a barre che rappresentano la percentuale di utilizzo 
#' delle tecnologie dell'Industria 4.0 da parte di aziende europee. 
#' I grafici mostrano come le aziende europee si stanno adattando alle nuove tecnologie dell'Industria 4.0.
#' 
#' --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

## 1.RIMOZIONE OGGETTI E LETTURA DELLE LIBRERIE

rm(list=ls())
library(tidyverse)
library(readr)
library(ggplot2)
library(maps)

## 2. DATA COLLECTION: lettura dei dataset scaricati

setwd("/Users/nicolasaltarelli/Desktop/Nicola_Saltarelli/Poliba/Esami_a_scelta/Business_Data_Analytics/Report/Dataset")
AIE <- read_csv("AI_Eurostat.csv")
IOTE <- read.csv("IOT_Eurostat.csv")
RobotE <- read_csv("Stampa3DRoboticaEurostat.csv")

## 3-4 DATA CLEANING & DATA VALIDATION: filtraggio dataset e validaizone dei dati

# Per filtrare i dataset, vengono selezionate le colonne relative al paese, all'anno e alla percentuale di utilizzo. 
# Il nome completo di ciascun paese viene utilizzato al posto della sigla e le righe vuote vengono eliminate.

AIE <- AIE[, c("geo", "TIME_PERIOD", "OBS_VALUE")]
names(AIE)[1:3] <- c("Country", "Year", "Value")
AIE$Country <- ifelse(AIE$Country == "BE" , "Belgium", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "BG" , "Bulgaria", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "CZ" , "Czech Republic", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "DK" , "Denmark", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "DE" , "Germany", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "EE" , "Estonia", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "IE" , "Ireland", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "EL" , "Greece", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "ES" , "Spain", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "FR" , "France", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "HR" , "Croatia", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "IT" , "Italy", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "CY" , "Cyprus", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "LV" , "Latvia", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "LT" , "Lithuania", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "LU" , "Luxembourg", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "HU" , "Hungary", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "MT" , "Malta", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "NL" , "Netherlands", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "AT" , "Austria", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "PL" , "Poland", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "PT" , "Portugal", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "RO" , "Romania", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "SI" , "Slovenia", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "SK" , "Slovakia", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "FI" , "Finland", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "SE" , "Sweden", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "NO" , "Norway", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "UK" , "UK", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "ME" , "Montenegro", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "MK" , "North Macedonia", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "AL" , "Albania", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "RS" , "Serbia", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "TR" , "Turkey", AIE$Country)
AIE$Country <- ifelse(AIE$Country == "BA" , "Bosnia and Herzegovina", AIE$Country)
AIE <- slice(AIE, -10)
AIE <- slice(AIE, -13)
AIE <- AIE %>%
  arrange(desc(Value))
AIE <- na.omit(AIE)
AIE$Year <- as.character(AIE$Year)


IOTE <- IOTE[, c("geo", "TIME_PERIOD", "OBS_VALUE")]
names(IOTE)[1:3] <- c("Country", "Year", "Value")
IOTE$Country <- ifelse(IOTE$Country == "BE" , "Belgium", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "BG" , "Bulgaria", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "CZ" , "Czech Republic", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "DK" , "Denmark", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "DE" , "Germany", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "EE" , "Estonia", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "IE" , "Ireland", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "EL" , "Greece", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "ES" , "Spain", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "FR" , "France", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "HR" , "Croatia", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "IT" , "Italy", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "CY" , "Cyprus", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "LV" , "Latvia", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "LT" , "Lithuania", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "LU" , "Luxembourg", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "HU" , "Hungary", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "MT" , "Malta", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "NL" , "Netherlands", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "AT" , "Austria", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "PL" , "Poland", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "PT" , "Portugal", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "RO" , "Romania", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "SI" , "Slovenia", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "SK" , "Slovakia", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "FI" , "Finland", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "SE" , "Sweden", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "NO" , "Norway", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "UK" , "UK", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "ME" , "Montenegro", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "MK" , "North Macedonia", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "AL" , "Albania", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "RS" , "Serbia", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "TR" , "Turkey", IOTE$Country)
IOTE$Country <- ifelse(IOTE$Country == "BA" , "Bosnia and Herzegovina", IOTE$Country)
IOTE <- slice(IOTE, -14)
IOTE <- slice(IOTE, -10)
IOTE <- IOTE %>%
  arrange(desc(Value))
IOTE <- na.omit(IOTE)
IOTE$Year <- as.character(IOTE$Year)


RobotE <- RobotE[, c("geo", "TIME_PERIOD", "OBS_VALUE")]
names(RobotE)[1:3] <- c("Country", "Year", "Value")
RobotE$Country <- ifelse(RobotE$Country == "BE" , "Belgium", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "BG" , "Bulgaria", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "CZ" , "Czech Republic", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "DK" , "Denmark", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "DE" , "Germany", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "EE" , "Estonia", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "IE" , "Ireland", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "EL" , "Greece", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "ES" , "Spain", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "FR" , "France", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "HR" , "Croatia", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "IT" , "Italy", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "CY" , "Cyprus", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "LV" , "Latvia", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "LT" , "Lithuania", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "LU" , "Luxembourg", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "HU" , "Hungary", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "MT" , "Malta", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "NL" , "Netherlands", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "AT" , "Austria", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "PL" , "Poland", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "PT" , "Portugal", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "RO" , "Romania", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "SI" , "Slovenia", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "SK" , "Slovakia", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "FI" , "Finland", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "SE" , "Sweden", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "NO" , "Norway", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "UK" , "UK", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "ME" , "Montenegro", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "MK" , "North Macedonia", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "AL" , "Albania", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "RS" , "Serbia", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "TR" , "Turkey", RobotE$Country)
RobotE$Country <- ifelse(RobotE$Country == "BA" , "Bosnia and Herzegovina", RobotE$Country)
RobotE <- slice(RobotE, -24, -25, -26, -37, -38, -39, -40, -41)
RobotE <- slice(RobotE, -27)
RobotE <- RobotE %>%
  arrange(desc(Value))
RobotE <- na.omit(RobotE)
RobotE$Year <- as.character(RobotE$Year)
Robot <- RobotE %>%
  filter(Year == "2022")

## 5. DATA VISUALIZATION: grafico Robot

ggplot(data = Robot, aes(x = reorder(Country, Value), y = Value, fill = Value)) + 
  geom_col() +
  labs(x = "Nazioni", y = "Percentuale di imprese") +
  ggtitle("Utilizzo di robot industriali o di servizio da parte di imprese con 10 o più persone occupate") +
  scale_y_continuous(breaks = seq(0, 70, by = 5)) + 
  theme(panel.grid = element_blank(), 
        panel.background =  element_rect(fill = "white"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  scale_fill_gradient(low = "#B3E5FC", high = "#0D47A1", limits = c(0, max(Robot$Value))) +
  geom_text(aes(label = Value), size = 4, vjust = 0.5, hjust = -0.2)

## 5. DATA VISUALIZATION: grafico IOT

ggplot(data = IOTE, aes(x = reorder(Country, Value), y = Value, fill = Value)) + 
  geom_col(position = position_dodge(width = 0.8)) +
  labs(x = "Nazioni", y = "Percentuale di imprese") +
  ggtitle("Utilizzo di IOT da parte di imprese con 10 o più persone occupate") +
  scale_y_continuous(breaks = seq(0, 70, by = 5)) + 
  theme(panel.grid = element_blank(), 
        panel.background =  element_rect(fill = "white"),
        legend.position = "none", 
        plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", limits = c(0, max(IOTE$Value))) +
  geom_text(aes(label = Value), size = 4, vjust = 0.5, hjust = -0.2) 

## 5. DATA VISUALIZATION: grafico AI

ggplot(data = AIE, aes(x = reorder(Country, Value), y = Value, fill = Value)) + 
  geom_col(position = position_dodge(width = 0.8)) +
  labs(x = "Nazioni", y = "Percentuale di imprese") +
  ggtitle("Utilizzo di Intelligenza artificiale da parte di imprese con 10 o più persone occupate") +
  scale_y_continuous(breaks = seq(0, 70, by = 5)) + 
  theme(panel.grid = element_blank(), 
        panel.background =  element_rect(fill = "white"),
        legend.position = "none", 
        plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  scale_fill_gradient(low = "#FFCDD2", high = "#D32F2F", limits = c(0, max(AIE$Value))) +
  geom_text(aes(label = Value), size = 4, vjust = 0.5, hjust = -0.2)  


## 5. DATA VISUALIZATION: creazione mappe

Robot22 <- Robot[, c("Country", "Value")]
names(Robot22)[1:2] <- c("region", "Value")

AI21 <- AIE[, c("Country", "Value")]
names(AI21)[1:2] <- c("region", "Value")

IOT21 <- IOTE[, c("Country", "Value")]
names(IOT21)[1:2] <- c("region", "Value")

# Scarica le coordinate dei confini dei paesi europei
world <- map_data("world")

dataRobot <- merge(world, Robot22, by = "region", all = TRUE)
dataAI <- merge(world, AI21, by = "region", all = TRUE)
dataIOT <- merge(world, IOT21, by = "region", all = TRUE)

ggplot(dataRobot, aes(x = long, y = lat, group = group)) +
  geom_map(aes(fill = Value, map_id = region), map = map_data("world"), 
           color = "black", size = 0.2) +
  coord_cartesian(xlim = c(-12, 40), ylim = c(35, 70)) +
  theme(panel.grid = element_blank(), 
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "#B3E5FC", high = "#0D47A1", name = "Robot (%)", limits = c(0, max(dataRobot$Value)))

ggplot(dataAI, aes(x = long, y = lat, group = group)) +
  geom_map(aes(fill = Value, map_id = region), map = map_data("world"), 
           color = "black", size = 0.2) +
  coord_cartesian(xlim = c(-12, 40), ylim = c(35, 70)) +
  theme(panel.grid = element_blank(), 
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "#FFCDD2", high = "#D32F2F", name = "AI (%)", limits = c(0, max(dataAI$Value)))

ggplot(dataIOT, aes(x = long, y = lat, group = group)) +
  geom_map(aes(fill = Value, map_id = region), map = map_data("world"), 
           color = "black", size = 0.2) +
  coord_cartesian(xlim = c(-12, 40), ylim = c(35, 70)) +
  theme(panel.grid = element_blank(), 
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "IOT (%)", limits = c(0, max(dataIOT$Value)))











