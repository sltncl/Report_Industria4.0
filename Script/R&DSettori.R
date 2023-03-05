#' --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#' 
#' REPORT BUSINESS DATA ANALYTICS
#' 
#' TITOLO: ITALIA 4.0
#' 
#' CREATOR: NICOLA SALTARELLI
#' 
#' SCRIPT: Questo script è stato progettato per creare un grafico a colonne che rappresenta l'evoluzione degli investimenti 
#' in ricerca e sviluppo da parte dei settori italiani che presentano i livelli di investimento più elevati.
#' 
#' --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

## 1.RIMOZIONE OGGETTI E LETTURA DELLE LIBRERIE

rm(list=ls())
library(tidyverse)
library(readr)
library(ggplot2)

## 2. DATA COLLECTION: lettura dei dataset scaricati

setwd("/Users/nicolasaltarelli/Desktop/Nicola_Saltarelli/Poliba/Esami_a_scelta/Business_Data_Analytics/Report/Dati")
scoreboard22 <- readxl::read_xlsx("SB2022_EU1000.xlsx")
scoreboard21 <- readxl::read_xlsx("SB2021_EU1000.xlsx")
scoreboard20 <- readxl::read_xlsx("SB2020_EU1000.xlsx")
scoreboard19 <- readxl::read_xlsx("SB2019_EU1000.xlsx")
scoreboard18 <- readxl::read_xlsx("SB2018_EU1000.xlsx")
scoreboard17 <- readxl::read_xlsx("SB2017_EU1000.xlsx")
scoreboard16 <- readxl::read_xlsx("SB2016_EU1000.xlsx")
scoreboard15 <- readxl::read_xlsx("SB2015_EU1000.xlsx")
scoreboard14 <- readxl::read_xlsx("SB2014_EU1000.xlsx")
scoreboard13 <- readxl::read_xlsx("SB2013_EU1000.xlsx")
scoreboard12 <- readxl::read_xls("SB2012_EU1000.xls")

## 3. DATA CLEANING: filtraggio dataset

# I dataset vengono filtrati selezionando le colonne relative al settore, al paese e alla quota di investimento in R&D

scoreboard22_EU <- scoreboard22[, c("Industry -ICB3 sector name", "Country", "R&D 2021 (€million)")] %>%
  arrange(Country)
names(scoreboard22_EU)[1:3] <- c("Industry", "Country", "R&D 2021 (€million)")

scoreboard21_EU <- scoreboard21[, c("Industry -ICB3 sector name", "Country", "R&D 2020 (€million)")] %>%
  arrange(Country)
names(scoreboard21_EU)[1:3] <- c("Industry", "Country", "R&D 2020 (€million)")

scoreboard20_EU <- scoreboard20[, c("Industry", "Country", "R&D 2019 (€million)")] %>%
  arrange(Country)

scoreboard19_EU <- scoreboard19[, c("Industry", "Country", "R&D 2018/19 (€million)")] %>%
  arrange(Country)
names(scoreboard19_EU)[1:3] <- c("Industry", "Country", "R&D 2018 (€million)")

scoreboard18_EU <- scoreboard18[, c("Industry", "Country", "R&D 2017/18 (€mn)")] %>%
  arrange(Country)
names(scoreboard18_EU)[1:3] <- c("Industry", "Country", "R&D 2017 (€million)")

scoreboard17_EU <- scoreboard17[, c("Industry", "Country", "R&D 2016/17 (€million)", "Op.profits (€million)" )] %>%
  arrange(Country)
names(scoreboard17_EU)[1:3] <- c("Industry", "Country", "R&D 2016 (€million)")

scoreboard16_EU <- scoreboard16[, c("...4", "...3", "...5", "...16")] 
names(scoreboard16_EU)[1:3] <- c("Industry", "Country", "R&D 2015 (€million)")
scoreboard16_EU <- scoreboard16_EU %>%
  arrange(Country)

scoreboard15_EU <- scoreboard15[, c("...4", "...3", "...5", "...16")] 
names(scoreboard15_EU)[1:3] <- c("Industry", "Country", "R&D 2014 (€million)")
scoreboard15_EU <- scoreboard15_EU %>%
  arrange(Country)

scoreboard14_EU <- scoreboard14[, c("Industrial sector (ICB-3D)", "Country", "R&D 2013 (€million)")] %>%
  arrange(Country)
names(scoreboard14_EU)[1:3] <- c("Industry", "Country", "R&D 2013 (€million)")

scoreboard13_EU <- scoreboard13[, c("...4", "...3", "...5")] 
names(scoreboard13_EU)[1:3] <- c("Industry", "Country", "R&D 2012 (€million)")
scoreboard13_EU <- scoreboard13_EU %>%
  arrange(Country)

scoreboard12_EU <- scoreboard12[, c("...4", "...3", "...5")] 
names(scoreboard12_EU)[1:3] <- c("Industry", "Country", "R&D 2011 (€million)")
scoreboard12_EU <- scoreboard12_EU %>%
  arrange(Country)

## 4. DATA VALIDATION 

# Per ogni anno, i dati vengono ulteriormente filtrati selezionando solo le colonne pertinenti ai settori italiani e 
# alle loro quote di investimento. Successivamente, vengono sommati tutti gli investimenti effettuati da ogni settore e, 
# al fine di permettere una comparazione tra i diversi dataset, si cerca di utilizzare la stessa denominazione per i settori 
# in tutti gli anni presi in considerazione.
# Infatti, si è notato che alcuni dataset presentano una differenza nella denominazione dei settori "Aerospace & Defence" 
# e "Gas, water & multiutilities". Per questo motivo, tali settori vengono rinominati in modo da uniformare la denominazione 
# nei diversi dataset.


# anno 2022

SBA22 <- scoreboard22_EU %>%
  filter(Country == "Italy")
SBA22 <- SBA22[, c("Industry", "R&D 2021 (€million)")]
colnames(SBA22)[2] <- "Anno_2022"
SBA22 <- SBA22 %>%
  mutate(Industry = toupper(Industry))
SBA22 <- aggregate(Anno_2022 ~ Industry, data = SBA22, sum)
SBA22$Industry <- ifelse(SBA22$Industry == "AEROSPACE & DEFENSE" , "AEROSPACE & DEFENCE", SBA22$Industry)
SBA22$Industry <- ifelse(SBA22$Industry == "GAS, WATER & MULTI-UTILITIES" , "GAS, WATER & MULTIUTILITIES", SBA22$Industry)

# anno 2021

SBA21 <- scoreboard21_EU %>%
  filter(Country == "Italy")
SBA21 <- SBA21[, c("Industry", "R&D 2020 (€million)")]
colnames(SBA21)[2] <- "Anno_2021"
SBA21 <- SBA21 %>%
  mutate(Industry = toupper(Industry))
SBA21 <- aggregate(Anno_2021 ~ Industry, data = SBA21, sum)
SBA21$Industry <- ifelse(SBA21$Industry == "AEROSPACE & DEFENSE" , "AEROSPACE & DEFENCE", SBA21$Industry)
SBA21$Industry <- ifelse(SBA21$Industry == "GAS, WATER & MULTI-UTILITIES" , "GAS, WATER & MULTIUTILITIES", SBA21$Industry)

# anno 2020

SBA20 <- scoreboard20_EU %>%
  filter(Country == "Italy")
SBA20 <- SBA20[, c("Industry", "R&D 2019 (€million)")]
colnames(SBA20)[2] <- "Anno_2020"
SBA20 <- SBA20 %>%
  mutate(Industry = toupper(Industry))
SBA20 <- aggregate(Anno_2020 ~ Industry, data = SBA20, sum)
SBA20$Industry <- ifelse(SBA20$Industry == "AEROSPACE & DEFENSE" , "AEROSPACE & DEFENCE", SBA20$Industry)
SBA20$Industry <- ifelse(SBA20$Industry == "GAS, WATER & MULTI-UTILITIES" , "GAS, WATER & MULTIUTILITIES", SBA20$Industry)

# anno 2019

SBA19 <- scoreboard19_EU %>%
  filter(Country == "Italy")
SBA19 <- SBA19[, c("Industry", "R&D 2018 (€million)")]
colnames(SBA19)[2] <- "Anno_2019"
SBA19 <- SBA19 %>%
  mutate(Industry = toupper(Industry))
SBA19 <- aggregate(Anno_2019 ~ Industry, data = SBA19, sum)
SBA19$Industry <- ifelse(SBA19$Industry == "AEROSPACE & DEFENSE" , "AEROSPACE & DEFENCE", SBA19$Industry)
SBA19$Industry <- ifelse(SBA19$Industry == "GAS, WATER & MULTI-UTILITIES" , "GAS, WATER & MULTIUTILITIES", SBA19$Industry)

# anno 2018

SBA18 <- scoreboard18_EU %>%
  filter(Country == "Italy")
SBA18 <- SBA18[, c("Industry", "R&D 2017 (€million)")]
colnames(SBA18)[2] <- "Anno_2018"
SBA18 <- SBA18 %>%
  mutate(Industry = toupper(Industry))
SBA18 <- aggregate(Anno_2018 ~ Industry, data = SBA18, sum)
SBA18$Industry <- ifelse(SBA18$Industry == "AEROSPACE & DEFENSE" , "AEROSPACE & DEFENCE", SBA18$Industry)
SBA18$Industry <- ifelse(SBA18$Industry == "GAS, WATER & MULTI-UTILITIES" , "GAS, WATER & MULTIUTILITIES", SBA18$Industry)

# anno 2017

SBA17 <- scoreboard17_EU %>%
  filter(Country == "Italy")
SBA17 <- SBA17[, c("Industry", "R&D 2016 (€million)")]
colnames(SBA17)[2] <- "Anno_2017"
SBA17 <- SBA17 %>%
  mutate(Industry = toupper(Industry))
SBA17 <- aggregate(Anno_2017 ~ Industry, data = SBA17, sum)
SBA17$Industry <- ifelse(SBA17$Industry == "AEROSPACE & DEFENSE" , "AEROSPACE & DEFENCE", SBA17$Industry)
SBA17$Industry <- ifelse(SBA17$Industry == "GAS, WATER & MULTI-UTILITIES" , "GAS, WATER & MULTIUTILITIES", SBA17$Industry)

# anno 2016

SBA16 <- scoreboard16_EU %>%
  filter(Country == "Italy")
SBA16 <- SBA16[, c("Industry", "R&D 2015 (€million)")]
colnames(SBA16)[2] <- "Anno_2016"
SBA16 <- SBA16 %>%
  mutate(Industry = toupper(Industry))
SBA16 <- aggregate(Anno_2016 ~ Industry, data = SBA16, sum)
SBA16$Industry <- ifelse(SBA16$Industry == "AEROSPACE & DEFENSE" , "AEROSPACE & DEFENCE", SBA16$Industry)
SBA16$Industry <- ifelse(SBA16$Industry == "GAS, WATER & MULTI-UTILITIES" , "GAS, WATER & MULTIUTILITIES", SBA16$Industry)

# anno 2015

SBA15 <- scoreboard15_EU %>%
  filter(Country == "Italy")
SBA15 <- SBA15[, c("Industry", "R&D 2014 (€million)")]
colnames(SBA15)[2] <- "Anno_2015"
SBA15 <- SBA15 %>%
  mutate(Industry = toupper(Industry))
SBA15 <- aggregate(Anno_2015 ~ Industry, data = SBA15, sum)
SBA15$Industry <- ifelse(SBA15$Industry == "AEROSPACE & DEFENSE" , "AEROSPACE & DEFENCE", SBA15$Industry)
SBA15$Industry <- ifelse(SBA15$Industry == "GAS, WATER & MULTI-UTILITIES" , "GAS, WATER & MULTIUTILITIES", SBA15$Industry)

# anno 2014

SBA14 <- scoreboard14_EU %>%
  filter(Country == "Italy")
SBA14 <- SBA14[, c("Industry", "R&D 2013 (€million)")]
colnames(SBA14)[2] <- "Anno_2014"
SBA14 <- SBA14 %>%
  mutate(Industry = toupper(Industry))
SBA14 <- aggregate(Anno_2014 ~ Industry, data = SBA14, sum)
SBA14$Industry <- ifelse(SBA14$Industry == "AEROSPACE & DEFENSE" , "AEROSPACE & DEFENCE", SBA14$Industry)
SBA14$Industry <- ifelse(SBA14$Industry == "GAS, WATER & MULTI-UTILITIES" , "GAS, WATER & MULTIUTILITIES", SBA14$Industry)

# anno 2013

scoreboard13_EU$`R&D 2012 (€million)` <- as.integer(scoreboard13_EU$`R&D 2012 (€million)`)
SBA13 <- scoreboard13_EU %>%
  filter(Country == "Italy")
SBA13 <- subset(SBA13, "R&D 2012 (€million)" !=0)
SBA13 <- SBA13[, c("Industry", "R&D 2012 (€million)")]
colnames(SBA13)[2] <- "Anno_2013"
SBA13 <- SBA13 %>%
  mutate(Industry = toupper(Industry))
SBA13 <- aggregate(Anno_2013 ~ Industry, data = SBA13, sum)
SBA13$Industry <- ifelse(SBA13$Industry == "AEROSPACE & DEFENSE" , "AEROSPACE & DEFENCE", SBA13$Industry)
SBA13$Industry <- ifelse(SBA13$Industry == "GAS, WATER & MULTI-UTILITIES" , "GAS, WATER & MULTIUTILITIES", SBA13$Industry)

# anno 2012

SBA12 <- scoreboard12_EU %>%
  filter(Country == "Italy")
SBA12 <- SBA12[, c("Industry", "R&D 2011 (€million)")]
colnames(SBA12)[2] <- "Anno_2012"
SBA12 <- SBA12 %>%
  mutate(Industry = toupper(Industry))
SBA12 <- aggregate(Anno_2012 ~ Industry, data = SBA12, sum)
SBA12$Industry <- ifelse(SBA12$Industry == "AEROSPACE & DEFENSE" , "AEROSPACE & DEFENCE", SBA12$Industry)
SBA12$Industry <- ifelse(SBA12$Industry == "GAS, WATER & MULTI-UTILITIES" , "GAS, WATER & MULTIUTILITIES", SBA12$Industry)

## 3. DATA CLEANING: creazione dataset in formato tidy

SBA_TOT <- merge(SBA22, merge(SBA21, merge(SBA20, merge(SBA19, merge(SBA18, merge(SBA17, merge(SBA16, merge(SBA15, merge(SBA14, merge(SBA13, SBA12, by = "Industry", all = TRUE), by = "Industry", all = TRUE), by = "Industry", all = TRUE), by = "Industry", all = TRUE), by = "Industry", all = TRUE), by = "Industry", all = TRUE), by = "Industry", all = TRUE), by = "Industry", all = TRUE), by = "Industry", all = TRUE), by = "Industry", all = TRUE)
tidy_SBATOT <- pivot_longer(SBA_TOT, cols = c("Anno_2022", "Anno_2021", "Anno_2020", "Anno_2019", "Anno_2018", "Anno_2017", "Anno_2016", "Anno_2015", "Anno_2014", "Anno_2013", "Anno_2012"), names_to = "Year", values_to = "Investments")
tidy_SBATOT$Year <- gsub("Anno_", "", tidy_SBATOT$Year)
tidy_SBATOT <- tidy_SBATOT %>%
  arrange(desc(tidy_SBATOT$Investments)) 
tidy_SBATOT <- na.omit(tidy_SBATOT)

## 5. DATA VISUALIZATION: creazione grafico

ggplot(data = tidy_SBATOT, aes(x = Year, y = Investments, fill = Industry)) + 
  geom_col(position = position_dodge(width = 0.8)) +
  labs(x = "Anni", y = "Investimenti in R&D (milioni di euro)", fill = "Settori") +
  ggtitle("Andamento degli investimenti in R&D dei settori italiani che investono di più") +
  scale_y_continuous(breaks = seq(0, 6000, by = 200)) + 
  theme(panel.grid = element_blank(), 
        panel.background =  element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E", "#006d2c", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#BCBD22", "#7F7F7F", "#17BECF", "#41ab5d", "#FF9896", "#AEC7E8", "#98DF8A", "#D8BFD8", "#FFD700", "#7f2704", "#00FFFF", "#0D47A1", "#87CEEB", "#F5DEB3", "#000000")) 
    


  