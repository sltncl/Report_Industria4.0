#' --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#' 
#' REPORT BUSINESS DATA ANALYTICS
#' 
#' TITOLO: ITALIA 4.0
#' 
#' CREATOR: NICOLA SALTARELLI
#' 
#' SCRIPT: Questo script serve per realizzare un grafico a colonne sull'andamento degli investimenti
#' in ricerca e sviluppo da parte delle aziende italiane che investono di più
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

# I dataset vengono filtrati selezionando le colonne relative all'azienda, al paese e alla quota di investimento in R&D

scoreboard22_EU <- scoreboard22[, c("Company", "Country", "R&D 2021 (€million)")] %>%
  arrange(Country)

scoreboard21_EU <- scoreboard21[, c("Company", "Country", "R&D 2020 (€million)")] %>%
  arrange(Country)

scoreboard20_EU <- scoreboard20[, c("Company", "Country", "R&D 2019 (€million)")] %>%
  arrange(Country)

scoreboard19_EU <- scoreboard19[, c("Company", "Country", "R&D 2018/19 (€million)")] %>%
  arrange(Country)
names(scoreboard19_EU)[1:3] <- c("Company", "Country", "R&D 2018 (€million)")

scoreboard18_EU <- scoreboard18[, c("Company", "Country", "R&D 2017/18 (€mn)")] %>%
  arrange(Country)
names(scoreboard18_EU)[1:3] <- c("Company", "Country", "R&D 2017 (€million)")


scoreboard17_EU <- scoreboard17[, c("Company", "Country", "R&D 2016/17 (€million)")] %>%
  arrange(Country)
names(scoreboard17_EU)[1:3] <- c("Company", "Country", "R&D 2016 (€million)")


scoreboard16_EU <- scoreboard16[, c("...2", "...3", "...5")] 
names(scoreboard16_EU)[1:3] <- c("Company", "Country", "R&D 2015 (€million)")
scoreboard16_EU <- scoreboard16_EU %>%
  arrange(Country)

scoreboard15_EU <- scoreboard15[, c("...2", "...3", "...5")] 
names(scoreboard15_EU)[1:3] <- c("Company", "Country", "R&D 2014 (€million)")
scoreboard15_EU <- scoreboard15_EU %>%
  arrange(Country)

scoreboard14_EU <- scoreboard14[, c("Name", "Country", "R&D 2013 (€million)")] %>%
  arrange(Country)
names(scoreboard14_EU)[1:3] <- c("Company", "Country", "R&D 2013 (€million)")

scoreboard13_EU <- scoreboard13[, c("...2", "...3", "...5")] 
names(scoreboard13_EU)[1:3] <- c("Company", "Country", "R&D 2012 (€million)")
scoreboard13_EU <- scoreboard13_EU %>%
  arrange(Country)

scoreboard12_EU <- scoreboard12[, c("...2", "...3", "...5")] 
names(scoreboard12_EU)[1:3] <- c("Company", "Country", "R&D 2011 (€million)")
scoreboard12_EU <- scoreboard12_EU %>%
  arrange(Country)


## 4. DATA VALIDATION 

# Per ogni anno, i dati vengono ulteriormente filtrati selezionando solo le colonne pertinenti alle aziende italiane 
# e alle relative quote di investimento. Successivamente, vengono eliminate eventuali righe vuote e si procede 
# alla modifica dei valori delle celle che potrebbero impedire una corretta comparazione tra i diversi dataset.
# Ad esempio, nel 2012 l'azienda Chiesi Farmaceutici viene identificata come Chiesi. In tal caso, sostituiamo tale valore 
# con quello presente in tutti gli altri dataset, poiché si tratta della stessa azienda.
# Tuttavia, non è sempre possibile effettuare la stessa operazione anche con altre aziende come Fiat, che ha subito 
# fusioni con altre aziende in diversi anni, oppure Leonardo e Finmeccanica, che hanno cambiato nome e ragione sociale. 
# In tali casi, si deve procedere ad una valutazione caso per caso al fine di garantire la comparabilità dei dati tra i diversi dataset.

# anno 2022

SBA22 <- scoreboard22_EU %>%
  filter(Country == "Italy")
SBA22 <- SBA22[, c("Company", "R&D 2021 (€million)")]
colnames(SBA22)[2] <- "Anno_2022"
SBA22 <- SBA22 %>%
  mutate(Company = toupper(Company))

# anno 2021

SBA21 <- scoreboard21_EU %>%
  filter(Country == "Italy")
SBA21 <- SBA21[, c("Company", "R&D 2020 (€million)")]
colnames(SBA21)[2] <- "Anno_2021"
SBA21 <- SBA21 %>%
  mutate(Company = toupper(Company))

# anno 2020

SBA20 <- scoreboard20_EU %>%
  filter(Country == "Italy")
SBA20 <- SBA20[, c("Company", "R&D 2019 (€million)")]
colnames(SBA20)[2] <- "Anno_2020"
SBA20 <- SBA20 %>%
  mutate(Company = toupper(Company))

# anno 2019

SBA19 <- scoreboard19_EU %>%
  filter(Country == "Italy")
SBA19 <- SBA19[, c("Company", "R&D 2018 (€million)")]
colnames(SBA19)[2] <- "Anno_2019"
SBA19 <- SBA19 %>%
  mutate(Company = toupper(Company))

# anno 2018

SBA18 <- scoreboard18_EU %>%
  filter(Country == "Italy")
SBA18 <- SBA18[, c("Company", "R&D 2017 (€million)")]
colnames(SBA18)[2] <- "Anno_2018"
SBA18 <- SBA18 %>%
  mutate(Company = toupper(Company))

# anno 2017

SBA17 <- scoreboard17_EU %>%
  filter(Country == "Italy")
SBA17 <- SBA17[, c("Company", "R&D 2016 (€million)")]
colnames(SBA17)[2] <- "Anno_2017"
SBA17 <- SBA17 %>%
  mutate(Company = toupper(Company))

# anno 2016

SBA16 <- scoreboard16_EU %>%
  filter(Country == "Italy")
SBA16 <- SBA16[, c("Company", "R&D 2015 (€million)")]
colnames(SBA16)[2] <- "Anno_2016"
SBA16 <- SBA16 %>%
  mutate(Company = toupper(Company))

# anno 2015

SBA15 <- scoreboard15_EU %>%
  filter(Country == "Italy")
SBA15 <- SBA15[, c("Company", "R&D 2014 (€million)")]
colnames(SBA15)[2] <- "Anno_2015"
SBA15 <- SBA15 %>%
  mutate(Company = toupper(Company))

# anno 2014

SBA14 <- scoreboard14_EU %>%
  filter(Country == "Italy")
SBA14 <- SBA14[, c("Company", "R&D 2013 (€million)")]
colnames(SBA14)[2] <- "Anno_2014"
SBA14 <- SBA14 %>%
  mutate(Company = toupper(Company))

# anno 2013

scoreboard13_EU$`R&D 2012 (€million)` <- as.integer(scoreboard13_EU$`R&D 2012 (€million)`)
SBA13 <- scoreboard13_EU %>%
  filter(Country == "Italy")
SBA13 <- subset(SBA13, "R&D 2012 (€million)" !=0)
SBA13 <- SBA13[, c("Company", "R&D 2012 (€million)")]
colnames(SBA13)[2] <- "Anno_2013"
SBA13 <- SBA13 %>%
  mutate(Company = toupper(Company))

# anno 2012

SBA12 <- scoreboard12_EU %>%
  filter(Country == "Italy")
SBA12 <- SBA12[, c("Company", "R&D 2011 (€million)")]
colnames(SBA12)[2] <- "Anno_2012"
SBA12 <- SBA12 %>%
  mutate(Company = toupper(Company))
SBA12$Company <- ifelse(SBA12$Company == "CHIESI" , "CHIESI FARMACEUTICI", SBA12$Company)


## 3. DATA CLEANING: creazione dataset in formato tidy

SBA_TOT <- merge(SBA22, merge(SBA21, merge(SBA20, merge(SBA19, merge(SBA18, merge(SBA17, merge(SBA16, merge(SBA15, merge(SBA14, merge(SBA13, SBA12, by = "Company", all = TRUE), by = "Company", all = TRUE), by = "Company", all = TRUE), by = "Company", all = TRUE), by = "Company", all = TRUE), by = "Company", all = TRUE), by = "Company", all = TRUE), by = "Company", all = TRUE), by = "Company", all = TRUE), by = "Company", all = TRUE)
tidy_SBATOT <- pivot_longer(SBA_TOT, cols = c("Anno_2022", "Anno_2021", "Anno_2020", "Anno_2019", "Anno_2018", "Anno_2017", "Anno_2016", "Anno_2015", "Anno_2014", "Anno_2013", "Anno_2012"), names_to = "Year", values_to = "Investments")
tidy_SBATOT$Year <- gsub("Anno_", "", tidy_SBATOT$Year)
tidy_SBATOT <- tidy_SBATOT %>%
  arrange(desc(tidy_SBATOT$Investments)) %>%
  filter(Company == "ENI" | Company == "ENEL" | Company == "FERRARI" | Company == "INTESA SANPAOLO" | Company == "TELECOM ITALIA" | Company == "FIAT" | Company == "FIAT CHRYSLER AUTOMOBILES" | Company == "LEONARDO - FINMECCANICA" | Company == "LEONARDO" | Company == "FIAT INDUSTRIAL" | Company == "FINMECCANICA" | Company == "UNICREDIT" | Company == "CHIESI FARMACEUTICI")
tidy_SBATOT <- na.omit(tidy_SBATOT)

## 5. DATA VISUALIZATION: creazione grafico

ggplot(data = tidy_SBATOT, aes(x = Year, y = Investments, fill = Company)) + 
  geom_col(position = position_dodge(width = 0.8)) +
  labs(x = "Anni", y = "Investimenti in R&D (milioni di euro)", fill = "Compagnie") +
  ggtitle("Andamento degli investimenti in R&D delle compagnie italiane che investono di più") +
  scale_y_continuous(breaks = seq(0, 5000, by = 200)) + 
  theme(panel.grid = element_blank(), 
        panel.background =  element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E", "#006d2c", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#BCBD22", "#7F7F7F", "#17BECF", "#41ab5d", "#F5DEB3", "#000000"))

