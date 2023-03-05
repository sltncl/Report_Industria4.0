#' --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#' 
#' REPORT BUSINESS DATA ANALYTICS
#' 
#' TITOLO: ITALIA 4.0
#' 
#' CREATOR: NICOLA SALTARELLI
#' 
#' SCRIPT: Lo script in questione consente di generare un grafico a linea che illustra l'andamento degli investimenti 
#' in ricerca e sviluppo effettuati dai paesi europei che presentano i livelli di investimento più elevati.
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
scoreboard11 <- readxl::read_xls("SB2011_EU1000.xls")
scoreboard10 <- readxl::read_xls("SB2010_EU1000.xls")
scoreboard09 <- readxl::read_xls("SB2009_EU1000.xls")
scoreboard08 <- readxl::read_xls("SB2008_EU1000.xls")
scoreboard07 <- readxl::read_xls("SB2007_EU1000.xls")
scoreboard06 <- readxl::read_xlsx("SB2006_EU1000.xlsx")

## 3. DATA CLEANING: filtraggio dataset

# I dataset vengono filtrati selezionando le colonne relative al paese e alla quota di investimento in R&D

scoreboard22_EU <- scoreboard22[, c("Country", "R&D 2021 (€million)")] %>%
  arrange(Country)
names(scoreboard22_EU)[1:2] <- c("Country", "Anno_2022")

scoreboard21_EU <- scoreboard21[, c("Country", "R&D 2020 (€million)")] %>%
  arrange(Country)
names(scoreboard21_EU)[1:2] <- c("Country", "Anno_2021")

scoreboard20_EU <- scoreboard20[, c("Country", "R&D 2019 (€million)")] %>%
  arrange(Country)
names(scoreboard20_EU)[1:2] <- c("Country", "Anno_2020")

scoreboard19_EU <- scoreboard19[, c("Country", "R&D 2018/19 (€million)")] %>%
  arrange(Country)
names(scoreboard19_EU)[1:2] <- c("Country", "Anno_2019")

scoreboard18_EU <- scoreboard18[, c("Country", "R&D 2017/18 (€mn)")] %>%
  arrange(Country)
names(scoreboard18_EU)[1:2] <- c("Country", "Anno_2018")


scoreboard17_EU <- scoreboard17[, c("Country", "R&D 2016/17 (€million)")] %>%
  arrange(Country)
names(scoreboard17_EU)[1:2] <- c("Country", "Anno_2017")


scoreboard16_EU <- scoreboard16[, c("...3", "...5")] 
names(scoreboard16_EU)[1:2] <- c("Country", "Anno_2016")
scoreboard16_EU <- scoreboard16_EU %>%
  arrange(Country)

scoreboard15_EU <- scoreboard15[, c("...3", "...5")] 
names(scoreboard15_EU)[1:2] <- c("Country", "Anno_2015")
scoreboard15_EU <- scoreboard15_EU %>%
  arrange(Country)

scoreboard14_EU <- scoreboard14[, c("Country", "R&D 2013 (€million)")] %>%
  arrange(Country)
names(scoreboard14_EU)[1:2] <- c("Country", "Anno_2014")

scoreboard13_EU <- scoreboard13[, c("...3", "...5")] 
names(scoreboard13_EU)[1:2] <- c("Country", "Anno_2013")
scoreboard13_EU <- scoreboard13_EU %>%
  arrange(Country)

scoreboard12_EU <- scoreboard12[, c("...3", "...5")] 
names(scoreboard12_EU)[1:2] <- c("Country", "Anno_2012")
scoreboard12_EU <- scoreboard12_EU %>%
  arrange(Country)

scoreboard11_EU <- scoreboard11[, c("...4", "...5")] 
names(scoreboard11_EU)[1:2] <- c("Country", "Anno_2011")
scoreboard11_EU <- scoreboard11_EU %>%
  arrange(Country)

scoreboard10_EU <- scoreboard10[, c("...4", "...5")] 
names(scoreboard10_EU)[1:2] <- c("Country", "Anno_2010")
scoreboard10_EU <- scoreboard10_EU %>%
  arrange(Country)

scoreboard09_EU <- scoreboard09[, c("...4", "...5")] 
names(scoreboard09_EU)[1:2] <- c("Country", "Anno_2009")
scoreboard09_EU <- scoreboard09_EU %>%
  arrange(Country)

scoreboard08_EU <- scoreboard08[, c("...4", "...5")] 
names(scoreboard08_EU)[1:2] <- c("Country", "Anno_2008")
scoreboard08_EU <- scoreboard08_EU %>%
  arrange(Country)

scoreboard07_EU <- scoreboard07[, c("...4", "...5")] 
names(scoreboard07_EU)[1:2] <- c("Country", "Anno_2007")
scoreboard07_EU <- scoreboard07_EU %>%
  arrange(Country)

scoreboard06_EU <- scoreboard06[, c("...4", "...5")] 
names(scoreboard06_EU)[1:2] <- c("Country", "Anno_2006")
scoreboard06_EU <- scoreboard06_EU %>%
  arrange(Country)

## 4. DATA VALIDATION 

# Per calcolare gli investimenti annuali, si sommano gli investimenti di tutti i paesi e si apportano eventuali modifiche 
# per garantire la comparabilità dei dati. Ad esempio, dai dati del 2012 al 2016, i Paesi Bassi sono stati indicati 
# come "The Netherlands", mentre dal 2017 al 2022 sono stati chiamati semplicemente "Netherlands". 
# Pertanto, abbiamo sostituito il valore "The Netherlands" con "Netherlands" per poter confrontare gli investimenti 
# nel corso degli anni. Inoltre, nel 2013, la colonna relativa agli investimenti in R&D non presentava valori numerici, 
# quindi abbiamo apportato le necessarie modifiche per rendere i dati numerici e poterli comparare con gli investimenti degli altri anni.

# anno 2022

SB22 <- aggregate(Anno_2022 ~ Country, data = scoreboard22_EU, sum)
SB22 <- SB22 %>%
  arrange(desc(`Anno_2022`))

# anno 2021

SB21 <- aggregate(Anno_2021 ~ Country, data = scoreboard21_EU, sum)
SB21 <- SB21 %>%
  arrange(desc(`Anno_2021`))

# anno 2020

SB20 <- aggregate(Anno_2020 ~ Country, data = scoreboard20_EU, sum)
SB20 <- SB20 %>%
  arrange(desc(`Anno_2020`))

# anno 2019

SB19 <- aggregate(Anno_2019 ~ Country, data = scoreboard19_EU, sum)
SB19 <- SB19 %>%
  arrange(desc(`Anno_2019`))

# anno 2018

SB18 <- aggregate(Anno_2018 ~ Country, data = scoreboard18_EU, sum)
SB18 <- SB18 %>%
  arrange(desc(`Anno_2018`))

# anno 2017

SB17 <- aggregate(Anno_2017 ~ Country, data = scoreboard17_EU, sum)
SB17 <- SB17 %>%
  arrange(desc(`Anno_2017`))

# anno 2016

SB16 <- aggregate(Anno_2016 ~ Country, data = scoreboard16_EU, sum)
SB16 <- SB16 %>%
  arrange(desc(`Anno_2016`))
SB16$Country <- ifelse(SB16$Country == "The Netherlands" , "Netherlands", SB16$Country)

# anno 2015

SB15 <- aggregate(Anno_2015 ~ Country, data = scoreboard15_EU, sum)
SB15 <- SB15 %>%
  arrange(desc(`Anno_2015`))
SB15$Country <- ifelse(SB15$Country == "The Netherlands" , "Netherlands", SB15$Country)

# anno 2014

SB14 <- aggregate(Anno_2014 ~ Country, data = scoreboard14_EU, sum)
SB14 <- SB14 %>%
  arrange(desc(`Anno_2014`))
SB14$Country <- ifelse(SB14$Country == "The Netherlands" , "Netherlands", SB14$Country)

# anno 2013

scoreboard13_EU$`Anno_2013` <- as.integer(scoreboard13_EU$`Anno_2013`)
SB13 <- aggregate(Anno_2013 ~ Country, data = scoreboard13_EU, sum)
SB13 <- SB13 %>%
  arrange(desc(`Anno_2013`))
SB13$Country <- ifelse(SB13$Country == "The Netherlands" , "Netherlands", SB13$Country)

# anno 2012

SB12 <- aggregate(Anno_2012 ~ Country, data = scoreboard12_EU, sum)
SB12 <- SB12 %>%
  arrange(desc(`Anno_2012`))
SB12$Country <- ifelse(SB12$Country == "The Netherlands" , "Netherlands", SB12$Country)

# anno 2011

SB11 <- aggregate(Anno_2011 ~ Country, data = scoreboard11_EU, sum)
SB11 <- SB11 %>%
  arrange(desc(`Anno_2011`))
SB11$Country <- ifelse(SB11$Country == "The Netherlands" , "Netherlands", SB11$Country)

# anno 2010

SB10 <- aggregate(Anno_2010 ~ Country, data = scoreboard10_EU, sum)
SB10 <- SB10 %>%
  arrange(desc(`Anno_2010`))
SB10$Country <- ifelse(SB10$Country == "The Netherlands" , "Netherlands", SB10$Country)

# anno 2009

SB09 <- aggregate(Anno_2009 ~ Country, data = scoreboard09_EU, sum)
SB09 <- SB09 %>%
  arrange(desc(`Anno_2009`))
SB09$Country <- ifelse(SB09$Country == "The Netherlands" , "Netherlands", SB09$Country)

# anno 2008

SB08 <- aggregate(Anno_2008 ~ Country, data = scoreboard08_EU, sum)
SB08 <- SB08 %>%
  arrange(desc(`Anno_2008`))
SB08$Country <- ifelse(SB08$Country == "The Netherlands" , "Netherlands", SB08$Country)

# anno 2007

SB07 <- aggregate(Anno_2007 ~ Country, data = scoreboard07_EU, sum)
SB07 <- SB07 %>%
  arrange(desc(`Anno_2007`))
SB07$Country <- ifelse(SB07$Country == "The Netherlands" , "Netherlands", SB07$Country)

# anno 2006

SB06 <- aggregate(Anno_2006 ~ Country, data = scoreboard06_EU, sum)
SB06 <- SB06 %>%
  arrange(desc(`Anno_2006`))
SB06$Country <- ifelse(SB06$Country == "The Netherlands" , "Netherlands", SB06$Country)

## 3. DATA CLEANING: creazione dataset in formato tidy

RD_SB <- merge(SB22, merge(SB21, merge(SB20, merge(SB19, merge(SB18, merge(SB17, merge(SB16, merge(SB15, merge(SB14, merge(SB13, merge(SB12, merge(SB11, merge(SB10, merge(SB09, merge(SB08, merge(SB07, SB06, by = "Country", all = TRUE), by = "Country", all = TRUE), by = "Country", all = TRUE), by = "Country", all = TRUE), by = "Country", all = TRUE), by = "Country", all = TRUE), by = "Country", all = TRUE), by = "Country", all = TRUE), by = "Country", all = TRUE), by = "Country", all = TRUE), by = "Country", all = TRUE), by = "Country", all = TRUE), by = "Country", all = TRUE), by = "Country", all = TRUE), by = "Country", all = TRUE), by = "Country", all = TRUE)
tidy_RDSB <- pivot_longer(RD_SB, cols = c("Anno_2022", "Anno_2021", "Anno_2020", "Anno_2019", "Anno_2018", "Anno_2017", "Anno_2016", "Anno_2015", "Anno_2014", "Anno_2013", "Anno_2012", "Anno_2011", "Anno_2010", "Anno_2009", "Anno_2008", "Anno_2007", "Anno_2006"), names_to = "Year", values_to = "Investiments")
tidy_RDSB$Year <- gsub("Anno_", "", tidy_RDSB$Year)
tidy_RDSB$Year <- as.integer(tidy_RDSB$Year)

RDSB2 <- tidy_RDSB %>%
  filter(Country == "Italy" | Country == "Germany" | Country == "UK" | Country == "France" | Country == "Netherlands" | Country == "Ireland" | Country == "Sweden" | Country == "Finland" | Country == "Spain" | Country == "Denmark")

## 5. DATA VISUALIZATION: creazione grafico

ggplot(data = RDSB2, aes(x = Year, y = Investiments, color = Country, group = Country)) +
  geom_line(size = 1) +
  labs(x = "Anni", y = "Investimenti in ricerca e sviluppo (milioni di euro)", color = "Nazioni") +
  ggtitle("Andamento degli investimenti in R&D dei 10 paesi europei che investono di più") +
  geom_point(data = RDSB2, aes(x = Year, y = Investiments, color = Country), size = 2, shape = 21, fill = "white") +
  scale_y_continuous(breaks = seq(0, 150000, by = 5000)) +
  scale_x_continuous( breaks = seq(2006, 2022, 1) , labels = c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")) +
  theme(panel.grid = element_blank(), 
        panel.background =  element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")) + 
  guides(color = guide_legend(override.aes = list(size = 2)))

