#### Script to Compare RFMO/FAO Catch and Export plus Imports ####

library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

comtrade <- read.csv(file.choose(), stringsAsFactors=TRUE)
head(comtrade)
colnames(comtrade)

faoallelas <- read.csv(file.choose(), stringsAsFactors=TRUE)
head(faoallelas)
colnames(faoallelas)

rfmoallelas <- read.csv(file.choose(), stringsAsFactors=TRUE)
head(rfmoallelas)
colnames(rfmoallelas)

############ Comtrade Tidying ###########
### Cut ComTrade to usable columns
comcut <- comtrade[, c(4, 8:9,11, 13:14, 21:22, 31:32, 35:36, 40)]
head(comcut)
colnames(comcut)

colnames(comcut) [1] <- "Year"

## Aggregate all import/export to exporter/importer per year
CommAgg <- aggregate(Qty ~ Year + ReporterDesc + PartnerDesc + FlowDesc + QtyUnitAbbr, data=comcut, FUN = sum)
head(CommAgg)
tail(CommAgg)

CommAggKG <- CommAgg[CommAgg$QtyUnitAbbr != "N/A", ]
head(CommAggKG)
tail(CommAggKG)

CommAgg2 <- aggregate(Qty ~ Year + ReporterDesc + PartnerDesc + FlowDesc, data=CommAggKG, FUN=sum)
head(CommAgg2)

## Check all unique countries
countriesreporter <- unique(CommAgg2$ReporterDesc)
sortedreporterdesc <- sort(countriesreporter)
print(sortedreporterdesc)

countriespartner <- unique(CommAgg2$PartnerDesc)
sortedpartner <- sort(countriespartner)
print(sortedpartner)


## Rename to standard country names
CommAggRename <- CommAgg2 %>%
  mutate(ReporterDesc = case_when(
    ReporterDesc == "C\xf4te d'Ivoire" ~ "Côte d'Ivoire",
    ReporterDesc == "T\xfcrkiye" ~ "Türkiye",
    ReporterDesc == "China, Hong Kong SAR" | ReporterDesc == "China, Macao SAR" ~ "China",
    ReporterDesc == "State of Palestine" ~ "Palestine",
    ReporterDesc == "Belgium-Luxembourg (...1998)" ~ "Belgium",
    ReporterDesc == "French Guiana (Overseas France)" ~ "French Guiana",
    ReporterDesc == "Guadeloupe (Overseas France)" ~ "Guadeloupe",
    ReporterDesc == "Martinique (Overseas France)" ~ "Martinique",
    ReporterDesc == "Mayotte (Overseas France)" ~ "Mayotte",
    ReporterDesc == "Réunion (Overseas France)" ~ "Reunion",
    ReporterDesc == "Serbia and Montenegro (...2005)" ~ "Serbia and Montenegro",
    ReporterDesc == "Sudan (...2011)" ~ "Sudan (former)",
    ReporterDesc == "United Rep. of Tanzania" ~ "Tanzania",
    ReporterDesc == "FS Micronesia" ~ "Micronesia",
    ReporterDesc == "Saint Barth\xe9lemy" ~ "Saint Barthelemy",
    ReporterDesc == "Cura\xe7ao" ~ "Curacao",
    TRUE ~ ReporterDesc
  ))

CommAggRename <- CommAggRename %>%
  mutate(PartnerDesc = case_when(
    PartnerDesc == "C\xf4te d'Ivoire" ~ "Côte d'Ivoire",
    PartnerDesc == "T\xfcrkiye" ~ "Türkiye",
    PartnerDesc == "China, Hong Kong SAR" | PartnerDesc == "China, Macao SAR" ~ "China",
    PartnerDesc == "State of Palestine" ~ "Palestine",
    PartnerDesc == "Belgium-Luxembourg (...1998)" ~ "Belgium",
    PartnerDesc == "Falkland Isds (Malvinas)" ~ "Falkland Is.(Malvinas)",
    PartnerDesc == "French Guiana (Overseas France)" ~ "French Guiana",
    PartnerDesc == "Guadeloupe (Overseas France)" ~ "Guadeloupe",
    PartnerDesc == "Martinique (Overseas France)" ~ "Martinique",
    PartnerDesc == "Mayotte (Overseas France)" ~ "Mayotte",
    PartnerDesc == "Réunion (Overseas France)" ~ "Reunion",
    PartnerDesc == "Serbia and Montenegro (...2005)" ~ "Serbia and Montenegro",
    PartnerDesc == "Sudan (...2011)" ~ "Sudan (former)",
    PartnerDesc == "United Rep. of Tanzania" ~ "Tanzania",
    PartnerDesc == "FS Micronesia" ~ "Micronesia",
    PartnerDesc == "Saint Barth\xe9lemy" ~ "Saint Barthelemy",
    PartnerDesc == "Cura\xe7ao" ~ "Curacao",
    TRUE ~ PartnerDesc
  ))

head(CommAggRename)


CommAggNewName <- aggregate(Qty ~ Year + ReporterDesc + PartnerDesc + FlowDesc, data=CommAggRename, FUN=sum)
head(CommAggNewName)

unique(CommAggNewName$FlowDesc)

CommExpImp <- CommAggNewName %>%
  mutate(FlowDesc = case_when(
    FlowDesc == "Domestic Export" | FlowDesc == "Export of goods after inward processing" |
      FlowDesc == "Re-export" ~ "Export",
    FlowDesc == "Foreign Import" | FlowDesc == "Import of goods after outward processing" |
      FlowDesc == "Import of goods for inward processing" | FlowDesc == "Re-import" ~ "Import",
    TRUE ~ FlowDesc
  ))
head(CommExpImp)

ExpImpAgg <- aggregate(Qty ~ Year + ReporterDesc + PartnerDesc + FlowDesc, data=CommExpImp, FUN=sum)
head(ExpImpAgg)


########### FAO Tidying ###############
## change data to long format
FAOlong <- gather(faoallelas, key="Year", value="CatchMT", -Country)
head(FAOlong)

FAOlong$Year <- substr(FAOlong$Year, 2, nchar(FAOlong$Year))
head(FAOlong)
FAOlong$Year <- as.numeric(FAOlong$Year)

FAOcountries <- unique(FAOlong$Country)
head(FAOcountries)

FAOlong <- FAOlong %>%
  mutate(Country = case_when(
    Country == "United States of America" ~ "USA",
    Country == "Congo, Dem. Rep. of the" ~ "Dem. Rep. of the Congo",
    Country == "British Virgin Islands" ~ "Br. Birgin Isds",
    Country == "Saint Vincent/Grenadines" ~ "Saint Vincent and the Grenadines",
    Country == "Saint Helena/Asc./Trist." ~ "Saint Helena",
    TRUE ~ Country
  ))
head(FAOlong)


########### RFMO Tidying ###############
## Check all unique countries
countriesrfmo <- unique(rfmoallelas$Country)
sortedcountries <- sort(countriesrfmo)
print(sortedcountries)


## Rename values to merge again
RFMOALL <- rfmoallelas %>%
  mutate(Country = case_when(
    Country == "China PR" ~ "China",
    Country == "Chinese Taipei" | Country == "Taiwan, Province of China" ~ "Taiwan",
    Country == "UK-British Virgin Islands" ~ "Br. Virgin Isds",
    Country == "UK-Sta Helena" ~ "Saint Helena",
    Country == "St Vincent and Grenadines" ~ "Saint Vincent and the Grenadines",
    Country == "Sta Lucia" ~ "Saint Lucia",
    Country == "UK-Bermuda" ~ "Bermuda",
    Country == "S Tomé e Príncipe" ~ "Sao Tome e Principe",
    Country == "UK-Turks and Caicos" ~ "Turks and Caicos",
    Country == "Maroc" ~ "Morocco",
    TRUE ~ Country
  ))

head(RFMOALL)  

### Aggregate RFMO catch to country
RFMOaggALL <- aggregate(CaptureMT ~ Country + Year, data=RFMOALL, FUN = sum)
head(RFMOaggALL)
tail(RFMOaggALL)



##### HK Comtrade ######

tradedb <- read.csv(file.choose(), stringsAsFactors=TRUE)
head(tradedb)
colnames(tradedb)

tradepairedt <- t.test(tradedb$HK.Import, tradedb$Comtrade.Export, paired=TRUE)
summary(tradepairedt)
tradepairedt
