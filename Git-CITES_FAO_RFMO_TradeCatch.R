### Finding Differences in shipments and trade vs. catch in RFMOs / FAO

library(dplyr)
library(tidyverse)
library(tidyr)


citesclean <- read.csv(file.choose(), stringsAsFactors=TRUE)
head(citesclean)
colnames(citesclean)
str(citesclean)

### Change all HS exporter to be the importing country 
citesclean <- citesclean %>%
  mutate(Exporter = trimws(Exporter),
         Importer = trimws(Importer))

citesHS <- citesclean %>%
  mutate(Exporter = case_when(
  Exporter == "HS" & Importer == "PT" ~ "PT",
  Exporter == "HS" & Importer == "ES" ~ "ES",
  Exporter == "HS" & Importer == "BZ" ~ "BZ",
  Exporter == "HS" & Importer == "KR" ~ "KR",
    TRUE ~ Exporter))
head(citesHS)

### Merge China subsidiaries
unique(citesHS$Exporter)
cites2Merge <- citesHS %>%
  mutate(Exporter = case_when(
    Exporter == "HK" ~ "CN",
    Exporter == "TW" ~ "CN",
    TRUE ~ Exporter))
head(cites2Merge)


#### Aggregate the CITES export per species and country ####
citeskg <- aggregate(Quantity ~ Exporter + Reported.Taxon, data=cites2Merge, FUN = sum)
head(citeskg)
tail(citeskg)

## Convert Kg to MT
citeskg$CITESMT <- (citeskg$Quantity*0.001)
head(citeskg)

citesMT <- citeskg[, c(1:2, 4)]
head(citesMT)

names(citesMT)[names(citesMT) == "Reported.Taxon"] <- "Species"

unique(citesMT$Species)
CITES_Spec <- citesMT %>%
  mutate(Species = case_when(
    Species == "Alopiidae spp." ~ "Alopias spp.",
    Species == "Manta birostris" ~ "Mobula birostris",
    Species == "Manta spp." ~ "Mobula spp.",
    Species == "Mobula japanica" ~ "Mobula mobular",
    Species == "Sphyrna spp." ~ "Sphyrnidae spp.",
    TRUE ~ Species))
head(CITES_Spec)


#### Format RFMO Data ####
rfmocatch <- read.csv(file.choose(), stringsAsFactors=TRUE)
head(rfmocatch)
tail(rfmocatch)
colnames(rfmocatch)

# Check species names and rename relevant ones
rfmospec <- unique(rfmocatch$Species)
rfmospec

rfmocatch <- rfmocatch %>%
  mutate(Species = case_when(
    Species == "Basking shark" ~ "Cetorhinus maximus",
    Species == "Alopias spp" ~ "Alopias spp.",
    Species == "Bigeye thresher" ~ "Alopias superciliosus",
    Species == "Bonnethead and hammerhead sharks" ~ "Sphyrnidae spp.",
    Species == "Devil Ray" ~ "Mobula spp.",
    Species == "Giant manta" ~ "Mobula birostris",
    Species == "Giant mantas" ~ "Mobula birostris",
    Species == "Great hammerhead" ~ "Sphyrna mokarran",
    Species == "Great hammerhead shark" ~ "Sphyrna mokarran",
    Species == "Great white shark" ~ "Carcharodon carcharias",
    Species == "Great White shark" ~ "Carcharodon carcharias",
    Species == "Hammerhead sharks nei" ~ "Sphyrnidae spp.",
    Species == "Hammerhead sharks, etc. nei" ~ "Sphyrnidae spp.",
    Species == "Longfin mako" ~ "Isurus paucus",
    Species == "Mako sharks" ~ "Isurus spp.",
    Species == "Mantas, devil rays nei" ~ "Mobula spp.",
    Species == "Mantas, devil rays, etc. nei" ~ "Mobula spp.",
    Species == "Manta alfredi" ~ "Mobula alfredi",
    Species == "Manta birostris" ~ "Mobula birostris",
    Species == "Mobula nei" ~ "Mobula spp.",
    Species == "Mobulidae" ~ "Mobula spp.",
    Species == "Mobula japanica" ~ "Mobula mobular",
    Species == "Mobula eregoodootenkee" ~ "Mobula eregoodoo",
    Species == "Oceanic whitetip shark" ~ "Carcharhinus longimanus",
    Species == "Pelagic thresher" ~ "Alopias pelagicus",
    Species == "Pelagic thresher shark" ~ "Alopias pelagicus",
    Species == "Porbeagle" ~ "Lamna nasus",
    Species == "Pristis microdon" ~ "Pristis pristis",
    Species == "Pristis perotteti" ~ "Pristis pristis",
    Species == "Scalloped hammerhead" ~ "Sphyrna lewini",
    Species == "Sphyrna spp" ~ "Sphyrnidae spp.",
    Species == "Sphyrnidae" ~ "Sphyrnidae spp.",
    Species == "Shortfin mako" ~ "Isurus oxyrinchus",
    Species == "Silky shark" ~ "Carcharhinus falciformis",
    Species == "Silky or blacktip" ~ "Carcharhinus falciformis",
    Species == "Spinetail mobula" ~ "Mobula mobular", 
    Species == "Smooth hammerhead" ~ "Sphyrna zygaena",
    Species == "Thresher" ~ "Alopias vulpinus",
    Species == "Thresher shark" ~ "Alopias vulpinus",
    Species == "Thresher sharks nei" ~ "Alopias spp.",
    Species == "Whale shark" ~ "Rhincodon typus",
    Species == "Sawfishes" ~ "Pristidae spp.",
    Species == "Common sawfish" ~ "Pristis pristis",
    Species == "Dwarf sawfish" ~ "Pristis clavata",
    TRUE ~ Species))
head(rfmocatch)

####  Subset to only relevant species and years post listing
rfmoCITES_YrSpec <- rfmocatch %>%
  filter( Species == "Alopias spp." & Year >= 2017 |
            Species == "Alopias pelagicus" & Year >= 2017 |
            Species == "Alopias superciliosus" & Year >= 2017 |
            Species == "Alopias vulpinus" & Year >= 2017 |
            Species == "Carcharhinus falciformis" & Year >= 2017 |
            Species == "Carcharhinus longimanus" & Year >= 2014 |
            Species == "Carcharodon carcharias" & Year >= 2005 |
            Species == "Cetorhinus maximus" & Year >= 2003 |
            Species == "Isurus spp." & Year >= 2020 | 
            Species == "Isurus oxyrinchus" & Year >= 2020 |
            Species == "Isurus paucus" & Year >= 2020 |
            Species == "Lamna nasus" & Year >= 2014 |
            Species == "Mobula spp." & Year >= 2014 |
            Species == "Mobula tarapacana" & Year >= 2017 |
            Species == "Mobula mobular" & Year >= 2017 |
            Species == "Mobula thurstoni" & Year >= 2017 |
            Species == "Mobula eregoodoo" & Year >= 2017 |
            Species == "Mobula birostris" & Year >= 2014 |
            Species == "Mobula alfredi" & Year >= 2014 |
            Species == "Mobula kuhlii" & Year >= 2017 |
            Species == "Mobula munkiana" & Year >= 2017 |
            Species == "Mobula hypostoma" & Year >= 2017 |
            Species == "Anoxypristis cuspidata" & Year >= 2007 |
            Species == "Pristis clavata" & Year >= 2007 |
            Species == "Pristis pectinata" & Year >= 2007 |
            Species == "Pristis pristis" & Year >= 2007 |
            Species == "Pristis zijsron" & Year >= 2007 |
            Species == "Pristidae spp." & Year >= 2007 |
            Species == "Rhincodon typus" & Year >= 2003 |
            Species == "Sphyrna lewini" & Year >= 2014 |
            Species == "Sphyrna mokarran" & Year >= 2014 |
            Species == "Sphyrna tiburo" & Year >= 2023 |
            Species == "Sphyrna zygaena" & Year >= 2014 |
            Species == "Sphyrnidae spp." & Year >= 2014)
head(rfmoCITES_YrSpec)


# Summarize all catch by species and country
rfmoMT <- aggregate(CaptureMT ~ Country + Species, data=rfmoCITES_YrSpec, FUN = sum)
head(rfmoMT)


### Get country names from RFMO to be 2 letter ISO codes to match with CITES data

countryISO <- read.csv(file.choose(), stringsAsFactors = TRUE)
head(countryISO)
colnames(countryISO) [1] <- "Country"

rfmoISO <- merge(rfmoMT, countryISO, by="Country", all.x=TRUE, all.y=FALSE)
head(rfmoISO)

rfmoISO <- rfmoISO[, c(1:5)]
head(rfmoISO)


rfmoISOfixed <- rfmoISO %>%
  mutate(alpha.2 = case_when(
    Country == "Algerie" ~ "DZ",
    Country == "Bolivia" ~ "BO",
    Country == "Cape Verde" ~ "CV",
    Country == "China PR" ~ "CN",
    Country == "Chinese Taipei" ~ "CN",
    Country == "Falklands" ~ "FK",
    Country == "Great Britain" ~ "GB",
    Country == "Guinea Ecuatorial" ~ "GQ",
    Country == "Iran" ~ "IR",
    Country == "Korea" ~ "KP",
    Country == "Korea Rep" ~ "KR",
    Country == "Maroc" ~ "MA",
    Country == "Namibia" ~ "NAm",
    Country == "Reunion" ~ "RE",
    Country == "S TomÃ© e PrÃ-ncipe" ~ "ST",
    Country == "St Pierre et Miquelon" ~ "PM",
    Country == "St Vincent and Grenadines" ~ "VC",
    Country == "Sta Lucia" ~ "LC",
    Country == "Taiwan" ~ "TW",
    Country == "Tanzania" ~ "TZ",
    Country == "TÃ¼rkiye" ~ "TR",
    Country == "UAE" ~ "AE",
    Country == "UK-Bermuda" ~ "BM",
    Country == "UK-British Virgin Islands" ~ "VG",
    Country == "UK-Sta Helena" ~ "SH",
    Country == "UK-Turks and Caicos" ~ "TC",
    Country == "United Kingdom" ~ "GB",
    Country == "United States" ~ "US",
    Country == "USA" ~ "US",
    Country == "Venezuela" ~ "VE",
    TRUE ~ alpha.2))
head(rfmoISOfixed)

rfmoISOfix <- rfmoISOfixed[rfmoISOfixed$Country != "Unrecorded", ]
head(rfmoISOfix)


#### Format FAO Data ####
faocatch <- read.csv(file.choose(), stringsAsFactors=TRUE)
head(faocatch)
tail(faocatch)

unique(faocatch$Species)

faocatch <- faocatch %>%
  mutate(Species = case_when(
    Species == "Basking shark" ~ "Cetorhinus maximus",
    Species == "Alopias spp" ~ "Alopias spp.",
    Species == "Bigeye thresher" ~ "Alopias superciliosus",
    Species == "Bonnethead and hammerhead sharks" ~ "Sphyrnidae spp.",
    Species == "Bonnethead" ~ "Sphyrna tiburo",
    Species == "Devil Ray" ~ "Mobula spp.",
    Species == "Devil fish" ~ "Mobula spp.",
    Species == "Giant manta" ~ "Mobula birostris",
    Species == "Giant mantas" ~ "Mobula birostris",
    Species == "Great hammerhead" ~ "Sphyrna mokarran",
    Species == "Great hammerhead shark" ~ "Sphyrna mokarran",
    Species == "Great white shark" ~ "Carcharodon carcharias",
    Species == "Great White shark" ~ "Carcharodon carcharias",
    Species == "Hammerhead sharks nei" ~ "Sphyrnidae spp.",
    Species == "Hammerhead sharks, etc. nei" ~ "Sphyrnidae spp.",
    Species == "Longfin mako" ~ "Isurus paucus",
    Species == "Mako sharks" ~ "Isurus spp.",
    Species == "Mantas, devil rays nei" ~ "Mobula spp.",
    Species == "Mantas, devil rays, etc. nei" ~ "Mobula spp.",
    Species == "Manta alfredi" ~ "Mobula alfredi",
    Species == "Manta birostris" ~ "Mobula birostris",
    Species == "Mobula nei" ~ "Mobula spp.",
    Species == "Mobulidae" ~ "Mobula spp.",
    Species == "Mobula japanica" ~ "Mobula mobular",
    Species == "Mobula eregoodootenkee" ~ "Mobula eregoodoo",
    Species == "Oceanic whitetip shark" ~ "Carcharhinus longimanus",
    Species == "Pelagic thresher" ~ "Alopias pelagicus",
    Species == "Pelagic thresher shark" ~ "Alopias pelagicus",
    Species == "Porbeagle" ~ "Lamna nasus",
    Species == "Pristis microdon" ~ "Pristis pristis",
    Species == "Pristis perotteti" ~ "Pristis pristis",
    Species == "Scalloped hammerhead" ~ "Sphyrna lewini",
    Species == "Sphyrna spp" ~ "Sphyrnidae spp.",
    Species == "Sphyrnidae" ~ "Sphyrnidae spp.",
    Species == "Shortfin mako" ~ "Isurus oxyrinchus",
    Species == "Silky shark" ~ "Carcharhinus falciformis",
    Species == "Silky or blacktip" ~ "Carcharhinus falciformis",
    Species == "Spinetail mobula" ~ "Mobula mobular", 
    Species == "Spinetail ray" ~ "Mobula mobular",
    Species == "Smooth hammerhead" ~ "Sphyrna zygaena",
    Species == "Smalleye hammerhead" ~ "Sphyrna tudes",
    Species == "Thresher" ~ "Alopias vulpinus",
    Species == "Thresher shark" ~ "Alopias vulpinus",
    Species == "Thresher sharks nei" ~ "Alopias spp.",
    Species == "Whale shark" ~ "Rhincodon typus",
    Species == "Sawfishes" ~ "Pristidae spp.",
    Species == "Common sawfish" ~ "Pristis pristis",
    Species == "Dwarf sawfish" ~ "Pristis clavata",
    TRUE ~ Species))
head(faocatch)

####  Subset to only relevant species and years post listing
faoCITES_YrSpec <- faocatch %>%
  filter( Species == "Alopias spp." & Year >= 2017 |
            Species == "Alopias pelagicus" & Year >= 2017 |
            Species == "Alopias superciliosus" & Year >= 2017 |
            Species == "Alopias vulpinus" & Year >= 2017 |
            Species == "Carcharhinus falciformis" & Year >= 2017 |
            Species == "Carcharhinus longimanus" & Year >= 2014 |
            Species == "Carcharodon carcharias" & Year >= 2005 |
            Species == "Cetorhinus maximus" & Year >= 2003 |
            Species == "Isurus spp." & Year >= 2020 | 
            Species == "Isurus oxyrinchus" & Year >= 2020 |
            Species == "Isurus paucus" & Year >= 2020 |
            Species == "Lamna nasus" & Year >= 2014 |
            Species == "Mobula spp." & Year >= 2014 |
            Species == "Mobula tarapacana" & Year >= 2017 |
            Species == "Mobula mobular" & Year >= 2017 |
            Species == "Mobula thurstoni" & Year >= 2017 |
            Species == "Mobula eregoodoo" & Year >= 2017 |
            Species == "Mobula birostris" & Year >= 2014 |
            Species == "Mobula alfredi" & Year >= 2014 |
            Species == "Mobula kuhlii" & Year >= 2017 |
            Species == "Mobula munkiana" & Year >= 2017 |
            Species == "Mobula hypostoma" & Year >= 2017 |
            Species == "Anoxypristis cuspidata" & Year >= 2007 |
            Species == "Pristis clavata" & Year >= 2007 |
            Species == "Pristis pectinata" & Year >= 2007 |
            Species == "Pristis pristis" & Year >= 2007 |
            Species == "Pristis zijsron" & Year >= 2007 |
            Species == "Pristidae spp." & Year >= 2007 |
            Species == "Rhincodon typus" & Year >= 2003 |
            Species == "Sphyrna lewini" & Year >= 2014 |
            Species == "Sphyrna mokarran" & Year >= 2014 |
            Species == "Sphyrna tiburo" & Year >= 2023 |
            Species == "Sphyrna zygaena" & Year >= 2014 |
            Species == "Sphyrnidae spp." & Year >= 2014)
head(faoCITES_YrSpec)

faoCITES_YrSpec <- faoCITES_YrSpec[,c(1:4)]


# Summarize all catch by species and country
faoMT <- aggregate(CaptureMT ~ Country + Species, data=faoCITES_YrSpec, FUN = sum)
head(faoMT)


### Match FAO countries to 2-letter ISO
faoISO <- merge(faoMT, countryISO, by="Country", all.x=TRUE, all.y=FALSE)
head(faoISO)

faoISO <- faoISO[, c(1:4)]
head(rfmoISO)

faoISOfixed <- faoISO %>%
  mutate(alpha.2 = case_when(
    Country == "Algerie" ~ "DZ",
    Country == "Bolivia" ~ "BO",
    Country == "Cape Verde" ~ "CV",
    Country == "Channel Islands" ~ "GB",
    Country == "China PR" ~ "CN",
    Country == "Chinese Taipei" ~ "CN",
    Country == "Falklands" ~ "FK",
    Country == "Falkland Is.(Malvinas)" ~ "FK",
    Country == "Great Britain" ~ "GB",
    Country == "Guinea Ecuatorial" ~ "GQ",
    Country == "Iran" ~ "IR",
    Country == "Iran (Islamic Rep. of)" ~ "IR",
    Country == "Korea" ~ "KP",
    Country == "Korea Rep" ~ "KR",
    Country == "Maroc" ~ "MA",
    Country == "Micronesia (Fed. States)" ~ "FM",
    Country == "Namibia" ~ "NAm",
    Country == "Reunion" ~ "RE",
    Country == "S TomÃ© e PrÃ-ncipe" ~ "ST",
    Country == "St Pierre et Miquelon" ~ "PM",
    Country == "St. Pierre and Miquelon" ~ "PM",
    Country == "St Vincent and Grenadines" ~ "VC",
    Country == "Saint Vincent/Grenadines" ~ "VC",
    Country == "Sta Lucia" ~ "LC",
    Country == "Taiwan" ~ "TW",
    Country == "Taiwan Province of China" ~ "TW",
    Country == "Tanzania" ~ "TZ",
    Country == "Tanzania, United Rep. of" ~ "TZ",
    Country == "TÃ¼rkiye" ~ "TR",
    Country == "UAE" ~ "AE",
    Country == "UK-Bermuda" ~ "BM",
    Country == "UK-British Virgin Islands" ~ "VG",
    Country == "UK-Sta Helena" ~ "SH",
    Country == "UK-Turks and Caicos" ~ "TC",
    Country == "United Kingdom" ~ "GB",
    Country == "US Virgin Islands" ~ "VI",
    Country == "United States" ~ "US",
    Country == "USA" ~ "US",
    Country == "Venezuela" ~ "VE",
    Country == "Venezuela (Boliv Rep of)" ~ "VE",
    TRUE ~ alpha.2))
head(faoISOfixed)


######### Merge territories to CITES Reporting Party #######
head(rfmoISOfix)
unique(rfmoISOfix$alpha.2)


rfmoCountryJoin <- rfmoISOfix %>%
  mutate(alpha.2 = case_when(
    alpha.2 == "BM" ~ "GB",
    alpha.2 == "CW" ~ "NL",
    alpha.2 == "NA" ~ "NAm",
    alpha.2 == "PF" ~ "FR",
    alpha.2 == "RE" ~ "FR",
    alpha.2 == "SH" ~ "GB",
    alpha.2 == "TC" ~ "GB",
    alpha.2 == "TW" ~ "CN",
    alpha.2 == "VG" ~ "GB",
TRUE ~ alpha.2))
head(rfmoCountryJoin)

rfmoCountries <- aggregate(CaptureMT ~ alpha.2 + Species, data=rfmoCountryJoin, FUN = sum)
head(rfmoCountries)


head(faoISOfixed)
unique(faoISOfixed$alpha.2)

faoCountryJoin <- faoISOfixed %>%
  mutate(alpha.2 = case_when(
    alpha.2 == "BM" ~ "GB",
    alpha.2 == "AS" ~ "US",
    alpha.2 == "PF" ~ "FR",
    alpha.2 == "RE" ~ "FR",
    alpha.2 == "NC" ~ "FR",
    alpha.2 == "PR" ~ "US",
    alpha.2 == "TW" ~ "CN",
    alpha.2 == "VI" ~ "US",
    alpha.2 == "YT" ~ "FR",
    TRUE ~ alpha.2))
head(faoCountryJoin)

faoCountries <- aggregate(CaptureMT ~ alpha.2 + Species, data=faoCountryJoin, FUN = sum)
head(faoCountries)

############ Merge CITES, RFMO, and FAO ###########

head(CITES_Spec)
names(CITES_Spec)[names(CITES_Spec) == "Exporter"] <- "alpha.2"

head(rfmoCountries)
names(rfmoCountries)[names(rfmoCountries) == "CaptureMT"] <- "RFMO_MT"

head(faoCountries)
names(faoCountries)[names(faoCountries) == "CaptureMT"] <- "FAO_MT"


#Merge RFMO and CITES trade DBs
CITESRFMO <- merge(CITES_Spec, rfmoCountries, by=c("alpha.2", "Species"), all.x=TRUE, all.y=TRUE)
head(CITESRFMO)

CITESRFMOFAO <- merge(CITESRFMO, faoCountries, by=c("alpha.2", "Species"), all.x=TRUE, all.y=TRUE)
head(CITESRFMOFAO)

# Make NAs = to zero 
CITESRFMOFAO$CITESMT[is.na(CITESRFMOFAO$CITESMT)] <- 0
CITESRFMOFAO$RFMO_MT[is.na(CITESRFMOFAO$RFMO_MT)] <- 0
CITESRFMOFAO$FAO_MT[is.na(CITESRFMOFAO$FAO_MT)] <- 0
head(CITESRFMOFAO)

##### Compare Values of each #######
CITESRFMOFAO$CIT.RFM <- CITESRFMOFAO$CITESMT/CITESRFMOFAO$RFMO_MT
CITESRFMOFAO$CIT.FAR <- CITESRFMOFAO$CITESMT/CITESRFMOFAO$FAO_MT
CITESRFMOFAO$RFM.CIT <- CITESRFMOFAO$RFMO_MT/CITESRFMOFAO$CITESMT
CITESRFMOFAO$FAO.CIT <- CITESRFMOFAO$FAO_MT/CITESRFMOFAO$CITESMT

head(CITESRFMOFAO)
write.csv(CITESRFMOFAO, "240220-CITES_RFMO_FAO_TradeComps.csv", row.names=FALSE)


#### Look at countries cumulatively ######

CRF_Country <- CITESRFMOFAO[, c(1:5)]
head(CRF_Country)
tail(CRF_Country)

CountryTrades <- aggregate(cbind(CITESMT, RFMO_MT, FAO_MT) ~ alpha.2, data=CRF_Country, FUN = sum)
head(CountryTrades)
tail(CountryTrades)

## Compare Values of each
CountryTrades$CIT.RFM <- CountryTrades$CITESMT/CountryTrades$RFMO_MT
CountryTrades$CIT.FAR <- CountryTrades$CITESMT/CountryTrades$FAO_MT
CountryTrades$RFM.CIT <- CountryTrades$RFMO_MT/CountryTrades$CITESMT
CountryTrades$FAO.CIT <- CountryTrades$FAO_MT/CountryTrades$CITESMT

head(CountryTrades)
write.csv(CountryTrades, "240220-CITES_RFMO_FAO_CountryTradeComps.csv", row.names=FALSE)


#### Look at species cumulatively ######
SpeciesTrades <- aggregate(cbind(CITESMT, RFMO_MT, FAO_MT) ~ Species, data=CRF_Country, FUN = sum)
head(SpeciesTrades)
tail(SpeciesTrades)

## Compare Values of each 
SpeciesTrades$CIT.RFM <- SpeciesTrades$CITESMT/SpeciesTrades$RFMO_MT
SpeciesTrades$CIT.FAR <- SpeciesTrades$CITESMT/SpeciesTrades$FAO_MT
SpeciesTrades$RFM.CIT <- SpeciesTrades$RFMO_MT/SpeciesTrades$CITESMT
SpeciesTrades$FAO.CIT <- SpeciesTrades$FAO_MT/SpeciesTrades$CITESMT

head(SpeciesTrades)
write.csv(SpeciesTrades, "240220-CITES_RFMO_FAO_SpeciesTradeComps.csv", row.names=FALSE)

