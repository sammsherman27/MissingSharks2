
## Looking at traffic seizure data

library(dplyr)
library(tidyr)
library(lubridate)
library(cowplot)



known_formats <- c("Ymd")

TrafficforR <- TrafficforR %>% 
  mutate(Year = parse_date_time(TrafficforR$Date, known_formats) %>% year())


unique(TrafficforR$Scientific_Name)
unique(TrafficforR$Unit_Weight)

TrafficFiltered <- TrafficforR %>%
  filter(Scientific_Name %in% c("Isurus paucus", "Sphyrna lewini", "Pristis", "Isurus", "Isurus oxyrinchus", "Sphyrna zygaena",
                                "Alopias", "Rhincodon typus", "Carcharhinus longimanus", "Pristis microdon", "Alopias vulpinus",
                                "Alopias pelagicus", "Alopias superciliosus", "Lamna nasus", "Pristidae", "Carcharhinus falciformis",
                                "Manta", "Carcharodon carcharias", "Pristis pristis", "Cetorhinus maximus", "Manta alfredi", "Manta birostris", "Mobula",
                                "Mobula japanica", "Mobula tarapacana"))


####### Make Figures with Colour Coding for Continent ###### 
head(TrafficFiltered)
unique(TrafficFiltered$Country_of_Incident)

TrafficCont <- TrafficFiltered %>% mutate(Continent = case_when(
  Country_of_Incident == "China" ~ "Asia",
  Country_of_Incident == "Netherlands" ~ "Europe",
  Country_of_Incident == "Indonesia" ~ "Asia",
  Country_of_Incident == "Mexico" ~ "North America",
  Country_of_Incident == "Brazil" ~ "South America",
  Country_of_Incident == "Sri Lanka" ~ "Asia",
  Country_of_Incident == "Hong Kong" ~ "Asia",
  Country_of_Incident == "Ecuador" ~ "South America",
  Country_of_Incident == "France" ~ "Europe",
  Country_of_Incident == "United Kingdom of Great Britain and Northern Ireland" ~ "Europe",
  Country_of_Incident == "Philippines" ~ "Asia",
  Country_of_Incident == "Hungary" ~ "Europe",
  Country_of_Incident == "Spain" ~ "Europe",
  Country_of_Incident == "Germany" ~ "Europe",
  Country_of_Incident == "Taiwan Province of China" ~ "Asia",
  Country_of_Incident == "Sao Tome and Principe" ~ "Africa",
  Country_of_Incident == "Gabon" ~ "Africa",
  Country_of_Incident == "United States of America" ~ "North America",
  Country_of_Incident == "Ireland" ~ "Europe",
  Country_of_Incident == "Poland" ~ "Europe",
  Country_of_Incident == "Peru" ~ "South America",
  Country_of_Incident == "Saint Barthelemy" ~ "North America",
  Country_of_Incident == "Chile" ~ "South America",
  Country_of_Incident == "India" ~ "Asia",
  Country_of_Incident == "Mozambique" ~ "Africa",
  Country_of_Incident == "Viet Nam" ~ "Asia",
  Country_of_Incident == "Australia" ~ "Oceania",
  Country_of_Incident == "Malaysia" ~ "Asia",
  Country_of_Incident == "Benin" ~ "Africa",
  Country_of_Incident == "Guinea" ~ "Africa",
  Country_of_Incident == "Canada" ~ "North America"
))
head(TrafficCont)
str(TrafficCont)

write.csv(TrafficCont, "240313-TRAFFICSeizureData.csv", row.names=FALSE)

TrafficCont2 <- TrafficCont %>% mutate(Scientific_Name = case_when(
    Scientific_Name == "Manta" ~ "Mobula spp.",
    Scientific_Name == "Mobula" ~ "Mobula spp.",
    Scientific_Name == "Manta alfredi" ~ "Mobula alfredi",
    Scientific_Name == "Manta birostris" ~ "Mobula birostris",
    Scientific_Name == "Mobula japanica" ~ "Mobula mobular",
    Scientific_Name == "Isurus" ~ "Isurus spp.",
    Scientific_Name == "Alopias" ~ "Alopias spp.",
    Scientific_Name == "Pristis" ~ "Pristidae",
    Species == "pristis" ~ "Pristis pristis",
    Scientific_Name == "Pristis microdon" ~ "Pristis pristis",
    TRUE ~ Scientific_Name
  ))

### Update weights when not included, convert grams to Kg
TrafficCont2 <- TrafficCont2 %>% mutate(QuantityWeight = case_when(
  Subject == "103 kgs Dried Manta Gills Seized" ~ 51.5,
  Subject == "50kg of Ocean thresher shark meat and 5 thresher shark fins seized at Cod Bay Fishing harbour, six people arrested" ~ 50,
  Subject == "20,196 shark fins (550 kg) seized in Richmond, BC - company fined" ~ 275,
  Unit_Weight == "Grams" ~ 0.6,
  TRUE ~ as.numeric(QuantityWeight) # Convert QuantityWeight column to numeric for other cases
))


Alopiidae <- TrafficCont2 %>%
  filter(Family == "Alopiidae")

Carcharhinidae <- TrafficCont2 %>%
  filter(Family == "Carcharhinidae")
head(Carcharhinidae)

Cetorhinidae <- TrafficCont2 %>%
  filter(Family == "Cetorhinidae")

Lamnidae <- TrafficCont2 %>%
  filter(Family == "Lamnidae")

Myliobatidae <- TrafficCont2 %>%
  filter(Family == "Myliobatidae")

Pristidae <- TrafficCont2 %>%
  filter(Family == "Pristidae")

Rhincodontidae <- TrafficCont2 %>%
  filter(Family == "Rhincodontidae")

Sphyrnidae <- TrafficCont2 %>%
  filter(Family == "Sphyrnidae")

OWT <- TrafficCont2 %>%
  filter(Scientific_Name == "Carcharhinus longimanus")


ContinentColours <- c("Africa" = "#1b9e77", "Asia" = "#7570b3", "Europe" = "#66a61e", "North America" = "#d95f02",
                      "Oceania" = "#e6ab02", "South America" = "#e7298a")


ggplot(OWT, aes(x = Date, y = Weight_in_tonnes, color = Commodity_Type)) +
  geom_point(size = 3, shape = 21, fill = "black", stroke = 2) +
  labs(title = "Oceanic Whitetip Seizure by Commodity Type and Country", x = "Date", y = "Weight (t)") +
  theme_minimal() +
  facet_wrap(~Country_of_Incident, scales = "free_y")

AlopFig <- ggplot(Alopiidae, aes(x = Date, y = QuantityWeight, color = Continent, shape=Scientific_Name)) +
  scale_color_manual(values = ContinentColours, guide = "none") +
  geom_point(size = 3) +
  labs(title = "Alopiidae") +
  theme_minimal() +  
  theme(legend.position = c(0.5, 0.5), legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
AlopFig

CarchFig <- ggplot(Carcharhinidae, aes(x = Date, y = QuantityWeight, color = Continent, shape=Scientific_Name)) +
  scale_color_manual(values = ContinentColours, guide = "none") +
  geom_point(size = 3) +
  labs(title= "Carcharhinidae", y = "Weight (Kg)") +
  theme_minimal() +
  theme(legend.position = c(0.5, 0.5), legend.title = element_blank()) +
  theme(axis.title.x = element_blank())
CarchFig


CetorFig <- ggplot(Cetorhinidae, aes(x = Date, y = QuantityWeight, color = Continent)) +
  scale_color_manual(values = ContinentColours, guide = "none") +
  geom_point(size = 3, shape = 20, stroke = 2) +
  labs(title = "Cetorhinidae", y = "Weight (Kg)") +
  theme_minimal() +  
  theme(axis.title.x = element_blank())
CetorFig

LamnidFig <- ggplot(Lamnidae, aes(x = Date, y = QuantityWeight, color = Continent, shape=Scientific_Name)) +
  scale_color_manual(values = ContinentColours, guide = "none") +
  geom_point(size = 3) +
  labs(title = "Lamnidae") +
  theme_minimal() +
  theme(legend.position = c(0.5, 0.5), legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
LamnidFig

MylioFig <- ggplot(Myliobatidae, aes(x = Date, y = QuantityWeight, color = Continent, shape=Scientific_Name)) +
  scale_color_manual(values = ContinentColours, guide = "none") +
  geom_point(size = 3) +
  labs(title = "Mobulidae") +
  theme_minimal() +
  theme(legend.position = c(0.5, 0.5), legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
MylioFig

PrisFig <- ggplot(Pristidae, aes(x = Date, y = QuantityWeight, color = Continent)) +
  scale_color_manual(values = ContinentColours, guide = "none") +
  geom_point(size = 2, shape = 20, stroke = 2) +
  labs(title = "Pristidae") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
PrisFig

RhincoFig <- ggplot(Rhincodontidae, aes(x = Date, y = QuantityWeight, color = Continent)) +
  scale_color_manual(values = ContinentColours, guide = "none") +
  geom_point(size = 2, shape = 20, stroke = 2) +
  labs(title = "Rhincodon typus") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
RhincoFig

SphyrFig <- ggplot(Sphyrnidae, aes(x = Date, y = QuantityWeight, color = Continent, shape = Scientific_Name)) +
  scale_color_manual(values = ContinentColours, guide = "none") +
  geom_point(size = 3) +
  labs(title = "Sphyrnidae", y = "Weight (Kg)") +
  theme_minimal() +
  theme(legend.position = c(0.5, 0.5), legend.title = element_blank()) +
  theme(axis.title.x = element_blank())
SphyrFig


allseizures <- plot_grid(CarchFig, LamnidFig, AlopFig, SphyrFig, MylioFig, RhincoFig, CetorFig, PrisFig,  ncol=3)
allseizures


#### Sum Seizure volumes
## By Family
FamSeizes <- aggregate(QuantityWeight ~ Family, data=TrafficCont2, FUN=sum)
FamSeizes
FamSeizeOrder <- FamSeizes %>%
  arrange(desc(QuantityWeight))
FamSeizeOrder

## By Species
SpecSeizes <- aggregate(QuantityWeight ~ Scientific_Name, data=TrafficCont2, FUN=sum)
SpecSeizes
SpecSeizeOrder <- SpecSeizes %>%
  arrange(desc(QuantityWeight))
SpecSeizeOrder

SeizeTot <- sum(SpecSeizes$QuantityWeight)
SeizeTot

## By Continent
ContSeizes <- aggregate(QuantityWeight ~ Continent, data=TrafficCont2, FUN=sum)
ContSeizes
ContSeizeOrder <- ContSeizes %>%
  arrange(desc(QuantityWeight))
ContSeizeOrder

## By Country
CountrySeizes <- aggregate(QuantityWeight ~ Country_of_Incident, data=TrafficCont2, FUN=sum)
CountrySeizes
CountrySeizeOrder <- CountrySeizes %>%
  arrange(desc(QuantityWeight))
CountrySeizeOrder


