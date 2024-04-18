
## Porbeagle Trade Stats from FAO 
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)


PorbeagleTrade <- read.csv(file.choose(), stringsAsFactors=TRUE)


FAOPorbeagleTrade <- PorbeagleTrade %>%
  pivot_longer(cols = !c(Reporting_Country, Trade_Flow),
               names_to = "Year",
               values_to = "MT")

## Remove X at beginning of year
FAOPorbeagleTrade$Year <- substr(FAOPorbeagleTrade$Year, 2,nchar(FAOPorbeagleTrade$Year))

## Only include data from 2012
PorbeagleTrade_2012 <- FAOPorbeagleTrade %>%
  filter(Year >= 2012)
head(PorbeagleTrade_2012)

### Combine all import and export across years to determine highest contributing countries
countrytops <- aggregate(MT ~ Reporting_Country + Trade_Flow , data=PorbeagleTrade_2012, FUN = sum)
head(countrytops)
tail(countrytops)


write.csv(countrytops_fix, "240326-FAOPorbTrade.csv", row.names=FALSE)
### Check top contributing countries 


head(PorbeagleTrade_2012)
#Remove total
PorbeagleTrade_2012 <- subset(PorbeagleTrade_2012, Reporting_Country !="Totals - Tonnes – net product weight")

## Rename countries contributing less than 5% total import and export to "Other"
PorbTop <- PorbeagleTrade_2012 %>%
  mutate(Reporting_Country = case_when(
    Reporting_Country == "Austria" ~ "Other",
    Reporting_Country == "Belgium" ~ "Other",
    Reporting_Country == "Croatia" ~ "Other",
    Reporting_Country == "Cyprus" ~ "Other",
    Reporting_Country == "Czechia" ~ "Other",
    Reporting_Country == "Estonia" ~ "Other",
    Reporting_Country == "Faroe Islands" ~ "Other",
    Reporting_Country == "Finland" ~ "Other",
    Reporting_Country == "Germany" ~ "Other",
    Reporting_Country == "Greece" ~ "Other",
    Reporting_Country == "Hungary" ~ "Other",
    Reporting_Country == "Iceland" ~ "Other",
    Reporting_Country == "Ireland" ~ "Other",
    Reporting_Country == "Luxembourg" ~ "Other",
    Reporting_Country == "Norway" ~ "Other",
    Reporting_Country == "Poland" ~ "Other",
    Reporting_Country == "Romania" ~ "Other",
    Reporting_Country == "Serbia" ~ "Other",
    Reporting_Country == "Sweden" ~ "Other",
    Reporting_Country == "United Kingdom" ~ "Other",
    TRUE ~ Reporting_Country
  ))

flows <- c("Exports", "Imports")
PorbTop2 <- PorbTop[PorbTop$Trade_Flow %in% flows, ]


## Plot trade
ggplot(PorbTop2, aes(x = Trade_Flow, y = MT, fill = Reporting_Country)) +
  geom_bar(stat = "identity") +
  labs(
    #title = "Porbeagle Global Import and Export",
       x = "Year",
       y = "Metric Tonnes",
       fill = "Country") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_minimal()+
  facet_grid(~Year)

