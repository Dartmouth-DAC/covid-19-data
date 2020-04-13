# Anoop Nanda
# March 2020
# Mapping COVID cases and Deaths by HRR

# Loading Package libraries
library(tmap)
library(tmaptools)
library(sf)
library(leaflet)
library(readr)
library(dplyr)
library(readr)
library(stringr)
library(tidycensus)
library(colorRamps)
# loading and modifying data
NYCcases = 20011
NYCdeaths = 280
KCcases = 50
KCdeaths = 0
  
geocorr2014 <- read_csv("geocorr2014.csv")
geocorr2014$cntyname = str_remove(geocorr2014$cntyname, "Parish")
nyt = read_csv("nyt.csv")
nyt$STATE = setNames(state.abb, state.name)[nyt$STATE]
nyt$combined = paste(nyt$COUNTY,nyt$STATE)
full = merge(geocorr2014, nyt, by.x = "cntyname", by.y = "combined", all.x = TRUE)
full[is.na(full)]=0
full$pop10 <- as.numeric(full$pop10)
full$afact = as.numeric(full$afact)
full$Cases_Weighted = full$CASES*full$afact
full$Deaths_Weighted = full$DEATHS*full$afact
test1 = full %>%
  group_by(hrr) %>%
  mutate(sumcases = sum(Cases_Weighted)) %>%
  mutate(sumdeaths = sum(Deaths_Weighted)) %>%
  mutate(sumpop = sum(pop10))

GreaterNY = subset(test1, hrr == 297 | hrr == 308 | hrr == 301 | hrr == 303)
GreaterNY$sumcases = sum(GreaterNY$sumcases)+NYCcases
GreaterNY$sumdeaths = sum(GreaterNY$sumdeaths)+NYCdeaths
GreaterNY$sumpop = sum(GreaterNY$pop10)
GreaterNY = distinct(GreaterNY, hrr, .keep_all = TRUE)

GreaterKC = subset(test1, hrr == 268)
GreaterKC$sumcases = GreaterKC$sumcases+KCcases
GreaterKC$sumdeaths = GreaterKC$sumdeaths+KCdeaths
GreaterKC = distinct(GreaterKC, hrr, .keep_all = TRUE)

test2 = subset(test1, hrr != 268 | hrr != 297 | hrr != 301 | hrr != 303 | hrr != 308)
test2 = rbind(GreaterNY, GreaterKC, test2)
test3 = distinct(test2, hrr, .keep_all = TRUE)
test3$casepop = test3$sumcases/test3$sumpop
test3$deathpop = test3$sumdeaths/test3$sumpop
test3$casepop100k = test3$casepop*100000
test3$deathpop100k = test3$deathpop*100000

shp <- st_read("C:/Users/anoop/Desktop/Skinner/R/Map 1.5/hrr_bdry_projected/hrr_bdry_projected.shp",
               stringsAsFactors = FALSE) 
county <- st_read("C:/Users/anoop/Desktop/Skinner/R/Map 1.5/acs_2012_2016_county_us_B27001/acs_2012_2016_county_us_B27001.shp",
                  stringsAsFactors = FALSE) %>%
  mutate(STFIPS = stringr::str_sub(GEOID, 1, 2)) 

county <- subset(county, STFIPS != "02" & STFIPS != "15")

states <- county %>%
  aggregate_map(by = "STFIPS")

shp$HRRNUM <- str_pad(shp$HRRNUM, 3, pad = "0") #adds a zero to FIPS if there is only four didgets

shp <- merge(shp, test3, by.x= "HRRNUM", by.y = "hrr")

hrrmap3 = tm_shape(shp)+
  tm_polygons("casepop100k", border.alpha = .5, palette = "BuPu", midpoint = NA, alpha = 1, title ="", 
              border.col = "white",
              style = "fixed", breaks = c(0, 2.859259,  5.614199, 10.896547, 43.228112, 624.3149), midpoint = 3) +
  tm_layout(main.title = "Cases of COVID-19, per 100,000", 
            main.title.position = c("center", "top"), 
            inner.margins = c(.05, .05, .15, .20), title.position = c("center", "top"), 
            legend.position = c("right", "bottom"))

filler3 = tm_shape(states)+tm_fill(col = "grey51")

borders3 = tm_shape(states)+tm_borders(col = "black", alpha = 1)

filler3+hrrmap3+borders3

hrrmap4 = tm_shape(shp)+
  tm_polygons("deathpop100k", border.alpha = .5, palette = "BuPu", midpoint = NA, alpha = 1, title ="", 
              border.col = "white", midpoint = 0,
              style = "fixed", breaks = c(0.00000000, 0.01278993, 0.18579254, 0.89207109, 4.552801))+
              
  tm_layout(main.title = "Deaths from COVID-19, per 100,000", 
            main.title.position = c("center", "top"), 
            inner.margins = c(.05, .05, .15, .20), title.position = c("center", "top"), 
            legend.position = c("right", "bottom"))

filler4 = tm_shape(states)+tm_fill(col = "grey51")

borders4 = tm_shape(states)+tm_borders(col = "black", alpha = 1)

filler4+hrrmap4+borders4

nofalse = test3$casepop100k[is.na(test3$deathpop100k) == FALSE]