# Anoop Nanda
# April 2020
# Getting a cumulative dataset

# Loading Package libraries
library(readr)
library(dplyr)
library(stringr)

# loading and modifying data
outcounty.file = NULL # creates empty file to place all the cumulative results in
outhrr.file = NULL
geocorr2014 <- read_csv("geocorr2014.csv") # loads crosswalk 
nytwhole = read_csv("us-counties.csv") # loads nytimes 
nytwhole$state = setNames(state.abb, state.name)[nytwhole$state] # abbreviates states
nytwhole$fips = str_pad(nytwhole$fips, 5, pad = 0) # pads 0s onto FIPS codes, for merging
dates = (unique(nytwhole$date))

for (i in 1:(length(dates)-1)){ # iterates through all the distinct dates, creating 
  nyt = subset(nytwhole, nytwhole$date == dates[i])
  # output0title = paste("outputs\\nytdata_", as.character(max(nyt$date)), ".csv", sep= "")
  # write.csv(nyt, output0title)
  
  # NYCcumulative and KCcumulative cases and deaths aren't separated out by county, grabbing those numbers for the nyt data
  if ("New York City" %in% nyt$county) {
    NYCcumulativecases = nyt$cases[which(nyt$county == "New York City")]
    NYCcumulativedeaths = nyt$deaths[which(nyt$county == "New York City")]
  } else {
    NYCcumulativecases = 0
    NYCcumulativedeaths = 0
  }
  if ("Kansas City" %in% nyt$county) {
    KCcumulativecases = nyt$cases[which(nyt$county == "Kansas City")]
    KCcumulativedeaths = nyt$deaths[which(nyt$county == "Kansas City")]
  } else {
    KCcumulativecases = 0
    KCcumulativedeaths = 0
  }
  
  # merging crosswalk and NYT data
  fullcumulative = merge(geocorr2014, nyt, by.x = "county", by.y = "fips", all.x = TRUE)
  fullcumulative$date = max(nyt$date)
  fullcumulative[is.na(fullcumulative)]=0
  fullcumulative$pop10 <- as.numeric(fullcumulative$pop10)

  # Calculating county level populations
  fullcumulative1 = fullcumulative %>%
    group_by(county) %>%
    mutate(countypop = sum(pop10)) 
  
  fullcumulative2 = fullcumulative1 %>%
    group_by(hrr) %>%
    mutate(hrrpop = sum(pop10))
  
  # NYCcumulative and KCcumulative data modifications - grabbing data for the metro regions, replacing empty county columns in the merged (fullcumulative) dataset
  NYCcumulative = subset(fullcumulative2, county == 36005 | county == 36047 | county == 36061 | county == 36081 | county == 36085)
  NYCcumulative$countypop = sum(NYCcumulative$pop10)
  NYCcumulative$cases = sum(NYCcumulative$cases)+NYCcumulativecases
  NYCcumulative$deaths = sum(NYCcumulative$deaths)+NYCcumulativedeaths
  
  KCcumulative = subset(fullcumulative2, county == 29047 | county == 29037 | county == 29095 | county == 29165)
  KCcumulative$countypop = sum(KCcumulative$pop10)
  KCcumulative$cases = sum(KCcumulative$cases)+KCcumulativecases
  KCcumulative$deaths = sum(KCcumulative$deaths)+KCcumulativedeaths
  
  NYCandKCcounties = c(36005, 36047, 36061, 36081, 36085, 29047, 29037, 29095, 29165) # removing duplicate counties from the fullcumulative dataset
  fullcumulative3 = fullcumulative2[!fullcumulative2$county %in% NYCandKCcounties,]
  fullcumulative3 = rbind(KCcumulative,NYCcumulative,fullcumulative3)
  
  # calculating and returning case and death rates by county, per capita and per 100k
  fullcumulative3$countycaserate = fullcumulative3$cases/fullcumulative3$countypop 
  fullcumulative3$countydeathrate = fullcumulative3$deaths/fullcumulative3$countypop
  fullcumulative3$countycaserate100k = fullcumulative3$countycaserate*100000
  fullcumulative3$countydeathrate100k = fullcumulative3$countydeathrate*100000
  # output1filename = paste("outputs\\casesanddeathsbycounty","_",as.character(max(nyt$date)),".csv",sep="") # uncomment to generate a separate CSV for every day of data
  # write.csv(fullcumulative3, output1filename)
  outcounty.file <- rbind(fullcumulative3, outcounty.file)

  # calculating and returning case and death rates by hrr, per capita and per 100k 
  fullcumulative4 = fullcumulative3
  fullcumulative4$hrrcaserate = fullcumulative4$countycaserate*(fullcumulative4$pop10/fullcumulative4$hrrpop) # multiples the rates by the allocation factors in the crosswalk
  fullcumulative4$hrrdeathrate = fullcumulative4$countydeathrate*(fullcumulative4$pop10/fullcumulative4$hrrpop)
  
  fullcumulative5 = fullcumulative4 %>% # adds up allocated hrr rates from various counties. 
    group_by(hrr) %>%
    mutate(hrrcaserate = sum(hrrcaserate)) %>%
    mutate(hrrdeathrate = sum(hrrdeathrate)) 
  
  fullcumulative5$hrrcaserate100k = fullcumulative5$hrrcaserate*100000
  fullcumulative5$hrrdeathrate100k = fullcumulative5$hrrdeathrate*100000
  fullcumulative6 = distinct(fullcumulative5, hrr, .keep_all = TRUE)
  fullcumulative7 = fullcumulative6 %>%
    select(hrr, hrrname, hrrpop, hrrcaserate, hrrdeathrate, hrrcaserate100k, hrrdeathrate100k, date)
  # output2filename = paste("outputs\\casesanddeathsbyHRR","_",as.character(max(nyt$date)),".csv",sep="") # uncomment to generate a separate CSV for every day of data
  # write.csv(fullcumulative7, output2filename)
  outhrr.file = rbind(outhrr.file, fullcumulative7)
}

write.csv(outcounty.file, file = "CasesandDeathsbyCounty.csv")
write.csv(outhrr.file, file = "CasesandDeathsbyHRR.csv")
