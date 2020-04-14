# Anoop Nanda
# April 2020
# Converting county Level Covid-19 data to HRRs

# Loading Package libraries
library(readr)
library(dplyr)
library(stringr)

# loading and modifying data
geocorr2014 <- read_csv("geocorr2014.csv") # loads crosswalk 
nyt = read_csv("us-counties.csv") # loads nytimes data
nyt = subset(nyt, nyt$date == max(nyt$date))
nyt$state = setNames(state.abb, state.name)[nyt$state] #abbreviates states
nyt$fips = str_pad(nyt$fips, 5, pad = 0) # pads 0s onto FIPS codes, for merging
output0title = paste("nytdata_", as.character(max(nyt$date)), ".csv", sep= "")
write.csv(nyt, output0title)

# NYC and KC cases and deaths aren't separated out by county, grabbing those numbers for the nyt data
if ("New York City" %in% nyt$county) {
  NYCcases = nyt$cases[which(nyt$county == "New York City")]
  NYCdeaths = nyt$deaths[which(nyt$county == "New York City")]
} else {
  NYCcases = 0
  NYCdeaths = 0
}
if ("Kansas City" %in% nyt$county) {
  KCcases = nyt$cases[which(nyt$county == "Kansas City")]
  KCdeaths = nyt$deaths[which(nyt$county == "Kansas City")]
} else {
  KC = 0
  KC = 0
}

# merging crosswalk and NYT data
full = merge(geocorr2014, nyt, by.x = "county", by.y = "fips", all.x = TRUE)
full$date = max(nyt$date)
full[is.na(full)]=0
full$pop10 <- as.numeric(full$pop10)

# Calculating county level populations
full1 = full %>%
  group_by(county) %>%
  mutate(countypop = sum(pop10)) 

full2 = full1 %>%
  group_by(hrr) %>%
  mutate(hrrpop = sum(pop10))

# NYC and KC data modifications - grabbing data for the metro regions, replacing empty county columns in the merged (full) dataset
NYC = subset(full2, county == 36005 | county == 36047 | county == 36061 | county == 36081 | county == 36085)
NYC$countypop = sum(NYC$pop10)
NYC$cases = sum(NYC$cases)+NYCcases
NYC$deaths = sum(NYC$deaths)+NYCdeaths

KC = subset(full2, county == 29047 | county == 29037 | county == 29095 | county == 29165)
KC$countypop = sum(KC$pop10)
KC$cases = sum(KC$cases)+KCcases
KC$deaths = sum(KC$deaths)+KCdeaths

NYCandKCcounties = c(36005, 36047, 36061, 36081, 36085, 29047, 29037, 29095, 29165) # removing duplicate counties from the full dataset
full3 = full2[!full2$county %in% NYCandKCcounties,]
full3 = rbind(KC,NYC,full3)

# calculating and returning case and death rates by county, per capita and per 100k
full3$countycaserate = full3$cases/full3$countypop 
full3$countydeathrate = full3$deaths/full3$countypop
full3$countycaserate100k = full3$countycaserate*100000
full3$countydeathrate100k = full3$countydeathrate*100000
output1filename = paste("casesanddeathsbycounty","_",as.character(max(nyt$date)),".csv",sep="")
write.csv(full3, output1filename)

# calculating and returning case and death rates by hrr, per capita and per 100k 
full4 = full3
full4$hrrcaserate = full4$countycaserate*(full4$pop10/full4$hrrpop) # multiples the county rates by the proportion of the county living in a certain hrr 
full4$hrrdeathrate = full4$countydeathrate*(full4$pop10/full4$hrrpop)

full5 = full4 %>% # adds up allocated hrr rates from various counties. 
  group_by(hrr) %>%
  mutate(hrrcaserate = sum(hrrcaserate)) %>%
  mutate(hrrdeathrate = sum(hrrdeathrate)) 

full5$hrrcaserate100k = full5$hrrcaserate*100000
full5$hrrdeathrate100k = full5$hrrdeathrate*100000
output1v2filename = paste("casesanddeathsbyhrr_uncollapsed", "_", as.character(max(nyt$date)), ".csv", sep="")
write.csv(full5, output1v2filename)
full6 = distinct(full5, hrr, .keep_all = TRUE)
full7 = full6 %>%
  select(hrr, hrrname, hrrpop, hrrcaserate, hrrdeathrate, hrrcaserate100k, hrrdeathrate100k, date)
output2filename = paste("casesanddeathsbyHRR","_",as.character(max(nyt$date)),".csv",sep="")
write.csv(full7, output2filename)
