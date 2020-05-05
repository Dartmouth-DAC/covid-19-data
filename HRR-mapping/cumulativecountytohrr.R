# Anoop Nanda
# April 2020
# Getting a cumulative dataset

# Loading Package libraries
library(readr)
library(dplyr)
library(stringr)
library(zoo)

# hardcoded values
casegrowth_limiter = 25 # number of cases/deaths before starting to calculate growth/deathrates
deathgrowth_limiter = 10
rollaveragevalue = 7 # number of days included in rolling averages
alignaveragevalue = "right" # sets where the rolling average should be aligned, left, right, or center
rollsumvalue = 14 # number of days included in rolling sums of new cases/deaths
alignsumvalue = "right" # sets where the rolling sum should be aligned, left, right, or center
growthvalue = 7 # number of days included in growthrates
newcasegrowth_limiter = 5 
newdeathgrowth_limiter = 1
NYCandKCcounties = c(36005, 36047, 36061, 36081, 36085, 29047, 29037, 29095, 29165) # FIPS for counties in NYC and KC, the geographic exemptions
NYCcounties = c(36005, 36047, 36061, 36081, 36085) 
KCcounties = c(29047, 29037, 29095, 29165)

# loading and modifying data
outcounty.file = NULL # creates empty file to place all the cumulative results in
outhrr.file = NULL
geocorr2014 <- read_csv("geocorr2014.csv") # loads crosswalk 
nytwhole = read_csv("us-counties.csv") # loads nytimes 
nytwhole$state = setNames(state.abb, state.name)[nytwhole$state] # abbreviates states
nytwhole$fips = str_pad(nytwhole$fips, 5, pad = 0) # pads 0s onto FIPS codes, for merging
dates = (unique(nytwhole$date))

for (i in 1:(length(dates))){ # iterates through all the distinct dates, creating 
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

  # Calculating county level and hrr populations
  fullcumulative1 = fullcumulative %>%
    group_by(county) %>%
    mutate(countypop = sum(pop10)) 
  
  fullcumulative2 = fullcumulative1 %>%
    group_by(hrr) %>%
    mutate(hrrpop = sum(pop10))
  
  # NYCcumulative and KCcumulative data modifications - grabbing data for the metro regions, replacing empty county columns in the merged (fullcumulative) dataset
  NYCcumulative = subset(fullcumulative2, county %in% NYCcounties)
  NYCcumulative$countypop = sum(NYCcumulative$pop10)
  NYCcumulative$cases = sum(NYCcumulative$cases)+NYCcumulativecases
  NYCcumulative$deaths = sum(NYCcumulative$deaths)+NYCcumulativedeaths
  
  KCcumulative = subset(fullcumulative2, county %in% KCcounties)
  KCcumulative$countypop = sum(KCcumulative$pop10)
  KCcumulative$cases = sum(KCcumulative$cases)+KCcumulativecases
  KCcumulative$deaths = sum(KCcumulative$deaths)+KCcumulativedeaths
  
  fullcumulative3 = fullcumulative2[!fullcumulative2$county %in% NYCandKCcounties,] # removes counties in NYC and KC (the geographic exemptions)
  fullcumulative3 = rbind(KCcumulative,NYCcumulative,fullcumulative3) # adds in updated data for NYC and KC with metro region data. 
  
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
  fullcumulative4$accuracy_index = (fullcumulative4$pop10/fullcumulative4$hrrpop)*as.numeric(fullcumulative4$afact)
  
  fullcumulative5 = fullcumulative4 %>% # adds up allocated hrr rates from various counties. 
    group_by(hrr) %>%
    mutate(hrrcaserate = sum(hrrcaserate)) %>%
    mutate(hrrdeathrate = sum(hrrdeathrate)) %>%
    mutate(accuracy_index = sum(accuracy_index))
  
  fullcumulative5$hrrcaserate100k = fullcumulative5$hrrcaserate*100000
  fullcumulative5$hrrdeathrate100k = fullcumulative5$hrrdeathrate*100000
  fullcumulative6 = distinct(fullcumulative5, hrr, .keep_all = TRUE)
  fullcumulative7 = fullcumulative6 %>%
    select(hrr, hrrname, hrrpop, hrrcaserate, hrrdeathrate, hrrcaserate100k, hrrdeathrate100k, date, accuracy_index)
  # output2filename = paste("outputs\\casesanddeathsbyHRR","_",as.character(max(nyt$date)),".csv",sep="") # uncomment to generate a separate CSV for every day of data
  # write.csv(fullcumulative7, output2filename)
  fullcumulative8 = subset(fullcumulative7, hrr != "Hospital referral regions (2016)") # removes extra text rows 
  outhrr.file = rbind(outhrr.file, fullcumulative8)
}

outhrr.file = outhrr.file %>% # produces case data and death data by hrr, calculates growth rates once the value a week before the latest data hits the limiter
  group_by(hrr) %>%
  arrange(date) %>%
  mutate(hrrcases = hrrpop*hrrcaserate) %>%
  mutate(hrrdeaths = hrrpop*hrrdeathrate) %>%
  mutate(lag_date = lag(date, n = growthvalue, fill = NA)) %>% # creates columns with date, cases, and deaths lagged by growthvalue, to calculate growthrates
  mutate(lag_cases = lag(hrrcases, n = growthvalue, fill = NA)) %>% 
  mutate(lag_deaths = lag(hrrdeaths, n = growthvalue, fill = NA)) %>%
  mutate(casegrowthrate = ifelse(lag_cases >= casegrowth_limiter, 
                               (hrrcases/lag_cases)^(1/growthvalue)-1,
                               NA)) %>%
  mutate(deathgrowthrate = ifelse(lag_deaths >= deathgrowth_limiter, 
                                  (hrrdeaths/lag_deaths)^(1/growthvalue)-1,
                                  NA)) %>% 
  mutate(hrrcases_rollingaverage = rollmean(hrrcases, rollaveragevalue, fill = NA, align = alignaveragevalue)) %>% # finds rolling averages
  mutate(hrrdeaths_rollingaverage = rollmean(hrrdeaths, rollaveragevalue, fill = NA, align = alignaveragevalue)) %>%
  mutate(hrrcaserate_rollingaverage = rollmean(hrrcaserate, rollaveragevalue, fill = NA, align = alignaveragevalue)) %>%
  mutate(hrrdeathrate_rollingaverage = rollmean(hrrdeathrate, rollaveragevalue, fill = NA, align = alignaveragevalue)) %>%
  mutate(casegrowrthrate_rollingaverage = rollmean(casegrowthrate, rollaveragevalue, fill = NA, align = alignaveragevalue)) %>%
  mutate(deathgrowthrate_rollingaverage = rollmean(deathgrowthrate, rollaveragevalue, fill = NA, align = alignaveragevalue)) %>%
  mutate(newcases = hrrcases - lag(hrrcases, n = 1, fill = NA)) %>%
  mutate(newdeaths = hrrdeaths - lag(hrrdeaths, n = 1, fill = NA)) %>%
  mutate(newcases_rollingsum = rollsum(newcases, rollsumvalue, fill = NA, align = alignsumvalue)) %>%
  mutate(newdeaths_rollingsum = rollsum(newdeaths, rollsumvalue, fill = NA, align = alignsumvalue)) %>%
  mutate(newcases_rollingaverage = rollmean(newcases, rollaveragevalue, fill = NA, align = alignaveragevalue)) %>%
  mutate(newdeaths_rollingaverage = rollmean(newdeaths, rollaveragevalue, fill = NA, align = alignaveragevalue)) %>%
  mutate(newcases_rate100k = newcases/hrrpop*100000) %>%
  mutate(newdeaths_rate100k = newdeaths/hrrpop*100000) %>%
  mutate(newcases_rollingsum_rate100k = newcases_rollingsum/hrrpop*100000) %>%
  mutate(newdeaths_rollingsum_rate100k = newdeaths_rollingsum/hrrpop*100000) %>%
  mutate(lag_newcases = lag(newcases, n = growthvalue, fill = NA)) %>% 
  mutate(lag_newdeaths = lag(newdeaths, n = growthvalue, fill = NA)) %>%
  mutate(newcasesgrowthrate = ifelse(lag_newcases >= newcasegrowth_limiter, 
                                 (newcases/lag_newcases)^(1/growthvalue)-1,
                                 NA)) %>%
  mutate(newdeathsgrowthrate = ifelse(lag_newdeaths >= newdeathgrowth_limiter, 
                                  (newdeaths/lag_newdeaths)^(1/growthvalue)-1,
                                  NA))  

outhrr.file = outhrr.file %>% # creates rankings for assorted variables, by date. 
  group_by(date) %>% 
  mutate(hrrcases_rank = order(order(hrrcases, decreasing=TRUE))) %>%
  mutate(hrrdeaths_rank = order(order(hrrdeaths, decreasing=TRUE))) %>%
  mutate(caserate_rank = order(order(hrrcaserate, decreasing=TRUE))) %>%
  mutate(deathrate_rank = order(order(hrrdeathrate, decreasing=TRUE))) %>%
  mutate(casegrowthrate_rank = order(order(casegrowthrate, decreasing=TRUE))) %>%
  mutate(deathgrowthrate_rank = order(order(deathgrowthrate, decreasing=TRUE))) %>%  
  mutate(hrrcases_rollingaverage_rank = order(order(hrrcases_rollingaverage, decreasing=TRUE))) %>%  
  mutate(hrrdeaths_rollingaverage_rank = order(order(hrrdeaths_rollingaverage, decreasing=TRUE))) %>%  
  mutate(hrrcaserate_rollingaverage_rank = order(order(hrrcaserate_rollingaverage, decreasing=TRUE))) %>%  
  mutate(hrrdeathrate_rollingaverage_rank = order(order(hrrdeathrate_rollingaverage, decreasing=TRUE))) %>%  
  mutate(casegrowrthrate_rollingaverage_rank = order(order(casegrowrthrate_rollingaverage, decreasing=TRUE))) %>%  
  mutate(deathgrowthrate_rollingaverage_rank = order(order(deathgrowthrate_rollingaverage, decreasing=TRUE))) %>%
  mutate(newcases_rank = order(order(newcases, decreasing=TRUE))) %>%
  mutate(newdeaths_rank = order(order(newdeaths, decreasing=TRUE))) %>%
  mutate(newcases_rollingsum_rank = order(order(newcases_rollingsum, decreasing=TRUE))) %>%
  mutate(newdeaths_rollingsum_rank = order(order(newdeaths_rollingsum, decreasing=TRUE))) %>%
  mutate(newcases_rank = order(order(newcases, decreasing=TRUE))) %>%
  mutate(newdeaths_rank = order(order(newdeaths, decreasing=TRUE))) %>%
  mutate(newcases_rollingsum_rank = order(order(newcases_rollingsum, decreasing=TRUE))) %>%
  mutate(newdeaths_rollingsum_rank = order(order(newdeaths_rollingsum, decreasing=TRUE))) %>%
  mutate(newcases_rollingaverage_rank = order(order(newcases_rollingaverage, decreasing=TRUE))) %>%
  mutate(newdeaths_rollingaverage_rank = order(order(newdeaths_rollingaverage, decreasing=TRUE))) %>%
  mutate(newcases_rate100k_rank = order(order(newcases_rate100k, decreasing=TRUE))) %>%
  mutate(newdeaths_rate100k_rank = order(order(newdeaths_rate100k, decreasing=TRUE))) %>%
  mutate(newcases_rollingsum_rate100k_rank = order(order(newcases_rollingsum_rate100k, decreasing=TRUE))) %>%
  mutate(newdeaths_rollingsum_rate100k_rank = order(order(newdeaths_rollingsum_rate100k, decreasing=TRUE))) %>%
  mutate(newcasesgrowthrate_rank = order(order(newcasesgrowthrate, decreasing=TRUE))) %>%
  mutate(newdeathsgrowthrate_rank = order(order(newdeathsgrowthrate, decreasing=TRUE))) 
  
write.csv(outcounty.file, file = "CasesandDeathsbyCounty.csv")
write.csv(outhrr.file, file = "CasesandDeathsbyHRR.csv")

