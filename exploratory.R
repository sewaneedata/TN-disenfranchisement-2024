#load in libraries
library(sf)
library(rnaturalearth)
library(remotes)
library(rnaturalearthhires)
library(rnaturalearthdata)
library(tmap)
library(osmdata)
library(tidycensus)
library(dplyr)
library(readr)

# upload csv
census <- read_csv('census.csv')
View(census)

# create df with census tract data from acs
# rename UniqueIDs
first_acs <- get_acs(geography = "tract",
                     state = "TN",
                     #county = "Hamilton",
                     variables = c(
                       #poverty income
                       poverty_income = "B23024_002",
                       #white 
                       white = "B02001_002", #"B02003_003", #detailed race codes start here
                       #african american
                       afr_amr = "B02001_003", #"B02003_004",
                       #indigenous
                       nativeamr = "B02001_004", #"B02003_005",
                       #asian
                       asian = "B02001_005", #"B02003_006",
                       #pacific islander
                       pac_isl = "B02001_006", #"B02003_007",
                       #other race
                       otherrace = "B02001_007", #"B02003_008",
                       #household income over the last 12 months follows
                       lessthan10k = "B25122_002",
                       btwn10kand19999 = "B25122_019",
                       btwn20kand34999 = "B25122_036",
                       btwn35kand49999 = "B25122_053",
                       btwn50kand74999 = "B25122_070",
                       btwn75kand99999 = "B25122_087",
                       over100k = "B25122_104"
                     ),
                     summary_var = "B01003_001",
                     year = 2022,
                     geometry = TRUE)

