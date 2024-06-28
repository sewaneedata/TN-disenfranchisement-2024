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
acs <- get_acs(geography = "tract",
                     state = "TN",
                     #county = all(),
                     variables = c(
                       #income in the past 12 months below poverty level
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
                       over100k = "B25122_104",
                       #employment
                       employed = "B23025_004",
                       unemployed = "B23025_005"
                     ),
                     summary_var = "B01003_001",
                     year = 2022,
                     geometry = TRUE)

# pivot to create columns with each variable for each unique census tract
pivot_acs <- acs %>% 
  select(-moe) %>% 
  pivot_wider(names_from = variable, values_from = estimate)

# heat map for income level below poverty line and unemployed population 
tmap_mode("view")
tm_shape(pivot_acs) +
  tm_polygons(alpha = 0.8, col = c('poverty_income', 'unemployed'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE) 

# # making map
# acs$geometry
# 
# #from tmap; picture of tn, plots
# tm_shape(acs) + 
#   tm_polygons()
# 
# #OR
# tmap_mode('view') 
# tm_shape(acs) +
#   tm_polygons(alpha = 0.3, id = 'NAME')
# 
# 
# 
# 