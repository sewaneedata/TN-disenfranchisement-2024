# PURPOSE:
# look at voter turnout amongst different income groups by county

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
library(tidyr)

# upload census csv
census <- read_csv('census.csv')
View(census)

# get data for all counties in Tennessee
acs <- get_acs(geography = "county",
               #geography = "tract",
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
tmap_mode("plot")
tm_shape(pivot_acs) +
  tm_polygons(alpha = 0.8, col = c('poverty_income', 'unemployed'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE) 

# upload tn voting by county csv
# this comes from secretary of state for year 2022
# add the link here
votes <- read_csv('tn_county_votes.csv')
View(votes)

# clean up votes
votes <- votes %>% 
  filter(!is.na(`County:`),
         `County:` != 'Total:') %>% 
  select(-'...9')

# in pivot_acs, delete ", Tennessee" and create a new column "County" from "NAME"
pivot_acs <- pivot_acs %>%
  mutate(County = gsub(" County, Tennessee", "", NAME), .after = NAME)
View(pivot_acs)

# in votes, remove ", Tennessee" and rename the column "County"
votes <- votes %>%
  rename(County = "County:")
View(votes)

# join df columns by "County" to create census_votes
census_votes <- left_join(votes, pivot_acs, by = "County")
View(census_votes)

# create income column 
# census_votes <- census_votes %>%
#   pivot_longer(cols = starts_with('btwn'),
#                values_to = 'income')
#   # mutate(income = btwn10kand19999 +
#   #           btwn20kand34999 +
#   #           btwn35kand49999 +
#   #           btwn50kand74999 +
#   #           btwn75kand99999 +
#   #           over100k)
# View(census_votes)
        #PROBLEMS:
          #turned btwn columns into new column, which means they're gone
          #does not include under 10k and over 100k
          #represents the number of people, not the number of people in each income group

# # Convert Income to a factor with specified order
# census_votes <- census_votes %>%
#   mutate(income = factor(income, ordered = TRUE))
# View(census_votes)

# # arrange census_votes from poorest to richest voter turnout based on Income categories
# arranged_census_votes <- census_votes %>%
#   arrange(income)
# View(arranged_census_votes)

# # in order to make the next heat map sing census_votes, you must
# # determine the maximum length of the columns
# target_length <- max(sapply(census_votes, length))
# target_length
# 
# # function to pad columns with NA to match target length
# pad_column <- function(income, target_length) {
#   length(income) <- target_length
#   return(income)
# }
# 
# # apply the padding function to each column in the data frame
# census_votes_padded <- as.data.frame(lapply(census_votes, pad_column, target_length = target_length))
# 
# # replace NA values with appropriate value (NA) if needed
# census_votes_padded[is.na(census_votes_padded)] <- NA
# # not NA, 0
# #census_votes_padded[is.na(census_votes_padded)] <- 0
# 
# # view the padded data frame
# print(census_votes_padded)
# View(census_votes_padded)

# convert dataframe into a sf type object
census_votes <- st_sf(census_votes)

# # heat map for income
# tmap_mode("plot")
# tm_shape(census_votes) +
#   tm_polygons(alpha = 0.8, col = c('income'), id = "NAME") +
#   # make several layered maps that you can toggle between
#   tm_facets(as.layers = TRUE) 

#heat map for income
tmap_mode("plot")
tm_shape(census_votes) +
  tm_polygons(alpha = 0.8, col = c('lessthan10k', 'btwn10kand19999', 'btwn20kand34999', 'btwn35kand49999', 'btwn50kand74999', 'btwn75kand99999', 'over100k'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE) 

# #create income class conditions
# census_votes <- census_votes %>%
#   mutate(
#     income_category = case_when(
#       lessthan10k > 0 | btwn10kand19999 > 0 | btwn20kand34999 > 0 ~ "low income",
#       btwn35kand49999 > 0 | btwn50kand74999 > 0 | btwn75kand99999 > 0 ~ "middle income",
#       over100k > 0 ~ "high income"
#     )
#   )
# View(census_votes)

# create the income category columns by combining values from the specified columns
census_votes <- census_votes %>%
  mutate(
    low_income = lessthan10k + btwn10kand19999 + btwn20kand34999,
    middle_income = btwn35kand49999 + btwn50kand74999 + btwn75kand99999,
    high_income = over100k
  )
View(census_votes)

# heat map for number of people in each income category
tmap_mode("view")
tm_shape(census_votes) +
  tm_polygons(alpha = 0.8, col = c('low_income', 'middle_income', 'high_income'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE)

# heat map for voter turnout
tmap_mode("view")
tm_shape(census_votes) +
  tm_polygons(alpha = 0.8, col = c('Voter Turnout:'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE) 





