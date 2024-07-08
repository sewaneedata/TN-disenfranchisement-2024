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
library(ggplot2)

  # CENSUS

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

# in pivot_acs, delete ", Tennessee" and create a new column "County" from "NAME"
pivot_acs <- pivot_acs %>%
  mutate(County = gsub(" County, Tennessee", "", NAME), .after = NAME)
View(pivot_acs)

  # VOTER TURNOUT

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

# in votes, remove ", Tennessee" and rename the column "County"
votes <- votes %>%
  rename(County = "County:")
View(votes)

# remove % sign from values in voter turnout
census_votes <- census_votes %>%
  mutate(`Voter Turnout (%):` = gsub("%", "", `Voter Turnout:`))
View(census_votes)

# convert the 'Voter Turnout:' column to numeric if it's not already
census_votes <- census_votes %>%
  mutate(`Voter Turnout (%):` = as.numeric(`Voter Turnout (%):`))

# create voter turnout in percentile intervals so they're not intervals of 2% each
census_votes <- census_votes %>%
  mutate(voter_turnout_interval = cut(`Voter Turnout (%):`, 
                                      breaks = c(0.00, 25.00, 50.00, 75.00, 100.00), 
                                      labels = c("0-25%", "25-50%", "50-75%", "75-100%"),
                                      include.lowest = TRUE))
View(census_votes)

# heat map for voter turnout (specific percentage)
tmap_mode("view")
tm_shape(census_votes) +
  tm_polygons(alpha = 0.8, col = c('Voter Turnout:'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE) 

# heat map for voter turnout (in percent groups)
tmap_mode("view")
tm_shape(census_votes) +
  tm_polygons(alpha = 0.8, col = c('Voter Turnout (%):'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE)

  # JOIN CENSUS & VOTER TURNOUT

# join df columns by "County" to create census_votes
census_votes <- left_join(votes, pivot_acs, by = "County")
View(census_votes)

# convert dataframe into a sf type object
census_votes <- st_sf(census_votes)

  # CENSUS (INCOME & EMPLOYMENT)

# heat map for income level below poverty line and unemployed population 
tmap_mode("plot")
tm_shape(pivot_acs) +
  tm_polygons(alpha = 0.8, col = c('poverty_income', 'unemployed'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE) 

#heat map for income
tmap_mode("plot")
tm_shape(census_votes) +
  tm_polygons(alpha = 0.8, col = c('lessthan10k', 'btwn10kand19999', 'btwn20kand34999', 'btwn35kand49999', 'btwn50kand74999', 'btwn75kand99999', 'over100k'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE) 

# create the income category columns by combining values from the specified columns
census_votes <- census_votes %>%
  mutate(
    low_income = lessthan10k + btwn10kand19999 + btwn20kand34999,
    middle_income = btwn35kand49999 + btwn50kand74999 + btwn75kand99999,
    high_income = over100k
  )
View(census_votes)

# total income household populations (no. of households), census
census_votes <- census_votes %>% 
  mutate(income_tally = low_income + middle_income + high_income)
View(census_votes)

# create columns with the percentage of people in each income category
census_votes <- census_votes %>%
  mutate(
    low_income_percent = (low_income / income_tally) * 100,
    middle_income_percent = (middle_income / income_tally) * 100,
    high_income_percent = (high_income / income_tally) * 100
  )
View(census_votes)

# create a column with the label of the highest percentage income category
census_votes <- census_votes %>%
  mutate(highest_income_cat = case_when(
    low_income_percent >= middle_income_percent & low_income_percent >= high_income_percent ~ "low income",
    middle_income_percent >= low_income_percent & middle_income_percent >= high_income_percent ~ "middle income",
    high_income_percent >= low_income_percent & high_income_percent >= middle_income_percent ~ "high income"
  ))
View(census_votes)

  # CENSUS (RACE)

#heat map for race
tmap_mode("plot")
tm_shape(census_votes) +
  tm_polygons(alpha = 0.8, col = c('white', 'afr_amr', 'nativeamr', 'asian', 'pac_isl', 'otherrace'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE) 

  # CENSUS & VOTER TURNOUT

# plot average voter turnout rates by income category via bar chart
ggplot(census_votes, aes(x = highest_income_cat, y = `Voter Turnout (%):`)) +
  geom_bar(stat = "summary", fun = "mean", fill = "blue", alpha = 0.7) +
  #geom_bar()#stat = "summary", fun = "mean", fill = "blue", alpha = 0.7) +
  labs(title = "Average Voter Turnout Rate by Highest Income Category",
       x = "Highest Income Category",
       y = "Average Voter Turnout Rate (%)") +
  theme_minimal()

# plot average voter turnout rates by income category via bar chart
ggplot(census_votes, aes(x = highest_income_cat, y = `Voter Turnout (%):`)) +
  geom_bar(stat = "summary", fun = "mean", fill = "blue", alpha = 0.7) +
  #geom_bar()#stat = "summary", fun = "mean", fill = "blue", alpha = 0.7) +
  labs(title = "Average Voter Turnout Rate by Highest Income Category",
       x = "Highest Income Category",
       y = "Average Voter Turnout Rate (%)") +
  theme_minimal()







