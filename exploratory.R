# PURPOSE:
# exploring our datasets to notice patterns 

#load in libraries
library(sf)
# library(rnaturalearth)
# library(remotes)
# library(rnaturalearthhires)
# library(rnaturalearthdata)
library(tmap)
# library(osmdata)
library(tidycensus)
# library(dplyr)
# library(readr)
# library(tidyr)
library(tidyverse)
# library(readxl)
# library(ggplot2)

# upload census csv
census <- read_csv('data/census.csv')
View(census)

# get data for all counties in Tennessee
# choose which tracts to keep
acs <- get_acs(geography = "county",
               # geography = "tract",
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
# from secretary of state for year 2022
votes <- read_csv('data/tn_county_votes.csv')
View(votes)

# clean votes
votes_clean <- votes %>%
  filter(!is.na(`County:`),
         `County:` != 'Total:') %>%
  select(-'...9')

# in pivot_acs, delete ", Tennessee" and create a new column "County" from "NAME"
pivot_acs <- pivot_acs %>%
  mutate(County = gsub(" County, Tennessee", "", NAME), .after = NAME)
View(pivot_acs)

# in votes, remove ", Tennessee" and rename the column "County"
votes_clean2 <- votes_clean %>%
  rename(County = "County:")
View(votes)

# join df columns by "County" to create census_votes
census_votes <- left_join(votes_clean2, pivot_acs, by = "County")
View(census_votes)

# create income column
# census_votes <- census_votes %>%
#   pivot_longer(cols = starts_with('btwn'),
#                values_to = 'income')
# View(census_votes)
#PROBLEMS:
  #turned btwn columns into new column, which means they're gone
  #does not include under 10k and over 100k
  #represents the number of people, not the number of people in each income group

# convert dataframe into a sf type object
census_votes_sf <- st_sf(census_votes)

# heat map for race
tmap_mode("plot")
tm_shape(census_votes_sf) +
  tm_polygons(alpha = 0.8, col = c('white', 'afr_amr', 'nativeamr', 'asian', 'pac_isl', 'otherrace'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE) 

# INCOME

# heat map for income
tmap_mode("plot")
tm_shape(census_votes_sf) +
  tm_polygons(alpha = 0.8, col = c('lessthan10k', 'btwn10kand19999', 'btwn20kand34999', 'btwn35kand49999', 'btwn50kand74999', 'btwn75kand99999', 'over100k'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE)

# create the income category columns by combining values from the specified columns
# categories determined by literature estimates in review
census_votes_income <- census_votes_sf %>%
  mutate(low_income = lessthan10k + btwn10kand19999 + btwn20kand34999,
    middle_income = btwn35kand49999 + btwn50kand74999,
    high_income = btwn75kand99999 + over100k) %>% 
  # total no. of households that have an income, census
  mutate(income_tally = low_income + middle_income + high_income) %>% 
  # create columns with the percentage of people in each income category
  mutate(low_income_percent = (low_income / income_tally) * 100,
    middle_income_percent = (middle_income / income_tally) * 100,
    high_income_percent = (high_income / income_tally) * 100) %>% 
  # create a column with the label of the highest percentage income category for each county
  mutate(highest_income_cat = case_when(
    low_income_percent >= middle_income_percent & low_income_percent >= high_income_percent ~ "low income",
    middle_income_percent >= low_income_percent & middle_income_percent >= high_income_percent ~ "middle income",
    high_income_percent >= low_income_percent & high_income_percent >= middle_income_percent ~ "high income"))
View(census_votes_income)

# heat map for number of people in each income category
tmap_mode("view")
tm_shape(census_votes_income) +
  tm_polygons(alpha = 0.8, col = c('low_income_percent', 'middle_income_percent', 'high_income_percent'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE)

# heat map for number of people in each income category
tmap_mode("view")
tm_shape(census_votes_income) +
  tm_polygons(alpha = 0.8, col = c('low_income', 'middle_income', 'high_income'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE)

# heat map for county income category majority
tmap_mode("view")
tm_shape(census_votes_income) +
  tm_polygons(alpha = 0.8, col = c('highest_income_cat'), id = "NAME") # +
  # make several layered maps that you can toggle between
  # tm_facets(as.layers = TRUE)

#census_votes
#create a column with the number of people in each income category
#create a column with the percentage of people in each income category
#create a column with the label of the highest percentage income category
#create dataset that has county, income category, and voter turnout rate (keep as percent)
#plot average rates

# VOTER TURNOUT

# remove % sign from values in voter turnout
census_votes_income_turnout <- census_votes_income %>%
  mutate(`Voter Turnout (%):` = gsub("%", "", `Voter Turnout:`)) %>% 
  # convert the 'Voter Turnout:' column to numeric if it's not already
  mutate(`Voter Turnout (%):` = as.numeric(`Voter Turnout (%):`)) %>% 
  # create voter turnout in percentile intervals so they're not intervals of 2% each
  mutate(voter_turnout_interval = cut(`Voter Turnout (%):`,
                                      breaks = c(0.00, 25.00, 50.00, 75.00, 100.00),
                                      labels = c("0-25%", "25-50%", "50-75%", "75-100%"),
                                      include.lowest = TRUE))
View(census_votes_income_turnout)

# heat map for voter turnout
tmap_mode("view")
tm_shape(census_votes_income_turnout) +
  tm_polygons(alpha = 0.8, col = c('Voter Turnout:'), id = "NAME") # +
  # make several layered maps that you can toggle between
  # tm_facets(as.layers = TRUE)

# heat map for voter turnout
tmap_mode("view")
tm_shape(census_votes_income_turnout) +
  tm_polygons(alpha = 0.8, col = c('Voter Turnout (%):'), id = "NAME") # +
  # make several layered maps that you can toggle between
  # tm_facets(as.layers = TRUE)

# plot average voter turnout rates by income category via bar chart
ggplot(census_votes_income_turnout, aes(x = highest_income_cat, y = `Voter Turnout (%):`)) +
  geom_bar(stat = "summary", fun = "mean", fill = "blue", alpha = 0.7) +
  #geom_bar()#stat = "summary", fun = "mean", fill = "blue", alpha = 0.7) +
  labs(title = "Average Voter Turnout Rate by Highest Income Category",
       x = "Highest Income Category",
       y = "Average Voter Turnout Rate (%)") +
  theme_minimal()

# plot average voter turnout rates by income category via line graph
# ggplot(census_votes_income_turnout, aes(x = highest_income_cat, y = `Voter Turnout (%):`)) +
#   geom_line() +
#   #geom_bar()#stat = "summary", fun = "mean", fill = "blue", alpha = 0.7) +
#   labs(title = "Average Voter Turnout Rate by Highest Income Category",
#        x = "Highest Income Category",
#        y = "Average Voter Turnout Rate (%)") +
#   theme_minimal()

# read in crime csv
crime <- read_csv('data/tennessee.csv')
View(crime)
names(crime)

# remove empty columns and NAs
crime_clean <- crime %>%
  select(-...13) %>%
  select(-...14) %>%
  select(-...15) %>%
  select(-...16) %>%
  filter(!is.na(`Metropolitan/Nonmetropolitan`))
View(crime_clean)

# read in corrections csvs
  # todc statistical abstract 2022
    # average sentence length by location as of june 30, 2022
    # C/S/F refers to conspiracy/ solicitation/ facilitation
    # 85% refers to legislation that states that a convicted defendant must serve minimum of 85% of       their sentence b4 eligible for parole
# name <- read_excel("data/name.xlsx")
crime_type_inmate_numbers <- read_csv("data/corrections1.csv")
View(crime_type_inmate_numbers)
  # todc statistical abstract 2023
    # felon population by county of conviction as of June 30, 2022
county_incarceration_numbers <- read_csv("data/corrections2.csv")
View(county_incarceration_numbers)

# create 'Offense Type' category column (violent/non-violent)
crime_type_inmate_numbers_clean <- crime_type_inmate_numbers %>%
  mutate(offense_category = 
            ifelse(`Offense Type` %in% 
            c( 'Murder', 'Forcible Sex Offense', 'Non-Forcible Sex
               Offense', 'Forgery/Fraud'),  'violent', 'non-violent'  
            ))
View(crime_type_inmate_numbers_clean)

# make offense type categorical (non-violent = 0, violent = 1) for visualization
incarceration_by_felony <- crime_type_inmate_numbers_clean %>%
  select(offense_category, `TDOC Inhouse Inmates`, `TDOC Backup Inmates`, `Locally Sentenced Inmates`,`Statewide Inmates`) %>%
  pivot_longer(!offense_category) %>%
  group_by( offense_category ) %>%
  summarize( total_inmates = sum(value) )

library(ggthemes)
# create a bar graph that visualizes number of offenses per offense type
ggplot(incarceration_by_felony, aes(x = offense_category, y=total_inmates)) +
  geom_col(fill = 'orange') +
  labs(title = "Incarceration by Felony",
       x = "Violent or Non-Violent",
       y = "Count") +
  theme_fivethirtyeight() +  # Apply theme_fivethirtyeight
  scale_x_discrete(labels = c("non-violent" = "Non-Violent", "violent" = "Violent"))
# print (violent_nonviolent)

# create the new column 'TDOC (%)' and remove '%' sign in tdoc, backup, local, systemwide
county_incarceration_numbers_clean <- county_incarceration_numbers %>%
  mutate(`TDOC (%)` = gsub("%", "", `TDOC %`)) %>% 
  mutate(`Backup (%)` = gsub("%", "", `Backup %`)) %>%
  mutate(`Local (%)` = gsub("%", "", `Local %`)) %>%
  mutate(`Systemwide (%)` = gsub("%", "", `Systemwide %`))
View(county_incarceration_numbers_clean)

# join census votes and incarceration by county columns by "County"
census_votes_corrections <- left_join(census_votes_income_turnout, county_incarceration_numbers_clean, by = "County")
View(census_votes_corrections)

# heat map for incarceration (tdoc, backup, local)
#tmap_mode("plot")
tmap_mode("view")
tm_shape(census_votes_corrections) +
  #tm_polygons(alpha = 0.8, col = c('TDOC #', 'Backup #', 'Local #' ), id = "NAME") +
  tm_polygons(alpha = 0.8, col = c('TDOC #', 'Backup #'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE)

# heat map for incarceration (statewide today)
tmap_mode("view")
tm_shape(census_votes_corrections) +
  tm_polygons(alpha = 0.8, col = c('Systemwide #' ), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE)

#categorical voter turnout
#how do these things p
#the dependent variable (categorical or real voter turnout)
#the independent/control variables (race (proportion) income unemployment)
#partisanship (state or federal, congress)-- how many representatives come from each, what party
  #tennessee legislature (makeup of parties)-- not congressional
  #state senate and house makeup: 27 republicans 6 dems (senate), 75 republicans 24 dems (house)
#dep: voter tunout
#ind: race (prop), unemployment, poverty 

# # RESULTS
# 
# # map crime and incarceration rates
# tmap_mode("plot")
# tm_shape(census_votes_clean_category2) +
#   tm_polygons(alpha = 0.8, col = c("corrections_data_1","crime_clean"), id = "NAME") +
#   # make several layered maps that you can toggle between
#   tm_facets(as.layers = TRUE)
# 
# # plotting incarceration map
# ggplot(data = census_votes_clean_category2, aes(x = census, y = census_votes, group = group, fill = crime)) +
#   geom_polygon (color = "black") +
#   scale_fill_gradient(name = "Incarceration Rate", low = "lightblue", high = "darkblue",
#                       na.value = "grey50", guide = "legend") +
#   labs(title = "Incarceration Rates by County") +
#   theme_minimal()
# 
# #bar chart for incarceration rates by felony
# ggplot(data = census_votes, aes(x = correction_data_1, y = census, group = group, fill = rate)) +
#   geom_bar(color = "blue") +
#   scale_fill_gradient(name = "Incarceration Rate", low = "lightblue", high = "darkblue",
#                       na.value = "grey50", guide = "legend") +
#   labs(title = "Incarceration Rates by Felony") +
#   theme_minimal()

