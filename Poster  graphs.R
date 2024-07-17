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
census <- read_csv('data/census.csv')
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
                 white_ = "B02001_002", #"B02003_003", #detailed race codes start here
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
  pivot_wider(names_from = variable, values_from = estimate) %>%
  # in pivot_acs, delete ", Tennessee" and create a new column "County" from "NAME"
  mutate(County = gsub(" County, Tennessee", "", NAME), .after = NAME)
View(pivot_acs)

# VOTER TURNOUT

# upload tn voting by county csv
# this comes from secretary of state for year 2022
# add the link here
votes <- read_csv('data/tn_county_votes.csv')
View(votes)

# clean up votes
votes_clean <- votes %>% 
  filter(!is.na(`County:`),
         `County:` != 'Total:') %>% 
  select(-'...9') %>% 
  # in votes, remove ", Tennessee" and rename the column "County"
  rename(County = "County:")
View(votes_clean)

# JOIN CENSUS & VOTER TURNOUT

# join df columns by "County" to create census_votes
census_votes <- left_join(votes_clean, pivot_acs, by = "County")
View(census_votes)

# convert dataframe into a sf type object
census_votes_sf <- st_sf(census_votes)
View(census_votes_sf)

# remove % sign from values in voter turnout
census_votes_clean <- census_votes_sf %>%
  mutate(`Voter Turnout (%):` = gsub("%", "", `Voter Turnout:`)) %>% 
  # convert the 'Voter Turnout:' column to numeric if it's not already
  mutate(`Voter Turnout (%):` = as.numeric(`Voter Turnout (%):`)) %>% 
  # create voter turnout in percentile intervals so they're not intervals of 2% each
  mutate(voter_turnout_interval = cut(`Voter Turnout (%):`, 
                                      breaks = c(0.00, 25.00, 50.00, 75.00, 100.00), 
                                      labels = c("0-25%", "25-50%", "50-75%", "75-100%"),
                                      include.lowest = TRUE))
View(census_votes_clean)

# heat map for voter turnout (specific percentage)
tmap_mode("view")
tmap_mode("plot")
tm_shape(census_votes_clean) +
  tm_polygons(alpha = 0.8, col = c('Voter Turnout:'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE) 

# heat map for voter turnout (in percent groups)
tmap_mode("view")
tmap_mode("plot")
tm_shape(census_votes_clean) +
  tm_polygons(alpha = 0.8, col = c("Voter Turnout (%):"), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE)

# CENSUS (INCOME & EMPLOYMENT)

# heat map for income level below poverty line and unemployed population 
tmap_mode("plot")
tm_shape(census_votes_clean) +
  tm_polygons(alpha = 0.8, col = c('poverty_income', 'unemployed'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE) 

#heat map for income
tmap_mode("plot")
tm_shape(census_votes_clean) +
  tm_polygons(alpha = 0.8, col = c('lessthan10k', 'btwn10kand19999', 'btwn20kand34999', 'btwn35kand49999', 'btwn50kand74999', 'btwn75kand99999', 'over100k'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE) 

census_votes_clean_category1 <- census_votes_clean %>%
  # create the income category columns by combining values from the specified columns
  mutate(
    low_income = lessthan10k + btwn10kand19999 + btwn20kand34999,
    middle_income = btwn35kand49999 + btwn50kand74999 + btwn75kand99999,
    high_income = over100k) %>% 
  # total income household populations (no. of households), census
  mutate(income_tally = low_income + middle_income + high_income) %>% 
  # create columns with the percentage of people in each income category
  mutate(
    low_income_perc = (low_income / income_tally) * 100,
    middle_income_perc = (middle_income / income_tally) * 100,
    high_income_perc = (high_income / income_tally) * 100) %>% 
  # create a column with the label of the highest percentage income category
  mutate(highest_income_cat = case_when(
    low_income_perc >= middle_income_perc & low_income_perc >= high_income_perc ~ "low income",
    middle_income_perc >= low_income_perc & middle_income_perc >= high_income_perc ~ "middle income",
    high_income_perc >= low_income_perc & high_income_perc >= middle_income_perc ~ "high income"
  ))
View(census_votes_clean_category1)

# CENSUS (RACE)

#heat map for race
tmap_mode("plot")
tm_shape(census_votes_clean_category1) +
  tm_polygons(alpha = 0.8, col = c('white_', 'afr_amr', 'nativeamr', 'asian', 'pac_isl', 'otherrace'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE) 

census_votes_clean_category2 <- census_votes_clean_category1 %>% 
  group_by(County) %>% 
  # total household populations (no. of households), census
  mutate(race_tally = sum(white_, afr_amr, nativeamr, asian, pac_isl, otherrace)) %>%
  # total people of color household populations (no. of households), census
  mutate(poc_tally = sum(afr_amr, nativeamr, asian, pac_isl, otherrace)) %>%
  # total percent of color household populations (perc. of households), census
  mutate(poc_perc = poc_tally / race_tally) %>% 
  # plot average voter turnout rates by race via bar chart
  # reshape the data from wide to long format
  pivot_longer(cols = c(white_, afr_amr, nativeamr, asian, pac_isl, otherrace), 
               names_to = "race_code", 
               values_to = "total_race_tally") 
View(census_votes_clean_category2)

# # plot average voter turnout rates by race via bar chart
# ggplot(census_votes, aes(x = race_tally, y = `Voter Turnout:`)) +
#   geom_bar(stat = "summary", fun = "mean", fill = "blue", alpha = 0.7) +
#   labs(title = "Average Voter Turnout Rate by Racial Demographic",
#        x = "Race",
#        y = "Average Voter Turnout Rate (%)") +
#   theme_minimal()

# calculate the average voter turnout rates by race
avg_perc_voter_turnout_race <- census_votes_clean_category2 %>%
  group_by(race_code) %>%
  summarize(average_turnout = mean(`Voter Turnout (%):`, na.rm = TRUE))
View(avg_perc_voter_turnout_race)

# plot average voter turnout rates by race via bar chart
ggplot(census_votes_clean_category2, aes(x = race_code, y = `Voter Turnout (%):`)) +
  stat_summary(fun = "mean", geom = "bar", fill = "blue", alpha = 0.7) +
  labs(title = "Average Voter Turnout Rate by Racial Demographic",
       x = "Race",
       y = "Average Voter Turnout Rate (%)") +
  theme_minimal()

# CENSUS & VOTER TURNOUT

ggplot(census_votes, aes(x = `Voter Turnout (%):`), binwidth = 20) +
  # geom_histogram() +
  #geom_bar()#stat = "summary", fun = "mean", fill = "blue", alpha = 0.7) +
  labs(title = "Average Voter Turnout Rate (per TN County)",
       x = "Average Voter Turnout Rate (%)") +
  theme_minimal()

# plot average voter turnout rates by income category via bar chart
ggplot(census_votes, aes(x = highest_income_cat, y = `Voter Turnout (%):`)) +
  geom_bar(stat = "summary", fun = "mean", fill = "blue", alpha = 0.7) +
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


# In counties where there's a higher proportion of people of color, is voter turnout higher/lower?


# making a map of incarceration by county

# insert link to the dataset
# read in corrections csv
# todc statistical abstract 2022
# Average Sentence Length by Location as of June 30, 2022
# C/S/F refers to conspiracy/ solicitation/ facilitation
# Under 85%, a convicted defendant must serve min. of 85% of their sentence b4 eligible for parole
# name <- read_excel("data/name.xlsx")
corrections_data_1 <- read_csv("data/corrections1.csv")

# insert link to the dataset
# load in the corrections data which is from
# todc statistical abstract 2023
# Felon Population by County of Conviction as of June 30, 2022
corrections_data_2 <- read_csv("data/corrections2.csv")

# seems we cannot make incarceration by county because we do not have the data
tmap_mode("plot")
tm_shape(corrections_data_2) +
  tm_polygons(alpha = 0.8, col = c("correction_data_1","crime"), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE)


# make a bar chart of incarceration by felony


library(readr)
corrections1 <- read_csv("corrections1.csv")
View(corrections1)

library(readr)
corrections2 <- read_csv("corrections2.csv")
View(corrections2)
# clean the data to remove NA rows
corrections_data_1_clean <- corrections1 %>% 
  
  
  
  
  
  
  
  
  
  
  
  library(dplyr)

# Assign corrections_data_1 to corrections_data_1_clean
corrections_data_1_clean <- corrections1


corrections_data_1_clean <- na.omit(corrections_data_1_clean)


library(dplyr)

# Example data manipulation
corrections_data_1_clean <- corrections_data_1_clean %>%
  mutate( offense_category = ifelse(`Offense Type` %in% c( 'Murder', 'Forcible Sex Offense', 'Non-Forcible Sex Offense', 'Forgery/Fraud'),  'violent', 'non-violent'  ) )




library(dplyr)

# Example data manipulation
incarceration_by_felony<- corrections_data_1_clean %>%
  select(offense_category, `TDOC Inhouse Inmates`, `TDOC Backup Inmates`, `Locally Sentenced Inmates`,`Statewide Inmates`) %>%
  pivot_longer(!offense_category) %>%
  group_by( offense_category ) %>%
  summarize( total_inmates = sum(value) )


#hgfhdhdhdhdh
library(ggplot2)
# Create a bar graph
p <- ggplot(incarceration_by_felony, aes(x = offense_category, y=total_inmates)) +
  geom_col(fill = 'orange') +
  labs(title = "Incarceration by Felony",
       x = "Violent or Non-Violent",
       y = "Count") +
  theme_fivethirtyeight() +  # Apply theme_fivethirtyeight
  scale_x_discrete(labels = c("non-violent" = "Non-Violent", "violent" = "Violent"))
print (p)


#Showing the Locally sentenced Inmates 

library(ggplot2)

# Create a bar graph
p <- ggplot(corrections_data_1_clean, aes(x = offense_category, y=`Locally Sentenced Inmates`)) +
  geom_col(fill = 'orange') +
  labs(title = "Incarceration by Felony",
       x = "Violent or Non-Violent",
       y = "Incarcenrated ") +
  theme_fivethirtyeight() +  # Apply theme_fivethirtyeight
  scale_x_discrete(labels = c("non-violent" = "Non-Violent", "violent" = "Violent"))

print (p)



library(dplyr)
library(ggplot2)

# # Example data manipulation
# incarceration_by_felony <- corrections_data_1_clean %>%
#   group_by(offense_category) %>%
#   summarize(total_locally_sentenced = sum(`Locally Sentenced Inmates`))
# 
# # Calculate percentages
# incarceration_by_felony <- incarceration_by_felony %>%
#   mutate(percentage = total_locally_sentenced / sum(total_locally_sentenced) * 100)
# 
# # Create a bar graph showing percentages
# p <- ggplot(incarceration_by_felony, aes(x = offense_category, y = percentage, fill = offense_category)) +
#   geom_col() +
#   labs(title = "Percentage of Locally Sentenced Inmates by Offense Category",
#        x = "Offense Category",
#        y = "Percentage of Inmates (%)",
#        fill = "Offense Category") +
#   theme_minimal()
# 
# print(p)
# 
# 









#make the bar chart 2

library(dplyr)
# Assign corrections_data_1 to corrections_data_1_clean
corrections_data_1_clean <- corrections1
corrections_data_1_clean <- na.omit(corrections_data_1_clean)
library(dplyr)
# Example data manipulation
corrections_data_1_clean <- corrections_data_1_clean %>%
  mutate( offense_category = ifelse(`Offense Type` %in% c( 'Murder', 'Forcible Sex Offense', 'Non-Forcible Sex Offense', 'Forgery/Fraud'),  'violent', 'non-violent'  ) )
library(ggplot2)
library(ggthemes)  # Load ggthemes after installing
# Example ggplot code with theme_fivethirtyeight
p <- ggplot(corrections_data_1_clean, aes(x = offense_category, fill = category)) +
  geom_bar(fill = 'orange') +
  labs(title = "Incarceration by Felony",
       x = "Violent or Non-Violent",
       y = "Count") +
  theme_fivethirtyeight() +  # Apply theme_fivethirtyeight
  scale_x_discrete(labels = c("non-violent" = "Non-Violent", "violent" = "Violent"))
print(p)
library(dplyr)
# Example data manipulation
incarceration_by_felony <- corrections_data_1_clean %>%
  select(offense_category, `TDOC Inhouse Inmates`, `TDOC Backup Inmates`, `Locally Sentenced Inmates`,`Statewide Inmates`) %>%
  pivot_longer(!offense_category) %>%
  group_by( offense_category ) %>%
  summarize( total_inmates = sum(value)) %>% 
  # create violent and non-violent categories for felonies
  within(incarceration_by_felony, {
    total_inmates[offense_category == "non-violent"]  <-   
      total_inmates[offense_category == "non-violent"]})




#hgfhdhdhdhdh


library(ggplot2)
# Create a bar graph
p <- ggplot(incarceration_by_felony, aes(x = offense_category, y=total_inmates)) +
  geom_col(fill = 'orange') +
  labs(title = "Current Incarceration by Felony",
       x = "Violent or Non-Violent",
       y = "Count") +
  theme_fivethirtyeight() +  # Apply theme_fivethirtyeight
  scale_x_discrete(labels = c("non-violent" = "Non-Violent", "violent" = "Violent"))
print (p)



library(ggplot2)

# Example data for released incarceration by felony
current_violent <- incarceration_by_felony$total_inmates[2]
current_nonviolent <- incarceration_by_felony$total_inmates[1]
current_total <- current_violent + current_nonviolent
total_released_felons <- 377157 # sentencing project 2022
released_nonviolent <- total_released_felons*(current_nonviolent/current_total)
released_violent <- total_released_felons*(current_violent/current_total)

released_incarceration_by_felony <- data.frame(
  offense_category = c("violent", "non-violent"),
  total_inmates = c(released_violent, released_nonviolent)  # Example data, replace with actual values
)

# Create a bar graph for released incarceration by felony
p_released <- ggplot(released_incarceration_by_felony, aes(x = offense_category, y = total_inmates)) +
  geom_col(fill = 'orange') +  # Use orange color for bars
  labs(title = "Released Incarceration by Felony Type",
       x = "Violent or Non-Violent",
       y = "Count") +
  theme_fivethirtyeight() +  # Apply theme_fivethirtyeight
  scale_x_discrete(labels = c("non-violent" = "Non-Violent", "violent" = "Violent")) +
  scale_y_continuous(limits = c(0, 470000), labels = scales::comma)

print(p_released)


###scale_y_continuous(limits = c(0, 470000), labels = scales::comma)

##ylim(0, 470000) +  # Set y-axis limits to ensure 470,000 is the maximum
##  scale_y_continuous(labels = scales::comma)  # Format y-axis labels with commas

library(ggplot2)

# Assuming you have a data frame 'incarceration_by_felony' with columns 'offense_category' and 'total_inmates'


# Create a bar graph
p <- ggplot(incarceration_by_felony, aes(x = offense_category, y = total_inmates)) +
  geom_col(fill = 'orange') +
  labs(title = "Current Incarceration by Felony Type",
       x = "Violent or Non-Violent",
       y = "Count") +
  theme_fivethirtyeight() +  # Apply theme_fivethirtyeight
  scale_x_discrete(labels = c("non-violent" = "Non-Violent", "violent" = "Violent")) +
  scale_y_continuous(limits = c(0, 470000), labels = scales::comma)

print(p)




#dddddd










