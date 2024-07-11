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
tm_shape(census_votes_clean) +
  tm_polygons(alpha = 0.8, col = c('Voter Turnout:'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE) 

# heat map for voter turnout (in percent groups)
tmap_mode("view")
tm_shape(census_votes_clean) +
  tm_polygons(alpha = 0.8, col = c("Voter Turnout (%):"), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE)

  # CENSUS (INCOME & EMPLOYMENT)

# heat map for income level below poverty line and unemployed population 
tmap_mode("view")
tm_shape(census_votes_clean) +
  tm_polygons(alpha = 0.8, col = c('poverty_income', 'unemployed'), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE) 

# poverty income / unemployment percentage map [here]

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

#income category map [here]

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
  # total percent of african american household populations (perc. of households), census
  mutate(afr_amr_perc = afr_amr / race_tally) %>% 
  # plot average voter turnout rates by race via bar chart
  # reshape the data from wide to long format
  pivot_longer(cols = c(white_, afr_amr, nativeamr, asian, pac_isl, otherrace), 
               names_to = "race_code", 
               values_to = "total_race_tally")
  #mutate(voter_race = )
View(census_votes_clean_category2)

# # plot average voter turnout rates by race via bar chart
# ggplot(census_votes, aes(x = race_tally, y = `Voter Turnout:`)) +
#  
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
  geom_histogram() +
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

# SCATTERPLOTS

avg_voter_turnout <- mean(census_votes_clean_category2$`Voter Turnout (%):`, na.rm = TRUE)
print(avg_voter_turnout)
#39.25526%

ggplot(data = census_votes_clean_category2, aes( x = `Voter Turnout:`, y = poverty_income)) +
  geom_point(color = "darkseagreen3")
ggplot(data = census_votes_clean_category2, aes( x = `Voter Turnout (%):`, y = low_income_perc)) +
  geom_point(color = "darkseagreen3")
ggplot(data = census_votes_clean_category2, aes( x = `Voter Turnout (%):`, y = middle_income_perc)) +
  geom_point(color = "darkseagreen3")
ggplot(data = census_votes_clean_category2, aes( x = `Voter Turnout (%):`, y = high_income_perc)) +
  geom_point(color = "darkseagreen3")
ggplot(data = census_votes_clean_category2, aes( x = `Voter Turnout (%):`, y = highest_income_cat)) +
  geom_point(color = "darkseagreen3")
# if you're a low income category county, you're more likely to have a voter turnout rate that's higher   than average

mod1 <- lm(`Voter Turnout (%):` ~ highest_income_cat, data = census_votes_clean_category2)
summary(mod1)

# in counties where there's a higher proportion of people of color, is voter turnout higher/lower?
poc_vote <- cor(census_votes_clean_category2$poc_perc, census_votes_clean_category2$`Voter Turnout (%):`)
print(poc_vote)
  # -0.1256031, negative correlation

# scatter plot of proportion of POC vs. voter turnout
ggplot(census_votes_clean_category2, aes(x = poc_perc, y = `Voter Turnout (%):`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Proportion of People of Color vs. Voter Turnout",
       x = "Proportion of People of Color",
       y = "Voter Turnout (%)") +
  theme_minimal()

# in counties where there's a higher proportion of african americans, is voter turnout higher/lower?
afr_amr_vote <- cor(census_votes_clean_category2$afr_amr_perc, census_votes_clean_category2$`Voter Turnout (%):`)
print(afr_amr_vote)
# -0.1111298, negative correlation

# scatter plot of proportion of afr_amr vs. voter turnout
ggplot(census_votes_clean_category2, aes(x = afr_amr_perc, y = `Voter Turnout (%):`)) +
  geom_point() +
  ylim(0,100) +
  labs(title = "Proportion of African Americans vs. Voter Turnout",
       subtitle = "in Tennessee Population by County",
       x = "Proportion of African Americans to TN Population",
       y = "Voter Turnout (%)",
       caption = "1 point = 1 TN county
       Correlation Coefficient Value: -0.1256031") +
  theme_minimal()

# bar chart of proportion of afr_amr vs. voter turnout
ggplot(census_votes_clean_category2, aes(x = afr_amr_perc)) +
  geom_histogram() +
  #geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Proportion of African Americans in Population vs. Voter Turnout",
       x = "Proportion of African Americans in Population",
       y = "Voter Turnout (%)") +
  theme_minimal()

# create a third dataset that still has afr_amr from c1 but also has t_r_t from c2
census_votes_clean_category3 <- census_votes_clean_category1 %>% 
  group_by(County) %>% 
  # total household populations (no. of households), census
  mutate(race_tally = sum(white_, afr_amr, nativeamr, asian, pac_isl, otherrace)) %>%
  # total people of color household populations (no. of households), census
  mutate(poc_tally = sum(afr_amr, nativeamr, asian, pac_isl, otherrace)) %>%
  # total percent of color household populations (perc. of households), census
  mutate(poc_perc = poc_tally / race_tally) %>% 
  # total percent of african american household populations (perc. of households), census
  mutate(afr_amr_perc = afr_amr / race_tally)
View(census_votes_clean_category3)

# model using registered voters and number of voters:
logistic <- census_votes_clean_category3 %>% 
  mutate(nonvoters = `Registered Voters:` - `Total\nVotes Cast:`) %>%  
  rename(voters = `Total\nVotes Cast:`) %>% 
  mutate(black_pop = afr_amr / race_tally * 100) 

# create dataset with the required columns
logistic_df <- logistic %>%
  select(voters, nonvoters, black_pop, highest_income_cat)

# fit a logistic model
fit <- glm(cbind(voters, nonvoters) ~ 1 + black_pop + highest_income_cat, family = "binomial", data = logistic_df)

# check out the results
summary(fit)
     # Coefficients:
     #   Estimate Std. Error
     # (Intercept)                     -3.931e-01  1.790e-03
     # black_pop                       -4.677e-03  5.583e-05
     # highest_income_catmiddle income  1.349e-02  1.960e-03
     # z value Pr(>|z|)    
     # (Intercept)                     -219.637  < 2e-16 ***
     #   black_pop                        -83.775  < 2e-16 ***
     #   highest_income_catmiddle income    6.886 5.74e-12 ***
  # The coefficient is -0.3931, which implies that in a hypothetical county where the percentage of Black population is zero and the income level is at the reference category (likely the highest income category), the log-odds of voter turnout is -0.3931. The z-value (-219.637) and the extremely small p-value (< 2e-16) indicate that the intercept is statistically significant.
  # The coefficient for black_pop is -0.004677. This negative value indicates that as the percentage of the Black population in a county increases, the log-odds of voter turnout decrease. The z-value (-83.775) and the p-value (< 2e-16) suggest that this effect is statistically significant.
  # The coefficient for the middle-income category relative to the highest income category is 0.01349. This positive value suggests that counties in the middle-income category have higher log-odds of voter turnout compared to the highest income category. The z-value (6.886) and the p-value (5.74e-12) indicate that this difference is statistically significant.

    # Black Population: There is a significant negative relationship between the percentage of     Black population in a county and voter turnout. For every 1% increase in the Black             population, the log-odds of voter turnout decrease by approximately 0.004677.
        # emphasize ratio
    # Income Level: Counties categorized as middle-income have a significantly higher log-odds     of voter turnout compared to counties in the highest income category.
    # Statistical Significance: All predictors in the model are statistically significant,         meaning their effects on voter turnout are unlikely to be due to chance.

install.packages("sjPlot")
library(sjPlot)
tab_model(fit,
          dv.labels = "Logistic Regression",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value")

# Create a dataframe with predicted probabilities
logistic_df2 <- logistic_df %>%
  mutate(predicted_prob = predict(fit, type = "response"))

# Plot predicted probabilities against black_population
ggplot(logistic_df2, aes(x = black_pop, y = predicted_prob)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Predicted Probability of Voter Turnout by Black Population",
       x = "Percentage of Black Population",
       y = "Predicted Probability of Voter Turnout") +
  theme_minimal()


# RESULTS
# map crime and incarceration rates
tmap_mode("plot")
tm_shape(census_votes_clean_category2) +
  tm_polygons(alpha = 0.8, col = c("correction_data_1","crime"), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE)

# Plotting the map
ggplot(data = census_votes, aes(x = census, y = census_votes, group = group, fill = crime)) +
  geom_polygon (color = "black") +
  scale_fill_gradient(name = "Incarceration Rate", low = "lightblue", high = "darkblue",
                      na.value = "grey50", guide = "legend") +
  labs(title = "Incarceration Rates by County") +
  theme_minimal()
#barchart Incarceration Rates by Felony
ggplot(data = census_votes, aes(x = correction_data_1, y = census, group = group, fill = rate)) +
  geom_bar(color = "blue") +
  scale_fill_gradient(name = "Incarceration Rate", low = "lightblue", high = "darkblue",
                      na.value = "grey50", guide = "legend") +
  labs(title = "Incarceration Rates by Felony") +
  theme_minimal()

range(census_votes_clean_category2$`Voter Turnout:`)
