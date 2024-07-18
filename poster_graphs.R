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
library(ggthemes)
library(readr)
library(tmap)
library(sf)

#load in data
corrections1 <- read_csv("data/corrections1.csv")
corrections2 <- read_csv("data/corrections2.csv")


# Assign corrections_data_1 to corrections_data_1_clean
corrections_data_1_clean <- na.omit(corrections1) %>% 
  #Example data manipulation
  mutate( offense_category = ifelse(`Offense Type` %in% c( 'Murder', 'Forcible Sex Offense', 'Non-Forcible Sex Offense', 'Forgery/Fraud'),  'violent', 'non-violent'  ) )


# Example data manipulation
incarceration_by_felony<- corrections_data_1_clean %>%
  select(offense_category, `TDOC Inhouse Inmates`, `TDOC Backup Inmates`, `Locally Sentenced Inmates`,`Statewide Inmates`) %>%
  pivot_longer(!offense_category) %>%
  group_by( offense_category ) %>%
  summarize( total_inmates = sum(value) )


# Create a bar graph
incar_by_felony_c <- ggplot(incarceration_by_felony, aes(x = offense_category, y=total_inmates)) +
  geom_col(fill = 'orange') +
  labs(title = "Incarceration by Felony",
       x = "Violent or Non-Violent",
       y = "Count") +
  theme_minimal() +  # Apply theme_fivethirtyeight
  scale_x_discrete(labels = c("non-violent" = "Non-Violent", "violent" = "Violent"))
##print (p)


#Showing the Locally sentenced Inmates - removed


#make the bar chart 2



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
  theme_minimal() +  # Apply theme_fivethirtyeight
  scale_x_discrete(labels = c("non-violent" = "Non-Violent", "violent" = "Violent")) +
  scale_y_continuous(limits = c(0, 470000), labels = scales::comma)

#print(p_released)


###scale_y_continuous(limits = c(0, 470000), labels = scales::comma)

##ylim(0, 470000) +  # Set y-axis limits to ensure 470,000 is the maximum
##  scale_y_continuous(labels = scales::comma)  # Format y-axis labels with commas



# Assuming you have a data frame 'incarceration_by_felony' with columns 'offense_category' and 'total_inmates'


# # Create a bar graph
# p <- ggplot(incarceration_by_felony, aes(x = offense_category, y = total_inmates)) +
#   geom_col(fill = 'orange') +
#   labs(title = "Current Incarceration by Felony Type",
#        x = "Violent or Non-Violent",
#        y = "Count") +
#   theme_fivethirtyeight() +  # Apply theme_fivethirtyeight
#   scale_x_discrete(labels = c("non-violent" = "Non-Violent", "violent" = "Violent")) +
#   scale_y_continuous(limits = c(0, 470000), labels = scales::comma)






