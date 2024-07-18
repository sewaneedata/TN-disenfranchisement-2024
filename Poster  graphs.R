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


# making a map of incarceration by county

# insert link to the dataset
# read in corrections csv
# todc statistical abstract 2022
# Average Sentence Length by Location as of June 30, 2022
# C/S/F refers to conspiracy/ solicitation/ facilitation
# Under 85%, a convicted defendant must serve min. of 85% of their sentence b4 eligible for parole
# name <- read_excel("data/name.xlsx")


# insert link to the dataset
# load in the corrections data which is from
# todc statistical abstract 2023
# Felon Population by County of Conviction as of June 30, 2022
library(readr)
corrections2 <- read_csv("data/corrections2.csv")


# seems we cannot make incarceration by county because we do not have the data
tmap_mode("plot")
tm_shape(corrections1) +
  tm_polygons(alpha = 0.8, col = c("corrections1","crime"), id = "NAME") +
  # make several layered maps that you can toggle between
  tm_facets(as.layers = TRUE)
library(tmap)
library(sf)



# make a bar chart of incarceration by felony


library(readr)
corrections1 <- read_csv("data/corrections1.csv")
View(corrections1)
library(readr)
corrections2 <- read_csv("data/corrections2.csv")
View(corrections2)

# clean the data to remove NA rows
corrections_data_1_clean <- corrections1 %>% 

  
  # Example data preparation (replace with your actual data structure)
  corrections1 <- data.frame(
    lon = c(-85.7585, -86.7816, -87.0836),
    lat = c(35.8456, 36.1627, 35.9525),
    crime = c("Theft", "Assault", "Burglary")
    # Add more columns as needed
  )

  
# Convert to spatial object (sf class)
corrections1_sf <- st_as_sf(corrections1, coords = c("lon", "lat"), crs = 4326)
library(tmap)

tm_shape(corrections1_sf) +
  tm_dots(col = "crime", size = 0.5) +
  tm_basemap()

  
  
    
  
  library(tmap)
library(sf)

# Assuming corrections1 is converted to sf class as corrections1_sf

# Plotting with tmap
tm_shape(corrections1_sf) +
  tm_polygons(alpha = 0.8, col = "crime")

  
  
  
  
  
  
  
  
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










