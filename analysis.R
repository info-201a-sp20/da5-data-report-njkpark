#SUMMARY:
library(lintr)
library(dplyr)
library(ggplot2)
library(styler)

shootings_2018 <- read.csv(file =
                             "./data/shootings-2018.csv",
                           stringsAsFactors = FALSE)

#How many shootings occurred? In 2018, 340 shooting have occurred.
total_shootings <- nrow(shootings_2018)

#How many lives were lost? According to the data, 373 lives were lost.
total_lives_lost <- shootings_2018 %>%
  summarize(num_killed = sum(num_killed)) %>%
  pull()

# Which city was most impacted by shootings
#(make sure to clarify how you are measuring "impact")?
#Chicago was the city that was most
#impacted in terms of the amount of different times it has held shootings.
most_city_impact <- tail(names(sort(table(shootings_2018$city))), 1)


#Two other insights of your choice
#How many were injured: 1347
total_injured <- shootings_2018 %>%
  summarize(num_injured = sum(num_injured)) %>%
  pull()

#Which state was most impacted by shootings? Illinois
most_state_impacted <- tail(names(sort(table(shootings_2018$state))), 1)


#SUMMARY TABLE
#sum of casualties in each state and fatality rate

agg_summary <- shootings_2018 %>%
  group_by(state) %>%
  summarize("Total Killed" = sum(num_killed),
            "Total Injured" = sum(num_injured),
            "Fatality Rate" = sum(num_killed) /
              (sum(num_killed) + sum(num_injured)))

# Description of Particular incident
#Data for Seattle
seattle_shooting <- shootings_2018 %>%
  filter(city == "Seattle (Skyway)")
#Number Killed
seattle_num_killed <- shootings_2018 %>%
  filter(city == "Seattle (Skyway)") %>%
  pull(num_killed)
#Number Injured
seattle_num_injured <- shootings_2018 %>%
  filter(city == "Seattle (Skyway)") %>%
  pull(num_injured)
#Total Impacted
seattle_impacted <- shootings_2018 %>%
  filter(city == "Seattle (Skyway)") %>%
  summarize(sum(num_killed + num_injured)) %>%
  pull()
#Location
seattle_state <- shootings_2018 %>%
  filter(city == "Seattle (Skyway)") %>%
  pull(state)
#date
seattle_date <- shootings_2018 %>%
  filter(city == "Seattle (Skyway)") %>%
  pull(date)


#Interactive Map
library(plotly)
library(dplyr)
library(ggplot2)
library(maps)

#creating a dataframe with state, city, long, lat, num_injured, num_killed
map_df <- shootings_2018 %>%
  select(city, lat, long, num_killed, num_injured, state)


#creating the map
state_shape <- map_data("state")

maps <- ggplot() +
  geom_polygon(data = state_shape,
               aes(x = long, y = lat, group = group), #making the state map
                color = "gray88", fill = "lavenderblush1") +
  geom_point(data = map_df,
             aes(x = long, y = lat, size = num_killed,
                 color = num_injured, #creating data points
              text = paste("State:", state,
               "<br>City:", city,
               "<br>Number Killed:", num_killed,
               "<br>Number Injured:", num_injured))) +
  scale_colour_distiller(palette = "RdPu") +
  labs(color = "Number Injured", #changing labels to be more appropriate
       x = "Longitude",
       y = "Latitude",
       title = "Shootings in the U.S. During 2018")
 
 ggplotly(maps, tooltip = "text") #making the map interactive
 
#Plot

 #creating a df with just fatality rate and state to graph
plot_map <- shootings_2018 %>%
  group_by(state) %>%
  summarize(fatality = sum(num_killed) / (sum(num_killed) + sum(num_injured)))
#creating the graph
ggplot(plot_map) +
  geom_col(mapping = aes(x = fatality, y = state), fill = "black") +
  labs(x = "Fatality Rate", y = "State", title = "Fatality Rate in States") +
  coord_cartesian(xlim = c(0, 1)) #setting new bounds on x-axis
  
lint("analysis.R")
