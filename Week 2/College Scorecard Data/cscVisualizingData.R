# Shreya Khanvilkar
# 12-30-2023
# Visualizing College Scorecard Data

library(tidyverse)


# exploring summary, standard deviation, and scatter plots/histograms of data
college_scorecard <- read_csv("/Users/shreyakhanvilkar/college/programming/college_sc_clean.csv")

summary(college_scorecard$costt4_a)

sd(college_scorecard$costt4_a, na.rm = TRUE)

# college_scorecard %>%
#   ggplot(aes(x = costt4_a)) + geom_density()
# 
#college_scorecard %>%
#  ggplot(aes(x = costt4_a)) + geom_histogram()

# How many college are there per state in database?
# find and plot the above question
college_scorecard %>% 
  count(state)

college_scorecard %>% 
  count(state) %>%
  View()
#or
table(college_scorecard$state)

college_scorecard %>% 
  count(n_distinct(state))

college_scorecard %>% 
  ggplot(aes(x = state)) + geom_bar() + coord_flip()
#without coord_flip(), chart is not impossible to read (by stretching the graph out), but easier
college_scorecard %>% 
  ggplot(aes(x = state)) + geom_bar()

# comparing cost of attendance to SAT score
college_scorecard %>% 
  ggplot(aes(x = sat_avg, y = costt4_a)) + geom_point()

# comparing cost of attendance by state and visualizing
college_scorecard %>% 
  group_by(state) %>% 
  summarize(avg_cost = mean(costt4_a, na.rm = TRUE)) %>% 
  arrange(desc(avg_cost)) %>%
  View()

college_scorecard %>% 
  group_by(state) %>% 
  summarise(avg_cost = mean(costt4_a, na.rm = TRUE)) %>%
  arrange(desc(avg_cost)) %>% 
  head(10) %>% 
  ggplot(aes( x = state, y = avg_cost)) + geom_col()

# comparing womenonly and state
table(college_scorecard$womenonly, college_scorecard$state)

college_scorecard %>%
  group_by(state) %>%
  count(womenonly) %>%
  filter(womenonly == 1) %>%
  ggplot(aes(x = state, y = n)) + geom_col()


# multivariable plotting
college_scorecard %>% 
  ggplot(aes(x = sat_avg, y = costt4_a, color = state)) + geom_point()

college_scorecard %>% 
  ggplot(aes(x = sat_avg, y = costt4_a, color = state)) + geom_point() +
  theme(legend.position = "none")

college_scorecard %>% 
  group_by(state) %>% 
  summarize(avg_cost = mean(costt4_a, na.rm = TRUE), 
            avg_median_hh_income = mean(median_hh_inc, na.rm = TRUE)) %>% 
  arrange(desc(avg_cost))

college_scorecard %>% 
  group_by(state) %>% 
  summarize(avg_cost = mean(costt4_a, na.rm = TRUE), 
            avg_median_hh_income = mean(median_hh_inc, na.rm = TRUE)) %>% 
  arrange(desc(avg_cost)) %>% 
  ggplot(aes(x = avg_median_hh_income, y = avg_cost, color = state)) + 
  geom_point() +
  theme(legend.position = "none")

college_scorecard %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  filter(state == "NC") %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude, 
                   popup=~instnm, radius = ~costt4_a/10000)

