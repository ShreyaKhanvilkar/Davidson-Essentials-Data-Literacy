library(tidyverse)

who <- read_csv("/Users/shreyakhanvilkar/college/programming/world_health_organization.csv")

glimspe(who)

name(who)

who$Country

who$`CHE GDP(%)`

#thisIsCamelCase

# making sure spaces and special characters aren't a problem
who <- who %>%
  rename(CHE_GDP_PCT = `CHE GDP(%)`)

who$CHE_GDP_PCT

# lowercasing names
names(who) <- str_to_lower(names(who))

summary(who)

# checking number of distinct countries
who %>%
  summarize(n_distinct(country, na.rm=TRUE))

who %>%
  count(continent)

# checking if there are any missing continent name values
who %>%
  filter(is.na(continent))

# checking if there are any missing status values
who %>%
  count(status)

# scaling down variable of population and gdp
who <- who %>%
  mutate(pop_mil = population/1000000)

who <- who %>%
  mutate(gdp_mil = gdp/1000000)

who <- who %>%
  mutate(gdp_per_cap = gdp/population)
  
# filtering database based on only one continent (or other subsets)
who_africa <- who %>%
  filter(continent == "Africa")

who_americans <- who %>%
  filter(continent == "North America" | continent == "South America")

who %>%
  filter(continent == "Europe" | status == "Developed")

who %>%
  filter(life_expectancy > 80)

# exporting the wrangled dataset
write_csv(who, "/Users/shreyakhanvilkar/college/programming/who_clean.csv")
