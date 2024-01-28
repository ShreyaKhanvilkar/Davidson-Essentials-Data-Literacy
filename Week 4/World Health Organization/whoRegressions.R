# Shreya Khanvilkar
# 1-28-2024
# Regressions with WHO dataset

#Setup
library(tidyverse)

who <- read_csv("/Users/shreyakhanvilkar/college/programming/who_clean.csv")

# Simple Linear Regression

slr_1 <- lm(life_expectancy ~ che_gdp_pct, data=who)
summary(slr_1)

# Regression Equation: 
# lifeExpectancy = 66.16 + 0.84 * cheGdpPct

slr_2 <- lm(life_expectancy ~ continent, data=who)
summary(slr_2)


# Multiple Linear Regression
who %>%
  ggplot(aes(x = bmi, y = life_expectancy, color = status)) +
  geom_point() + facet_wrap(~status)

mlr_1 <- lm(life_expectancy ~ bmi + status, data=who)
summary(mlr_1)

mlr_2 <- lm(life_expectancy ~ bmi + status + continent + 
              gdp_mil + pop_mil + che_gdp_pct, data=who)
summary(mlr_2)

# for mlr_3, rescale numeric values, setting reference groups for char. var.,
# exculding var. that weren't significant in mlr_2
mlr_3 <- lm(life_expectancy ~
              scale(bmi) + fct_relevel(status, "Developing") + continent,
            data=who)
summary(mlr_3)
