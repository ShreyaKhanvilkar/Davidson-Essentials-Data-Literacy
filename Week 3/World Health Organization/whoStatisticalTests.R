# Shreya Khanvilkar
# 1-6-2024
# Statistical Tests with WHO dataset

#Setup
library(tidyverse)

who <- read_csv("/Users/shreyakhanvilkar/college/programming/who_clean.csv")


# Comparing Two Numerical Variables
# (Life Expectancy and BMI)

# last week's plot
who %>%
  ggplot(aes(x=bmi, y=life_expectancy)) + geom_point()

# adding line of best fit
who %>%
  ggplot(aes(x = bmi, y = life_expectancy)) +
  geom_point() + geom_smooth(method = "lm")

# perform correlation test
cor.test(who$life_expectancy, who$bmi)

# annotate plot with test results
who %>%
  ggplot(aes(x =bmi, y = life_expectancy)) +
  geom_point() + geom_smooth(method = "lm") + 
  annotate("text", x=30, y=55, label="r = .59, p < 0.001")


# Comparing Averages Across Two Groups
# (Life Expectancy and Status)

# last week's plot
who %>%
  ggplot(aes(x = status, y = life_expectancy)) + geom_boxplot()

who %>%
  group_by(status) %>%
  summarize(avg_le = mean(life_expectancy, na.rm=TRUE)) %>%
  ggplot(aes(x = status, y = avg_le)) + geom_col()

# peform t-test
t.test(life_expectancy ~ status, data=who)

# annotate boxplot with test results
who %>%
  ggplot(aes(x = status, y = life_expectancy)) + geom_boxplot() + 
  annotate("text", x = "Developed", y = 65, label = "t = 14.302, p < 0.001") + 
  geom_point(aes(x = "Developed", y = 80.14), color="red") + 
  geom_point(aes(x = "Developing", y = 69.43), color="red")


# Associations Between Categorical Variables
# Continent and Status

# last week's numeric summary
table(who$status, who$continent)

# last week's plot
who %>%
  ggplot(aes(x = status, fill = continent)) + 
  geom_bar(position="stack")

# performing a chi squared test
chisq.test(who$continent, who$status)
