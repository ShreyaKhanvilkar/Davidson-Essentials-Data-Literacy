library(tidyverse)

who <- read_csv("/Users/shreyakhanvilkar/college/programming/who_clean.csv")


# summary, standard deviation, and plot for life-expectancy
summary(who$life_expectancy)

sd(who$life_expectancy, na.rm = TRUE)

who %>%
  ggplot(aes(x = life_expectancy)) + geom_density()


# summary and plot for BMI

summary(who$bmi)

who %>%
  ggplot(aes(x = bmi)) + geom_density()


# looking at the relationship between life-expectancy and bmi
# using different kinds of plots: scatter plot, box plot, bar chart 
who %>%
  ggplot(aes(x = bmi, y = life_expectancy)) + geom_point()

who %>%
  group_by(status) %>%
  summarize(min_le = min(life_expectancy, na.rm = TRUE), 
            med_le = median(life_expectancy, na.rm = TRUE), 
            avg_le = mean(life_expectancy, na.rm = TRUE), 
            max_le = max(life_expectancy, na.rm = TRUE))

who %>%
  ggplot(aes(x = status, y = life_expectancy)) + geom_point()

who %>%
  ggplot(aes(x = status, y = life_expectancy)) + geom_boxplot()

who %>%
  group_by(status) %>%
  summarize(avg_le = mean(life_expectancy, na.rm = TRUE)) %>%
  ggplot(aes(x = status, y = avg_le)) + geom_col()

who %>%
  group_by(status) %>%
  count(continent)
#or
table(who$status, who$continent)

who %>%
  ggplot(aes(x = bmi, y = life_expectancy, color = status)) + geom_point()
#separating plots
who %>%
  ggplot(aes(x = bmi, y = life_expectancy, color = status)) + geom_point() + facet_wrap(~status)
#adding continent color and changing size of points and transparency
who %>%
  ggplot(aes(x = bmi, y = life_expectancy, color = continent, size = pop_mil)) +
  geom_point(alpha=0.4) + facet_wrap(~status) + theme(legend.position = "none")
