# Shreya Khanvilkar
# 1-6-2023
# Statistical Tests with College Scorecard data

#Setup
library(tidyverse)

college_scorecard <- read_csv("/Users/shreyakhanvilkar/college/programming/college_sc_clean.csv")


# Comparing Two Numerical Variables
# (Cost of Attendance and Average SAT Score)

# plot
college_scorecard %>%
  ggplot(aes(x = sat_avg, y = costt4_a)) + geom_point()

# adding line of best fit
college_scorecard %>%
  ggplot(aes(x = sat_avg, y = costt4_a)) +
  geom_point() + geom_smooth(method = "lm")

# perform correlation test
cor.test(college_scorecard$costt4_a, college_scorecard$sat_avg)

# annotate plot with test results
college_scorecard %>%
  ggplot(aes(x = costt4_a, y = sat_avg)) +
  geom_point() + geom_smooth(method = "lm") + 
  annotate("text", x=750, y=75000, label="r = 0.54, p < 0.001")


# Comparing Averages Across Two Groups
# (Cost of Attendance and Women-Only Institutions)

# table
college_scorecard %>%
  group_by(womenonly) %>%
  summarize(avg_cost = mean(costt4_a, na.rm=TRUE),
            count = n())

# boxplot
college_scorecard %>%
  filter(!is.na(womenonly)) %>%
  ggplot(aes(x = as_factor(womenonly), y = costt4_a)) + geom_boxplot() +
  geom_point(aes(x = 1, y = 26148), color="red") + 
  geom_point(aes(x = 1, y = 48229.91), color="red")

# peform t-test
t.test(costt4_a ~ as_factor(womenonly), data=college_scorecard)

# annotate boxplot with test results
college_scorecard %>%
  filter(!is.na(womenonly)) %>%
  ggplot(aes(x = as_factor(womenonly), y = costt4_a)) + geom_boxplot() +
  geom_point(aes(x = 1, y = 26148), color="red") + 
  geom_point(aes(x = 1, y = 48229.91), color="red") + 
  annotate("text", x = 2, y = 87500, label = "T = -11.264, p < 0.001")


# Associations Between Categorical Variables
# (Admission Rate and Women-Only Institutions)

# creating a new variables 
college_scorecard <- college_scorecard %>%
  mutate(adm_rate_75 = ifelse(adm_rate < 0.75, 1, 0))

# table
college_scorecard %>% 
  group_by(womenonly, adm_rate_75) %>%
  summarize(count = n())

table(college_scorecard$womenonly, college_scorecard$adm_rate_75)

prop.table(table(college_scorecard$womenonly, college_scorecard$adm_rate_75), 1)

# chi-squared test
chisq.test(college_scorecard$womenonly, college_scorecard$adm_rate_75)
