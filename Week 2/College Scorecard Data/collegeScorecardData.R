# Shreya Khanvilkar
# 12-30-2023
# Wrangling College Scorecard Data

library(tidyverse)

college_sc <- read_csv("/Users/shreyakhanvilkar/college/programming/MERGED2021_22_PP.csv")

glimpse(college_sc)

colnames(college_sc)


# the data that I downloaded from the website was VERY different from the data described/shown
# using the following, columns are reduced to get only what is needed
col <- college_sc[c("UNITID", "INSTNM", "CITY", "STABBR", "ZIP", "ACCREDAGENCY", "LATITUDE", "LONGITUDE", "MENONLY", "WOMENONLY", "ADM_RATE", "SAT_AVG", "UGDS", "UGDS_WHITE", "UGDS_BLACK", "UGDS_HISP", "UGDS_ASIAN", "UGDS_AIAN", "UGDS_NHPI", "UGDS_2MOR", "COSTT4_A", "AVGFACSAL", "AGE_ENTRY", "FEMALE", "FIRST_GEN", "MEDIAN_HH_INC", "POVERTY_RATE", "UNEMP_RATE")]

head(col)

tail(col)

head(col, 6)

names(col)


# converting all variable names to lowercase
names(col) <- str_to_lower(names(col))


# renaming variable names
col <- col %>%
  rename(state = STABBR)

col <- col %>%
  rename(id = UNITID)

col <- col %>%
  rename(inst = INSTNM)

col <- col %>%
  rename(acc = accredagency)


# changing the format and restoring numeric values
col <- col %>%
  mutate(adm_rate = as.numeric(str_replace(adm_rate, "NULL", "")))

col <- col %>%
  mutate(sat_avg = as.numeric(str_replace(sat_avg, "NULL", "")))

summary(col)


# finding average fiscal year
col <- col %>%
  mutate(avgfacsal = as.numeric(str_replace(avgfacsal, "NULL", "")))

col <- col %>%
  mutate(avg_facsal_year = avgfacsal*12)

col %>% 
  select(inst, avgfacsal, avg_facsal_year)


# exporting the wrangled dataset
write_csv(col, "/Users/shreyakhanvilkar/college/programming/college_sc_clean.csv")
