########################################################################################
# Summary: Tidying and analyzing cotton production trends in NC
# Date: September 25, 2019
########################################################################################

# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)
library(dplyr)

# Overall Questions:
## How have yield and are harvested of cotton changed across all of the NC agricultural districts over time?
## What were the top 3 cotton producing counties in NC in terms of total lbs of cotton for 2018?

# 2. Read & inspect the dataset ----
cot <- read_csv("data/cotton-usda-nass.csv")
str(cot)
head(cot)
tail(cot)
dim(cot)
summary(cot)

# 3.1. Create a NC data subset ----
nc_cot <- cot %>%
  dplyr::select(year, state, ag_district, county, data_item, value) %>%
  filter(state == "NORTH CAROLINA")


# 3.2. Divide the data_item column ----
nc_cot %>%
  separate(data_item,
         into = c("cotton_type", "measurement"),
         sep = " - ") -> nc_cot

# 3.3. Convert the value column to numeric type ----
nc_cot %>%
  filter(!value == "(D)")
as.numeric(nc_cot$value)
nc_cot$value <- as.numeric(nc_cot$value)

nc_cot %>%
  drop_na()

# 4. Visualizing trends ----
nc_cot %>%
  ggplot(mapping = aes(x = year, y = value, group = ag_district)) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(measurement ~ ag_district,
             scales = "free_y")

# 5. Summarize data from 2018 ----
## What were the top 3 cotton producing counties in NC in terms of total lbs of cotton for 2018?
nc_cot %>%
  filter(year == "2018") %>%
  spread(measurement, value) -> cot_weight

as.numeric(cot_weight$`ACRES HARVESTED`)
cot_weight$`ACRES HARVESTED` <- as.numeric(cot_weight$`ACRES HARVESTED`)

as.numeric(cot_weight$`YIELD, MEASURED IN LB / ACRE`)
cot_weight$`YIELD, MEASURED IN LB / ACRE` <- as.numeric(cot_weight$`YIELD, MEASURED IN LB / ACRE`)

cot_weight %>%
  mutate(total_lbs = `ACRES HARVESTED` * `YIELD, MEASURED IN LB / ACRE`) -> cot_tot_weight

cot_tot_weight %>%
  arrange(desc(total_lbs)) %>%
  top_n(3, total_lbs) -> top_three

top_three %>%
  select(county, total_lbs)
  
