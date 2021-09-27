## Wrangling county-level election results

# Load data
library(here)
library(tidyverse)
votes <- read_csv(here("data", "countypres_2000-2020.csv"))

# Need to focus on 2020 and 2016
votes <- votes %>%
  filter(year %in% c(2016, 2020))

# Getting the two-party vote formatted
votes2016 <- votes %>%
  filter(year == 2016) %>%
  select(year, state, state_po, county_name, county_fips, party, 
         totalvotes, candidatevotes) %>%
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  pivot_wider(names_from = party, values_from = candidatevotes) %>%
  janitor::clean_names() %>%
  mutate(twoparty = democrat + republican,
         gop_pct = 100 * republican / twoparty,
         dem_pct = 100 * democrat / twoparty) 

votes2020 <- votes %>%
  filter(year == 2020) %>%
  group_by(year, state, state_po, county_name, county_fips, party, totalvotes) %>%
  summarize(candidatevotes = sum(candidatevotes)) %>%
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  ungroup() %>%
  pivot_wider(names_from = party, values_from = candidatevotes) %>%
  janitor::clean_names() %>%
  mutate(twoparty = democrat + republican,
         gop_pct = 100 * republican / twoparty,
         dem_pct = 100 * democrat / twoparty) 


# Merging the data sets

votes_combo <- left_join(
  votes2020, 
  votes2016, 
  by = c("state", "state_po", "county_name"),
  suffix = c("2020", "2016")
) %>%
  mutate(pct_change_gop = 100 * (gop_pct2020 / gop_pct2016 - 1),
         pct_change_dem = 100 * (dem_pct2020 / dem_pct2016 - 1)) %>%
  select(
    state, state_po, county_name, county_fips2020, 
    totalvotes2020:dem_pct2020, 
    totalvotes2016:pct_change_dem
  ) %>%
  rename(fips = county_fips2020)

write_csv(votes_combo, here("data", "pres_pct_change_2016_2020.csv"))

