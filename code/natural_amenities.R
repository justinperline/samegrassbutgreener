library(tigris)
library(sf)
library(dplyr)
library(readxl)

# Recreating topography code dictionary from provided file
topography_codes <- list(Plains = c("1" = "Flat plains", "2" = "Smooth plains", "3" = "Irregular plains, slight relief", "4" = "Irregular plains"),
                         Tablelands = c("5" = "Tablelands, moderate relief", "6" = "Tablelands, considerable relief", "7" = "Tablelands, high relief", "8" = "Tablelands, very high relief"),
                         Plains_with_Hills_or_Mountains = c("9" = "Plains with hills", "10" = "Plains with high hills", "11" = "Plains with low mountains", "12" = "Plains with high mountains"),
                         Open_Hills_and_Mountains = c("13" = "Open low hills", "14" = "Open hills", "15" = "Open low mountains", "16" = "Open high mountains"),
                         Hills_and_Mountains = c("18" = "Hills", "19" = "High hills", "20" = "Low mountains", "21" = "High mountains"))

# Reading in data
natural <- read_excel("~/Documents/Github/samegrassbutgreener/data/natamenf_1_.xlsx",
                      range = "A105:V3216") %>%
  mutate(TOPO = as.factor(`topography code`))

# Checking distribution of codes
natural %>%
  group_by(TOPO) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  print(n = 21)

# Merging natural amenities data with spatial data
# Have to download old county data because some records do not match 2022 county codes
counties <- counties(cb = TRUE, year = 2022) %>%
  mutate(GEOID = ifelse(GEOID == "12086", "12025", GEOID)) #Miami-Dade name change caused code change
old_counties <- counties(cb = TRUE, year = 2010) %>%
  mutate(GEOID = paste0(STATE, COUNTY))

natural_counties <- natural %>%
  left_join(counties, by = c("FIPS Code" = "GEOID")) %>%
  st_as_sf()

missing_counties <- natural_counties %>%
  filter(is.na(STATE_NAME)) %>%
  pull(`FIPS Code`)

resolved_counties <- natural %>%
  filter(`FIPS Code` %in% missing_counties) %>%
  inner_join(old_counties, by = c("FIPS Code" = "GEOID"))

natural_counties <- natural_counties %>%
  filter(!is.na(STATE_NAME)) %>%
  bind_rows(resolved_counties) %>%
  select(`FIPS Code`, NAME, STUSPS, TOPO)

# Plotting county by topography code
ggplot(natural_counties) +
  geom_sf(aes(fill = TOPO)) +
  theme_void()

# Downloading every census-designated place in the US
# Removing islands, territories
us_places <- places(state = NULL, year = 2022, cb = TRUE) %>%
  filter(!(STATEFP %in% c(60, #American Samoa
                          66, #Guam
                          69, #Mariana Islands
                          72, #Puerto Rico
                          78))) %>% #Virgin Islands
  rownames_to_column("PlaceNum")

# Spatial join of place to county in order to match TOPO code
# Using intersects here instead of st_within
us_places_counties <- us_places %>%
  st_join(natural_counties,
          join = st_intersects,
          largest = TRUE)

#for 3+ county matches, use mode?
#for 2, use mean?
#how many places have multiple counties that are actually different topo codes?
us_places_counties %>%
  group_by(PlaceNum) %>%
  filter(n() > 1) %>%
  mutate(TOPO = as.numeric(TOPO)) %>%
  filter(max(TOPO) - min(TOPO) > 0)
  