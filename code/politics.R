library(tigris)
library(sf)
library(dplyr)
library(furrr)

votes <- read.csv("~/Documents/Github/samegrassbutgreener/data/2020_US_County_Level_Presidential_Results.csv",
                  colClasses = c(county_fips = "character"))

# Merging natural amenities data with spatial data
# Have to download old county data because some records do not match 2022 county codes
counties <- counties(cb = TRUE, year = 2022) %>%
  filter(STUSPS != "CT") %>% #Connecticut counties became "Planning Regions" in 2022
  mutate(GEOID = case_when(GEOID == "12086" ~ "12025", #Dade -> Miami-Dade
                           GEOID == "46102" ~ "46113", #Shannon -> Oglala Lakota
                           TRUE ~ GEOID)) %>%
  bind_rows(counties(state = "CT", cb = TRUE, year = 2021))

county_votes <- counties %>%
  left_join(votes, by = c("GEOID" = "county_fips"))


# Downloading every census-designated place in the US
# Removing islands, territories
us_places <- places(state = NULL, year = 2022, cb = TRUE) %>%
  filter(!(STATEFP %in% c(60, #American Samoa
                          66, #Guam
                          69, #Mariana Islands
                          72, #Puerto Rico
                          78))) #Virgin Islands

# Function to limit spatial join to nearby counties from each state
large_spatial_join <- function(state) {
  
  us_places_state <- us_places %>%
    filter(STUSPS == state)
  
  county_votes_state <- county_votes %>%
    st_filter(us_places_state,
              .predicate = st_is_within_distance,
              dist = 100000)
  
  # us_places_counties <- us_places_state %>%
  #   st_join(county_votes_state,
  #           join = st_intersects,
  #           largest = TRUE)
  
  intersections <- st_intersection(us_places_state, county_votes_state)
  intersections <- intersections %>%
    filter(st_is_valid(geometry) == TRUE) %>%
    mutate(AREA = as.numeric(st_area(geometry))) %>%
    group_by(GEOID) %>%
    mutate(PCT_AREA = AREA / sum(AREA))
    summarize()
  
  return(us_places_counties)
}

# Using parallel processing to attempt this spatial join one state at a time
n_cores <- future::availableCores()
plan(multisession, workers = n_cores)

us_places_counties <- future_map(unique(us_places$STUSPS), large_spatial_join) %>%
  bind_rows()

plan(sequential)

place_votes <- us_places_counties %>%
  select(GEOID.x, per_gop, per_dem) %>%
  rename(GEOID = GEOID.x) %>%
  st_drop_geometry()

write.csv(place_votes, file = "~/Documents/Github/samegrassbutgreener/data/votes.csv", row.names = FALSE)


