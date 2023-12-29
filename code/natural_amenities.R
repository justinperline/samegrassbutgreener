library(tigris)
library(sf)
library(dplyr)
library(readxl)
library(mapview)
library(tibble)
library(furrr)

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
  filter(STUSPS != "CT") %>% #Connecticut counties became "Planning Regions" in 2022
  mutate(GEOID = case_when(GEOID == "12086" ~ "12025", #Dade -> Miami-Dade
                           GEOID == "46102" ~ "46113", #Shannon -> Oglala Lakota
                           TRUE ~ GEOID)) %>%
  bind_rows(counties(state = "CT", cb = TRUE, year = 2021))

natural_counties <- natural %>%
  left_join(counties, by = c("FIPS Code" = "GEOID")) %>%
  st_as_sf() %>%
  select(`FIPS Code`, NAME, STUSPS, TOPO)

# Plotting county by topography code
mapview(natural_counties, zcol = "TOPO")

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
    
    natural_counties_state <- natural_counties %>%
      st_filter(us_places_state,
                .predicate = st_is_within_distance,
                dist = 100000)
    
    us_places_counties <- us_places_state %>%
      st_join(natural_counties_state,
            join = st_intersects,
            largest = TRUE)
    
    # In case of failure, try this
    # intersections <- st_intersection(us_places_state, natural_counties_state)
    # intersections <- intersections %>%
    #   filter(st_is_valid(geometry) == TRUE) %>%
    #   mutate(AREA = as.numeric(st_area(geometry))) %>%
    #   group_by(GEOID) %>%
    #   slice_max(order_by = AREA)
  
  return(us_places_counties)
}

# Using parallel processing to attempt this spatial join one state at a time
n_cores <- future::availableCores()
plan(multisession, workers = n_cores)

us_places_counties <- future_map(unique(us_places$STUSPS), large_spatial_join) %>%
  bind_rows()

plan(sequential)

topo <- us_places_counties %>%
  st_drop_geometry() %>%
  select(GEOID, TOPO)

write.csv(topo, file = "~/Documents/Github/samegrassbutgreener/data/topo.csv", row.names = FALSE)

# spatially_weighted_max <- function(x) {
#   x <- st_sfc(x, crs = st_crs(natural_counties))
#   areas <- st_intersection(natural_counties, x) #can filter natural counties down to the ones from the join on 68
#   areas %>%
#     mutate(area = st_area(.)) %>%
#     st_drop_geometry(.) %>%
#     group_by(TOPO) %>%
#     summarize(area = sum(area)) %>%
#     arrange(desc(area)) %>%
#     slice(1) %>%
#     pull(TOPO)
# }
# 
# us_places$TOPO_MAX <- sapply(us_places$geometry, spatially_weighted_max)