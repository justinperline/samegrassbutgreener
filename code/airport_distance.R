library(tigris)
library(mapboxapi)
library(dplyr)
library(tidyr)
library(sf)
library(furrr)
library(tibble)

# Functions ----
# Function to read in and filter airport data
read_airports <- function(url, crs) {
  airports <- st_read(url) %>%
    st_transform(crs) %>%
    filter(OWNER == 'Public') %>%
    filter(STATE %in% states)
  
  return(airports)
}

# Function to calculate travel time between a place and its three closest airports
calc_travel_time <- function(geoid) {
  
  closest_airport_nums <- place_airports %>%
    filter(GEOID == geoid) %>%
    pull(FAA_ID)
  
  closest_airports <- airports %>%
    filter(FAA_ID %in% closest_airport_nums)
  
  place <- us_places %>%
    filter(GEOID == geoid)
  
  times <- mb_matrix(origins = place,
                     destinations = closest_airports)
  
  if (length(times) > 0) {
    times <- times %>%
      as.data.frame() %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column("Rank") %>%
      rename(TravelTime = V1)
    
    place_airports_timed_add <- place_airports %>%
      filter(GEOID == geoid) %>%
      left_join(times, by = "Rank")
  }
  
  # Avoiding API rate limits
  Sys.sleep(1)
  
  return(place_airports_timed_add)
}

# Main ----
# Downloading every census-designated place in the US
# Removing islands, territories
us_places <- places(state = NULL, year = 2022, cb = TRUE) %>%
  filter(!(STATEFP %in% c(60, #American Samoa
                          66, #Guam
                          69, #Mariana Islands
                          72, #Puerto Rico
                          78))) #Virgin Islands

# Storing original coordinate system to convert airport datasets
crs <- st_crs(us_places)
states <- unique(us_places$STUSPS)

# Downloading major airport data
major_airport_url <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Airports_by_scale/FeatureServer/1/query?outFields=*&where=1%3D1&f=geojson"
major_airports <- read_airports(url = major_airport_url,
                          crs = crs)

# Downloading mid-major airport data
mid_airports_url <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Airports_by_scale/FeatureServer/2/query?outFields=*&where=1%3D1&f=geojson"
mid_airports <- read_airports(url = mid_airports_url,
                              crs = crs)

airports <- rbind(major_airports, mid_airports) %>%
  mutate(AIRTYPE = ifelse(ENPLANEMEN > 1000000, 1, 2)) %>%
  rownames_to_column("AirportNum") %>%
  mutate(AirportNum = as.numeric(AirportNum))

# Calculating distance between every place and every airport
# Limiting to 3 closest airports to each place
dist <- us_places %>%
  st_centroid() %>%
  st_distance(airports) %>%
  magrittr::divide_by(1000)

place_airports <- t(apply(dist, 1, function(row) order(row)[1:3])) %>%
  as.data.frame() %>%
  cbind(us_places %>% select(GEOID)) %>%
  st_drop_geometry(.) %>%
  select(-c(geometry)) %>%
  pivot_longer(cols = V1:V3,
               names_to = "Rank",
               values_to = "AirportNum") %>%
  left_join(airports %>% select(AirportNum, FAA_ID), by = "AirportNum") %>%
  st_drop_geometry(.) %>%
  select(-c(geometry, AirportNum))

place_airports_dist <- place_airports <- t(apply(dist, 1, function(row) sort(row)[1:3])) %>%
  as.data.frame() %>%
  cbind(us_places %>% select(GEOID)) %>%
  st_drop_geometry(.) %>%
  select(-c(geometry)) %>%
  pivot_longer(cols = V1:V3,
               names_to = "Rank",
               values_to = "AirportDist")

place_airports <- place_airports %>%
  left_join(place_airports_dist, by = c("GEOID" = "GEOID",
                                        "Rank" = "Rank"))

# Using parallel processing to calculate travel time to every airport
n_cores <- future::availableCores()
plan(multisession, workers = n_cores)
place_airports_timed <- future_map(unique(us_places$GEOID), calc_travel_time) %>%
  bind_rows()

# Returning to sequential processing
plan(sequential)

# Slicing to get the closest airport
min_place_airports_timed <- place_airports_timed %>%
  group_by(GEOID) %>%
  slice_min(order_by = AirportDist, #for now, using distance over time since it seems more reliable
            with_ties = FALSE)

us_places <- us_places %>%
  left_join(min_place_airports_timed, by = "GEOID")

write.csv(us_places, file = "~/Documents/Github/samegrassbutgreener/data/airports_matched.csv")

