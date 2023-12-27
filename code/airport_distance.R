library(tigris)
library(mapboxapi)
library(dplyr)
library(tibble)
library(tidyr)
library(sf)

# Function to read in and filter airport data
read_airports <- function(url, crs) {
  airports <- st_read(url) %>%
    st_transform(crs) %>%
    filter(OWNER == 'Public') %>%
    filter(STATE %in% states)
  
  return(airports)
}

# Downloading every census-designated place in the US
# Removing islands, territories
us_places <- places(state = NULL, year = 2022, cb = TRUE) %>%
  filter(!(STATEFP %in% c(60, #American Samoa
                          66, #Guam
                          69, #Mariana Islands
                          72, #Puerto Rico
                          78))) %>% #Virgin Islands
  rownames_to_column("PlaceNum")

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
  rownames_to_column("AirportNum")

# Calculating distance between every place and every airport
# Limiting to 3 closest airports to each place
dist <- us_places %>%
  st_centroid() %>%
  st_distance(airports) %>%
  magrittr::divide_by(1000)

place_airports <- t(apply(dist, 1, function(row) order(row)[1:3])) %>%
  as.data.frame() %>%
  rownames_to_column("PlaceNum") %>%
  mutate(PlaceNum = as.numeric(PlaceNum)) %>%
  pivot_longer(cols = V1:V3,
               names_to = "Rank",
               values_to = "AirportNum")

place_airports_timed <- data.frame()

# Looping through every place and using Mapbox API to calculate travel time to its 3 closest airports
for (i in 8001:10000) {
  print(i)
  
  closest_airport_nums <- place_airports %>%
    filter(PlaceNum == i) %>%
    pull(AirportNum)
  
  closest_airports <- airports %>%
      filter(AirportNum %in% closest_airport_nums)
  
  place <- us_places %>%
    filter(PlaceNum == i)
  
  times <- mb_matrix(origins = place,
                     destinations = closest_airports)
  
  times <- times %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("Rank") %>%
    rename(TravelTime = V1)
  
  place_airports_timed_add <- place_airports %>%
    filter(PlaceNum == i) %>%
    left_join(times, by = "Rank")
  
  place_airports_timed <- bind_rows(place_airports_timed, place_airports_timed_add)
  
  Sys.sleep(1)
}

write.csv(place_airports_timed, file = "~/Documents/Github/samegrassbutgreener/data/timedairports.csv")
