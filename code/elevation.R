library(tigris)
library(sf)
library(dplyr)
library(elevatr)

# Downloading every census-designated place in the US
# Removing islands, territories
us_places <- places(state = NULL, year = 2022, cb = TRUE) %>%
  filter(!(STATEFP %in% c(60, #American Samoa
                          66, #Guam
                          69, #Mariana Islands
                          72, #Puerto Rico
                          78))) #Virgin Islands

crs <- st_crs(us_places)

# Reducing polygons down to centroids for simplicity
us_places <- us_places %>%
  st_centroid()

# Using AWS elevation API to calculate for every location
us_places <- get_elev_point(us_places,
                            prj = crs,
                            src = "aws",
                            units = "feet")

us_places <- us_places %>%
  st_drop_geometry() %>%
  select(GEOID, elevation)

write.csv(us_places, "~/Documents/Github/samegrassbutgreener/data/elevation.csv", row.names = FALSE)
