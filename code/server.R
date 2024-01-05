library(dplyr)
library(data.table)
library(shiny)
library(leaflet)
library(tigris)

# Reading in pre-calculated data
# Move into reactive functions that load upon user input
politics <- fread("~/Documents/Github/samegrassbutgreener/data/votes.csv",
                  keepLeadingZeros = getOption("datatable.keepLeadingZeros", TRUE))
topo <- fread("~/Documents/Github/samegrassbutgreener/data/topo.csv",
              keepLeadingZeros = getOption("datatable.keepLeadingZeros", TRUE))
elevation <- fread("~/Documents/Github/samegrassbutgreener/data/elevation.csv",
                   keepLeadingZeros = getOption("datatable.keepLeadingZeros", TRUE))
airports <- fread("~/Documents/Github/samegrassbutgreener/data/airports_matched.csv",
                  keepLeadingZeros = getOption("datatable.keepLeadingZeros", TRUE))

# Downloading every census-designated place in the US
# Removing islands, territories
us_places <- places(state = NULL, year = 2022, cb = TRUE) %>%
  filter(!(STATEFP %in% c(60, #American Samoa
                          66, #Guam
                          69, #Mariana Islands
                          72, #Puerto Rico
                          78))) #Virgin Islands

us_places <- us_places %>%
  left_join(elevation, by = "GEOID") %>%
  left_join(topo, by = "GEOID") %>%
  left_join(politics, by = "GEOID") %>%
  left_join(airports, by = "GEOID")
  


server <- function(input, output) {
  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$Stadia.StamenTonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      )
    
  })
  
}

