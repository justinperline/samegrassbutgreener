library(tidycensus)
library(dplyr)
library(tigris)
library(sf)

census_api_key("429a314d2802197a456809f4b90cae60226291f9")

demographics <- get_acs(geography = "place",
                        variables = c(houseincome = "B19013_001",
                                      totpop = "B01003_001",
                                      medianage = "B01002_001",
                                      commuters = "S0101_C01_026",
                                      male = "B01001A_002",
                                      female = "B01001A_017",
                                      medincome = "B06011_001"),
                        geometry = FALSE,
                        output = "wide",
                        year = 2022)

demographics <- demographics %>%
  select(GEOID, houseincomeE, totpopE, medianageE, medincomeE)

write.csv(demographics, file = "~/Documents/Github/samegrassbutgreener/data/demographics.csv", row.names = FALSE)


