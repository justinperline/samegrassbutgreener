library(tigris)
library(sf)
library(dplyr)
# library(raster)
# library(rgeoboundaries)
# library(zonal)
# library(AOI)
# library(terra)
# library(mapview)
# library(ncdf4)

# # Acquiring geometry and location data for every US county
# AOI <- aoi_get(state = "all", county = "all")
# 
# # Opening up ncdf4 file using ncdf4 package
# # nc file can be downloaded from: https://www.ncei.noaa.gov/products/land-based-station/us-climate-normals#tab-1027
# d <- nc_open("Downloads/tmax-2006_2020-monthly-normals-v1.0.nc")
# lon <- ncvar_get(d, "lon")
# lat <- ncvar_get(d, "lat")
# 
# # Subsetting ncdf4 file so that only January data is extracted
# v3 <- d$var[[3]]
# varsize <- v3$varsize
# start <- rep(1, 3)
# data3 <- ncvar_get(d, "mlytmax_norm", start = start, count = varsize )
# 
# nc_close(d)
#
# # Create raster object and then convert to SpatRaster object for Zonal purposes
# raster_obj <- raster(data3, xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat))
# rast <- rast(raster_obj)
# 
# # Match raster layer to zip code geometries
# pr_zone <- execute_zonal(data = rast, 
#                          geom = AOI, 
#                          ID = "fip_code", 
#                          join = TRUE,
#                          progress = TRUE)
# 
# mapview(pr_zone, zcol = "mean.layer")

# Abandoing this approach because not all counties seem to be getting picked up
# and I'm unsure if this is appropriately extracting January from the ncdf4 file


# Data downloaded from: https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/county/mapping/110/tmax/202301/1/value
jan_temp <- read.csv("~/Documents/Github/samegrassbutgreener/data/jan_temp.csv", skip = 4) %>%
  filter(!is.na(Rank)) %>%
  select(ID, X1901.2000.Mean) %>%
  rename(JanTemp = X1901.2000.Mean)

july_temp <- read.csv("~/Documents/Github/samegrassbutgreener/data/july_temp.csv", skip = 4) %>%
  filter(!is.na(Rank)) %>%
  select(ID, X1901.2000.Mean) %>%
  rename(JulyTemp = X1901.2000.Mean)

precip <- read.csv("~/Documents/Github/samegrassbutgreener/data/precip.csv", skip = 4) %>%
  filter(!is.na(Rank)) %>%
  select(ID, X1901.2000.Mean) %>%
  rename(Precipitation = X1901.2000.Mean)

# Merging natural amenities data with spatial data
# Have to download old county data because some records do not match 2022 county codes
counties <- counties(cb = TRUE, year = 2022) %>%
  filter(STUSPS != "CT") %>% #Connecticut counties became "Planning Regions" in 2022
  mutate(GEOID = case_when(GEOID == "12086" ~ "12025", #Dade -> Miami-Dade
                           GEOID == "46102" ~ "46113", #Shannon -> Oglala Lakota
                           TRUE ~ GEOID)) %>%
  bind_rows(counties(state = "CT", cb = TRUE, year = 2021)) %>%
  mutate(COUNTYID = ifelse(STUSPS == "DC", "MD-511", paste0(STUSPS, "-", COUNTYFP)))

county_climates <- counties %>%
  left_join(jan_temp, by = c("COUNTYID" = "ID")) %>%
  left_join(july_temp, by = c("COUNTYID" = "ID")) %>%
  left_join(precip, by = c("COUNTYID" = "ID")) %>%
  mutate(Seasonality = JulyTemp - JanTemp)
