##########################################################
# Project: NeuroRehab Case Study 
# This file: costing the average round-trip travel to a clinic in North Dakota
# 05/2023
# Spencer Arbuckle
# R version 4.2.2
##########################################################

# NOTE: this code is quick and dirty in order to quickly get an average cost estimate.

# add necessary packages:
source("code/packages.R")

##### pre-analysis ####
# load county shape file for north dakota:
#nd <- read_sf("/Users/saarbuckle/Downloads/cb_2020_us_county_500k/cb_2020_us_county_500k.shp") %>% 
#  st_transform(4269) 
#nd <- nd[nd$STATE_NAME=="North Dakota",]

# merge shapefile with county population info (from 2020 census)
#nd_pop <- read.csv(here("data/nd_county_population.csv"))
#nd <- merge(nd,nd_pop, by.x = "NAMELSAD", by.y = "county_name")

# get county polygon centroids (centre of mass):
#sf_cent <- st_centroid(nd) %>%
#  st_transform(., 4269) 
#nd_coords <- st_coordinates(sf_cent)

#df_nd <- data.frame(county_name = nd$NAMELSAD, pop_2020 = nd$pop_2020, lat = nd_coords[,2], long=nd_coords[,1])
#write.csv(df_nd, "data/nd_counties.csv", row.names=FALSE)


# get latlon coordinates of ND counties (and their 2020 population counts)
df_nd <- read.csv(here("data/nd_counties.csv"))

# get latlon coordinates of 6 stroke hospitals in ND:
df_clinic <- read.csv(here("data/nd_stroke_hospitals.csv"))

# compute distance between each county centroid and the 6 hospitals
dist_km <- distm(cbind(df_nd$long,df_nd$lat), 
                 cbind(df_clinic$long,df_clinic$lat), 
                 fun = distHaversine) / 1000 # convert from m to km

# find the minimum distance (i.e., distance to nearest hospital for each county centroid)
dist_km <- apply(dist_km, 1, FUN = min)

# get the cost of each round one-way trip
roundtrip_cost <- dist_km * 0.4071 * 2 # 0.4071 is the reimbursable rate per km in ND, and 2 is to make the cost round-trip

# compute average round-trip cost, weighted by county populations
avg_roundtrip_cost <- (roundtrip_cost %*% df_nd$pop_2020) / sum(df_nd$pop_2020) 
