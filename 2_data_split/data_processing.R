# INFO --------------------------------------------------------------------

# Purpose: Select road links of interest, join with the road speed data for each link and the time info
# Link selection approach: All links that are to some extent within a given borough are selected (can extend outside of the borough) - set here for City of London
# Inputs: UJT, LCAPShp, dates, London Boroughs Shapefile
# Outputs: traffic_speeds.csv, link_info.csv

# traffic_speeds.csv: 
# - Columns are the selected links, indexed by LCAP_ID
# - Rows are the traffic speeds in sec/m
# - Last two columns contain the time info (Time and Date from 'dates' table) 

# link_info.csv:
# - Columns are the attributes for each link, from LCAPShp (no geog info included)
# - Rows are the selected links

# SET UP ------------------------------------------------------------------

# setwd("C:/Users/hanna/Desktop/Winter2020/spatio-temporal/assessment/analysis/UCL_Traffic_Forecast") 
# working directory assummed to be inside a "project" folder
load("./0_source_data/UJTWorkSpace.RData")
library(sf)
library(tmap)
library(dplyr)

# REFORMATTING DATA -------------------------------------------------------

LCAPShp_sf <- st_as_sf(LCAPShp) # convert to sf object 
UJT_df <- as.data.frame(UJT) # convert matrix to df
UJT_df <- as.data.frame(t(UJT_df)) # transpose to get LCAP_ID as column
UJT_df$LCAP_ID <- as.numeric(rownames(UJT_df)) # add LCAP_ID as column for join

# JOINING LINK GEOMETRY WITH UJT DATA -------------------------------------

Joined <- st_as_sf(inner_join(UJT_df, LCAPShp_sf, by = 'LCAP_ID')) # subset shp data to only get those with UJT data

# SELECTING LINKS BASED ON LOCATION ---------------------------------------

# read in polygon shp and set crs to match 
LondonBor <- st_read('./1_shape_files/shapefiles/London_Borough_Excluding_MHW.shp') %>%
  st_transform(LondonBor, crs = st_crs(LCAPShp_sf))

# get all links that are totally within a given borough
Subsetted <- Joined[st_intersects(Joined, 
                              LondonBor[LondonBor$NAME == "City of London",], 
                              sparse = FALSE),]

length(Subsetted$OBJECTID) # check number of links in the subset

# VISUALIZING -------------------------------------------------------------

tmap_mode("view") # for interactive viewing 

# layer the boroughs, all links with data, and selected links 
tm_shape(LondonBor)+
  tm_polygons(col='white')+
  tm_shape(Joined)+
  tm_lines(col='lightgreen')+
tm_shape(Subsetted)+
  tm_lines(col='red')

# RESTRUCTURE AND EXPORT TO CSV -------------------------------------------

# selet only the traffic speeds and transpose 
traffic_speeds <- Subsetted[,1:5400] %>% st_drop_geometry()
rownames(traffic_speeds) <- Subsetted$LCAP_ID
traffic_speeds <- as.data.frame(t(traffic_speeds))

# add time information
traffic_speeds$Time <- dates$Time
traffic_speeds$Date <- dates$Date

# get attributes for each of the links 
link_info <- Subsetted[,5401:5442] %>% st_drop_geometry()

# write data to csv
write.csv(traffic_speeds, './2_data_split/traffic_speeds.csv', row.names=FALSE)
write.csv(link_info, './2_data_split/link_info.csv', row.names=FALSE)