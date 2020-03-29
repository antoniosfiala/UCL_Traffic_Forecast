
# SET UP ------------------------------------------------------------------
# setwd("C:/Users/hanna/Desktop/Winter2020/spatio-temporal/assessment/analysis") 
# working directory assummed to be inside a "project" folder

load("./r_processing/0_source_data/UJTWorkSpace.RData")
library(sf)
library(tmap)

# PROCESSING --------------------------------------------------------------

# convert to sf object 
LCAPShp_sf <- st_as_sf(LCAPShp)

# read in polygon shp and set crs to match 
LondonBor <- st_read('./r_processing/1_shape_files/shapefiles/London_Borough_Excluding_MHW.shp') %>%
  st_transform(LondonBor, crs = st_crs(LCAPShp_sf))

# get all links that are totally within a given borough
LCAPShp_sf_sub <- LCAPShp_sf[st_within(LCAPShp_sf, 
                              LondonBor[LondonBor$NAME == "Islington",], 
                              sparse = FALSE),]

# check number of links in the subset
length(LCAPShp_sf_sub$OBJECTID)

# export object ID list
link_list <- LCAPShp_sf_sub$OBJECTID
write.csv(link_list,"link_sub_select.csv")

# plot to take a look
plot(LCAPShp_sf_sub)


# VISUALIZING -------------------------------------------------------------

# for interactive viewing 
tmap_mode("view")

# layer the boroughs, all links, and selected links 
tm_shape(LondonBor)+
  tm_polygons(col='white')+
  tm_shape(LCAPShp_sf)+
  tm_lines(col='lightgreen')+
tm_shape(LCAPShp_sf_sub)+
  tm_lines(col='red')
