# INFO --------------------------------------------------------------------

# Purpose: Create a map of selected road links  

# SETUP -------------------------------------------------------------------

setwd("C:/Users/hanna/Desktop/Winter2020/spatio-temporal/assessment/analysis/UCL_Traffic_Forecast")
load("./0_source_data/UJTWorkSpace_Processed.RData")
library(tmap)
library(sf)
library(grid)

# VISUALIZING -------------------------------------------------------------

tmap_mode("plot")  

# border for the inset map
region = st_bbox(Subsetted,crs = st_crs(LondonBor)) %>% 
  st_as_sfc()

# border for the large map
bbox_new <- st_bbox(LCAPShp_sf) # current bounding box
xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
bbox_new[2] <- bbox_new[2] - (0.15 * yrange) # ymin - bottom
bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

# help from: https://geocompr.robinlovelace.net/adv-map.html#inset-maps
# create the main map 
link_map <- 
  tm_shape(LondonBor, bbox=bbox_new) + tm_fill(col='#c4bfb1', alpha=0.50) + tm_borders(col='white')+ # boroughs background
  tm_shape(LCAPShp_sf) + tm_lines(col='#575757', lwd =1)+ # all links
  tm_shape(Subsetted) + tm_lines(col='red', lwd=1)+ # selected links
  tm_shape(region) + tm_borders(lwd=3, col='black')+
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1)+
  tm_compass(type = "arrow", position = c("right", "top"))

inset_map <- 
  tm_shape(LondonBor, bbox=region) + tm_fill(col='#c4bfb1') + tm_borders(col='white')+ # boroughs background
  tm_shape(LCAPShp_sf) + tm_lines(col='#575757', lwd =1)+ # all links
  tm_shape(Subsetted) + tm_lines(col='red', lwd=3)# selected links
  tm_scale_bar(breaks = c(0, 0.5, 1), text.size = 0.8)+
  tm_layout(frame='black', frame.lwd=4)

link_map
print(inset_map, vp=viewport(0.30, 0.15, width=0.35, height=0.5))
