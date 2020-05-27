
## This script creates a matrix of distances and bearings between every BGC
# Author: Lizzy Hoffman


#load libraries
library(tidyr)
library(sp)
library(sf)
library(dplyr)
library(stplanr)
library(tibble)
library(units)
library(magrittr)
library(flextable)
library(naniar)
library(parallel)

###############################################################################

#### read in bgc data
#input data should be in multipolygon form
bgc_data = st_read("USA_SubZoneMap_800m_4_May_2020_dissolved_multi.gpkg") %>%
  arrange(., BGC)


###############################################################################
#### calculate distance matrix
dist_matrix = st_distance(bgc_data)  %>%
  divide_by(1000) %>%
  drop_units() %>%
  round(., digits=2)

#### calculate bearing matrix
#find lines connecting nearest points for all feature pairs
nearness = st_nearest_points(bgc_data, bgc_data)

#convert to spatiallines for line_bearing
nearness_sp = as(nearness, "Spatial")

#convert to lat lon for bearing calculation
nearness_sp_latlon = spTransform(nearness_sp, CRS("+init=epsg:4326"))

#calculates the bearing of each line between features, returns a numeric vector
linebearing = line_bearing(nearness_sp_latlon)

#convert bearings to matrix
bearing_matrix = matrix(linebearing, nrow=nrow(bgc_data), ncol=nrow(bgc_data), byrow=TRUE) %>%
  as.data.frame() %>%
  round(., digits=2) %>%
  na_if(-180.00)

#### combine distance and bearing matrices into one
two_tables_into_one <- as.data.frame(do.call(cbind, lapply(1:ncol(dist_matrix), function(i) paste0(dist_matrix[ , i], " (", bearing_matrix[ , i], ")"  ) ))) %>%
  set_rownames(bgc_data$BGC) %>%
  set_colnames(bgc_data$BGC) %>%
  rownames_to_column(., "BGC")
  
#### create formatted table with results, can be exported as image
flextable(two_tables_into_one) %>%
  theme_box() %>%
  bold(., bold=TRUE, j="BGC")

#### export result tables as csv (no formatting)
# distance and bearing matrix
write.csv(two_tables_into_one, "distance_and_bearing_matrix.csv")

#distance matrix
write.csv(dist_matrix, "distance_matrix.csv")

#bearing matrix
write.csv(bearing_matrix, "bearing_matrix.csv")



###############################################################################
#calculation of distance from specific point to selected BGCs

#### define point of interest
longitude=-128
latitude=62

#### define list of bgcs of interest
bgc_list = c("BSJPap", "SASbo") 

#### create point feature
point = data.frame(longitude=longitude, latitude=latitude) %>%
  st_as_sf(., coords=c("longitude", "latitude"), crs=4326) %>%
  st_transform(., crs=3005)

#### subset BGC data to list of interest
bgc_shortlist = bgc_data %>%
  filter(., BGC %in% bgc_list) %>%
  st_transform(., crs=3005)

#### calculate distance between point and BGCs of interest
shortlist_dist = st_distance(point, bgc_shortlist) %>%
  divide_by(1000) %>%
  drop_units() %>%
  round(., digits=2) %>%
  t() %>%
  as.data.frame() %>%
  rename(Distance = "V1") %>%
  set_rownames(bgc_shortlist$BGC)

#### calculate bearing matrix
#find lines connecting nearest points for all feature pairs
shortlist_nearness = st_nearest_points(point, bgc_shortlist)

#convert to spatiallines for line_bearing
shortlist_nearness_sp = as(shortlist_nearness, "Spatial")

#convert to lat lon for bearing calculation
shortlist_nearness_sp_latlon = spTransform(shortlist_nearness_sp, CRS("+init=epsg:4326"))

#calculates the bearing of each line between features, returns a numeric vector
shortlist_linebearing = line_bearing(shortlist_nearness_sp_latlon) %>%
  as.data.frame() %>%
  round(., digits=2) %>%
  na_if(-180.00) %>%
  rename(Bearing = ".") %>%
  set_rownames(bgc_shortlist$BGC)

#### combine distance and bearing dfs into one
shortlist_comb = cbind(shortlist_dist, shortlist_linebearing) %>%
  rownames_to_column(., "BGC")

#### create formatted table with results
flextable(shortlist_comb) %>%
  theme_box() %>%
  bold(., bold=TRUE, j="BGC")

#### export result table as csv (no formatting)
write.csv(shortlist_comb, "spec_point_dist_and_bearing.csv")

