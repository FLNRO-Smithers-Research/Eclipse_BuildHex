## This script builds a base hex polygon layer for use in WNA BGC modelling 
# Author: Lizzy Hoffman

#### load required libraries
library(sp)
library(sf)
library(dplyr)
library(tibble)
require(raster)
require(tidyverse)

#### read in boundary layer in 3005 and select a state(s) to generate the hex within.
states <- c("NV")#"ID", "MT"
wna_boundary = st_read("D:/CommonTables/BC_AB_US_Shp/WNA_State_Boundaries.gpkg") %>% st_as_sf() %>% filter(State %in% states) %>%
  #st_transform( crs = st_crs(3005)) %>% 
  st_buffer(., dist = 500) %>%
  as(., "Spatial")

#### create point centroid layer and hex polygon layer
wna.pt = spsample(wna_boundary, type = "hexagonal", cellsize = 400)

wna.hex = HexPoints2SpatialPolygons(wna.pt)

#### write points and hexes to geopackage
as(wna.hex, "sf") %>% st_transform( crs = st_crs(3005)) %>% 
  rownames_to_column(., var="hex_id") %>%
  st_write(., "./outputs/NV_bgc_hex400.gpkg", delete_dsn = TRUE)

as(wna.pt, "sf") %>% st_transform( crs = st_crs(3005)) %>% 
  rownames_to_column(., var="hex_id") %>%
  st_write(., "./outputs/NV_bgc_pts400.gpkg", delete_dsn = TRUE)

st_bbox(wna.hex)

##-------------------------------
##### Create point set for submitting to ClimateNA

##set projection to NAD83 lat/long for both DEM and points file
CRS.NAD83 <- CRS("+init=epsg:4269")
WNA_DEM <- raster("D:/CommonTables/DEMs/WNA_DEM_SRT_30m.tif")
projection(WNA_DEM) <- CRS.NAD83 
wna.pt2 <- st_as_sf(wna.pt) %>% st_transform( crs = st_crs(CRS.NAD83))
#extracts elevation for each point in p
elev <- ""
elev <- raster::extract(WNA_DEM,wna.pt2)
elev <- as.data.frame(elev) %>% rownames_to_column("ID1")

wna.pt3 <-  st_coordinates(wna.pt2) 
wna.pt3 <- as.data.frame(wna.pt3) %>% rownames_to_column("ID1")
wna_cb <- left_join(wna.pt3, elev) %>% rename("latitude" = Y, "longitude" = X)

wna_cb$ID2 <- ""
pts.info2 <- wna_cb %>% dplyr::select(ID1, ID2, latitude, longitude, elev) #variable order for ClimateWNA .csv
write.csv (pts.info2, "./outputs/NV_400m_HexPts.csv", row.names = FALSE) 

