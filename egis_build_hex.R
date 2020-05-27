## This script builds a base hex polygon layer for use in WNA BGC modelling 
# Author: Lizzy Hoffman

#### load required libraries
library(sp)
library(sf)
library(dplyr)
library(tibble)

#### read in boundary layer
wna_boundary = st_read("BC_AB_WesternStates_Fixed_v2_300m.gpkg") %>%
  st_buffer(., dist=500) %>%
  as(., "Spatial")

#### create point centroid layer and hex polygon layer
wna.pt = spsample(wna_boundary, type = "hexagonal", cellsize = 800)

wna.hex = HexPoints2SpatialPolygons(wna.pt)

#### write points and hexes to geopackage
as(wna.hex, "sf") %>%
  rownames_to_column(., var="hex_id") %>%
  st_write(., "wna_bgc_hex.gpkg")

as(wna.pt, "sf") %>%
  rownames_to_column(., var="hex_id") %>%
  st_write(., "wna_bgc_pt.gpkg")



