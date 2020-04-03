#required libraries
library(sp)
library(sf)
library(dplyr)
require(raster)
require(rgdal)
require(tidyverse)
require(velox)
require(fasterize)

#read in boundary layer
wna_boundary = st_read("WashingtonState_Boundary.shp") %>%
  st_buffer(., dist=500) %>%
  as(., "Spatial")

# wna_boundary = st_read("BCProvincialOutline_hires2.shp") %>%
#   st_buffer(., dist=500) %>%
#   as(., "Spatial")


#create point centroid layer and hex polygon layer
wna.pt = spsample(wna_boundary, type = "hexagonal", cellsize = 400)
wna.hex = HexPoints2SpatialPolygons(wna.pt)

#write points and hexes to shapefile
as(wna.hex, "sf") %>%
  st_write(., "./outputs/WA_bgc_hex_400m.shp")#, delete_dsn=TRUE

as(wna.pt, "sf") %>%
  st_write(., "./outputs/WA_bgc_pt_400m.shp")#, delete_dsn=TRUE)

#######Create points file for submission to ClimateNA
    wna.pt <- st_read("./outputs/WA_bgc_pt_400m.shp")
   # wna.hex <- st_read("./outputs/USA_bgc_hex_800m.shp")
####set projection to NAD83
CRS.NAD83 <- CRS("+init=epsg:4269")
wna.pt2 <- as(wna.pt, "Spatial")
wna.pt2 <- spTransform(wna.pt2, CRS.NAD83)

# add georeferencing to points
coords <- wna.pt2@coords
coords <- as.data.frame(coords)

### Reada in DEM for WNA and transform to NAD83
WNA_DEM <- raster("D:/CommonTables/DEMs/WNA_DEM_clipped.tif")
projection(WNA_DEM) <- CRS.NAD83

#extracts elevation for each point in p
coords$elev <- raster::extract(WNA_DEM,wna.pt2)
colnames(coords) <- c("longitude","latitude","elevation")
coords <- as_tibble(rownames_to_column(coords, var = "ID1"))
coords$ID2 <- ""
pts.info2 <- coords %>% dplyr::select(ID1, ID2, latitude, longitude, elevation) #variable order for ClimateWNA
write.csv (pts.info2,  "./outputs/WA_400m_HexPts.csv", row.names = FALSE) 

#OPTIONAL overlay of points over administrative boundaries

### Over function is very slow. Probably fasterize polygons and then extract values at points?

WNA <- readOGR(dsn = "D:/CommonTables/BC_AB_US_Shp", layer = "WNA_MergeStates_WGS84")#layer = "USA_States"
# set projection
WNA <- spTransform(WNA, CRS.NAD83)
WNA$State <- as.character (WNA$State)
pnt_state <- over(wna.pt2, WNA[,"State"],  minDimension = 1, returnList = FALSE)
pnt_state <- as_tibble(rownames_to_column(pnt_state, var = "ID1"))
pts.info <- left_join(pnt_state, coords,  by = "ID1")
pts.info <- pts.info %>% drop_na(State)
pts.info2 <- pts.info %>% dplyr::select(ID1, State, latitude, longitude, elevation) #variable order for ClimateWNA
write.csv (pts.info2,  "./outputs/USA_800m_HexPts_State.csv", row.names = FALSE) ### this file to be submitted to ClimateWNA


#### OPTIONAL overlay points over BGC boundaries

BGC <- readOGR(dsn = "D:/CommonTables/BGC_maps", layer = "WNA_BGC_v11_31Dec2019")#
BGC <- spTransform(BGC, CRS.NAD83)
BGC$BGC <- as.character(BGC$BGC)
pnt_BGC <- over(wna.pt2, BGC[,"BGC"],  minDimension = 1, returnList = FALSE)
pnt_BGC <- as_tibble(rownames_to_column(pnt_BGC, var = "ID1"))
pts.info <- left_join(pnt_BGC, coords,  by = "ID1")
pts.info <- pts.info %>% drop_na(BGC)



pts.info2 <- pts.info %>% dplyr::select(ID1, BGC, latitude, longitude, elevation) #variable order for ClimateWNA
write.csv (pts.info2,  "./outputs/USA_800m_HexPts_BGC.csv", row.names = FALSE) ### this file to be submitted to ClimateWNA

##########Now Submit file to ClimateNa to add climate data for further analysis




###over function is very slow  - looking for other methods
#  pnts <- st_read(dsn = "TrainingPts", layer = "ArcticZonePlotLocations"
# pnts <-  pnts[!duplicated(pnts[,c(6:7)]),] # remove any points that have duplicated locations
# pnts$Zone <- as.factor(pnts$Zone)
# pnts2 <- SpatialPointsDataFrame( pnts[,6:7],pnts) # convert data frame to spatial points
# crs(pnts2) <- CRS ("+init=epsg:4326") # set crs to WGS84
# crs(wna.pt2)
# write a shapefile
#writeOGR(pnts2,   dsn = "./TrainingPts", layer = "ArcticZoneTrainingPts", driver="ESRI Shapefile", overwrite_layer=TRUE)

# pnts_sf <- st_as_sf(coords)
# pnts_sf <- st_transform(pnts_sf, CNA_proj)
# mapview(pnts_sf)

# pnts.xy <- st_coordinates(wna.pt)
# data <- data.frame(coordinates(wna.pt2),
#                    wna.pt2$name, 
#                    extract(BGC2, germany.places.mrc.sample))
# names(data) <- c("x", "y", "name", "value")
# pnt_BGC2 <- raster::extract(BGC2, wna.pt2)
# pnt_BGC <- over(wna.pt2, BGC[,"BGC"],  minDimension = 1, returnList = FALSE)
# pnt_BGC <- as_tibble(rownames_to_column(pnt_BGC, var = "ID1"))
# pts.info <- left_join(pnt_BGC, coords,  by = "ID1")
# pts.info <- pts.info %>% drop_na(BGC)

####Load up raster stack


#BGC2 <- fasterize(BGC, raster::raster(WNA_DEM))

#raster_stk <- stackOpen("D:/CommonTables/ClimateNA_rasters/ClimateNA_stk.tif")
#ClimateNA_names <- as.character (names(raster_stk))
## convert to velox object for fast extraction
#tic()
#vx <- velox::velox(BGC2) ## this takes about a minute
#toc()

# extract values from raster stack
#tic()
#raster.xy.s <- vx$extract_points(sp = wna.pt) ## this is incredible fast .5 secs
#toc()
#####produce final training set
#raster.xy.s <- cbind(raster.xy.s, pnts.xy)
#training_dat <- as.data.frame(raster.xy.s)
#colnames(training_dat) <- names(raster_stk)
#colnames(training_dat) [23:24] <- c("X", "Y")
# <- cbind(pnts, training_dat)
#training_dat2 <-  training_dat2 [!(training_dat2$ClimateNA_DEM == "NA"),]

