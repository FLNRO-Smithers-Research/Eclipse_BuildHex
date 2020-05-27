## This script dissolves input BGC data using GRASS v.dissolve and exports as geopackage
#Author: Lizzy Hoffman

library(sf)
library(dplyr)
library(rgrass7)
library(stars)

#########################################
## import undissolved data, transform to desired project and export as geopackage

#undissolved input layer
bgc_undissolved = st_read("USA_SubZoneMap_800m_31Mar2020_ungrouped.shp") %>%   
  st_transform(., crs = 3005) %>%
  st_write(., "bgc_undissolved_input.gpkg")

#st_is_valid does not show any invalid geometries
#st_is_valid(bgc_undissolved)


########################################
## dissolve undissolved BGC data using GRASS v.dissolve

# initialisation of GRASS
initGRASS(gisBase = "/Applications/GRASS-7.4.4.app/Contents/Resources", 
          home = tempdir(),
          mapset = 'PERMANENT',
          override=TRUE)

#set projection for GRASS session
execGRASS("g.proj", flags = "c", epsg = 3005) 

#import undissolved data into GRASS
execGRASS("v.in.ogr", input = "bgc_undissolved_input.gpkg", output = "bgc_undiss_input_gr", snap=1e-09, flags="overwrite")  

#Run GRASS v.dissolve tool
execGRASS("v.dissolve", input = "bgc_undiss_input_gr", column = "BGC", output = "bgc_diss_output_gr") #good

#export result from GRASS to R session
use_sf()
bgc_diss_output_r = readVECT("bgc_diss_output_gr")

#prepare R object for export as geopackage
bgc_diss_output_r_1 = bgc_diss_output_r %>%
  select(., -cat)

#export as single polygon geopackage
bgc_diss_output_r_1 %>%
  st_write(., "USA_SubZoneMap_800m_4_May_2020_dissolved.gpkg")

#export as multipolygon geopackage aggregated by BGC (needed for distance matrix script)
aggregate(bgc_diss_output_r_1, by=list(bgc_diss_output_r$BGC), function(x) x[1]) %>% 
  select(., -Group.1) %>%
  st_write(., "USA_SubZoneMap_800m_4_May_2020_dissolved_multi.gpkg") 


