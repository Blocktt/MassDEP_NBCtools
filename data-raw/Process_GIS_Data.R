# Prepare data for use in the package
#
# Ben.Block@tetratech.com
# 2023-02-01
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save GIS shapefiles as RDA
# saves space and should load quicker
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Prep ####
(wd <- getwd()) # assume is package directory
library(sf)
library(dplyr)
library(rmapshaper)
# Get data and process ####
fn_shp <- file.path(wd, "data-raw", "GIS_Data")

## dams ####
dams_shp <- sf::st_read(dsn = fn_shp, layer = "DAMS_PT") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84')
object.size(dams_shp)

## superfund ####
SEMS_shp <- sf::st_read(dsn = fn_shp, layer = "SEMS") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84')
object.size(SEMS_shp)

## NPDES ####
NPDES_shp <- sf::st_read(dsn = fn_shp, layer = "NPDES_Major") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84')
object.size(NPDES_shp)

## TRIs ####
TRIs_shp <- sf::st_read(dsn = fn_shp, layer = "TRIs") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84')
object.size(TRIs_shp)

## PWS ####
PWS_SW_shp <- sf::st_read(dsn = fn_shp, layer = "PWS_SW_PT") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84')
object.size(PWS_SW_shp)

## basins ####
basins_shp <- sf::st_read(dsn = fn_shp, layer = "305BasinMergeMarch2023_20mSimp") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

## centroids ####
AU_Centroids <- sf::st_transform(basins_shp, 2249) %>% # project data
  sf::st_centroid()
Centroids_4326 <- sf::st_transform(AU_Centroids, 4326) # transform to WGS 1984
Centroids_final <- Centroids_4326 %>% 
  select(AU_ID, geometry) %>% 
  mutate(Longitude = sf::st_coordinates(.)[,1]
         , Latitude = sf::st_coordinates(.)[,2]) %>% # obtain lat/long
  st_drop_geometry()
# Export data
write.table(Centroids_final, file.path(wd, "inst", "shiny-examples"
                                       , "MassNBCtools", "Data"
                                       , paste0("AU_Centroids.csv"))
            , row.names = FALSE, sep = ",", na = "")

## AU polylines ####
polylines_shp <- sf::st_read(dsn = fn_shp, layer = "2022arcs_mergeDRAFT_10mSimp") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

## Zone II ####
Zone2_shp <- sf::st_read(dsn = fn_shp, layer = "ZONE2_POLY_DISSOLVE_10mSimp") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

# Save as RDA for use in package ####
## dams ####
GISlayer_dams <- dams_shp
usethis::use_data(GISlayer_dams, overwrite = TRUE)

## superfunds ####
GISlayer_SEMS <- SEMS_shp
usethis::use_data(GISlayer_SEMS, overwrite = TRUE)

## NPDES ####
GISlayer_NPDES <- NPDES_shp
usethis::use_data(GISlayer_NPDES, overwrite = TRUE)

## TRIs ####
GISlayer_TRIs <- TRIs_shp
usethis::use_data(GISlayer_TRIs, overwrite = TRUE)

## PWS ####
GISlayer_PWS_SW <- PWS_SW_shp
usethis::use_data(GISlayer_PWS_SW, overwrite = TRUE)

## basins ####
GISlayer_AUpoly <- basins_shp
usethis::use_data(GISlayer_AUpoly, overwrite = TRUE)

## AU polylines ####
GISlayer_AUflow <- polylines_shp
usethis::use_data(GISlayer_AUflow, overwrite = TRUE)

## Zone II ####
GISlayer_Zone2 <- Zone2_shp
usethis::use_data(GISlayer_Zone2, overwrite = TRUE)

# Helpful links
# https://www.r-bloggers.com/2021/03/simplifying-geospatial-features-in-r-with-sf-and-rmapshaper/
# https://stackoverflow.com/questions/54734771/sf-write-lat-long-from-geometry-into-separate-column-and-keep-id-column
# https://r-spatial.github.io/sf/articles/sf1.html#crs
# https://spatialreference.org/ref/?search=nad+83+massachusetts&srtext=Search