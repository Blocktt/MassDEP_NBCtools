# Prepare data for use in the package
#
# Ben.Block@tetratech.com
# 2023-02-01
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save GIS shapefiles as RDA
# saves space and should load quicker
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Prep ####
wd <- getwd() # assume is package directory
library(sf)
library(dplyr)
library(rmapshaper)
# Get data and process ####
fn_shp <- file.path(wd, "data-raw", "GIS_Data")

## dams ####
dams_shp <- sf::st_read(dsn = fn_shp, layer = "DAMS_PT") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84')
object.size(dams_shp)

## basins ####
basins_shp <- sf::st_read(dsn = fn_shp, layer = "305BasinMerge2022") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84')

basins_2249 <- sf::st_transform(basins_shp, 2249) # project data
basins_simp <- rmapshaper::ms_simplify(basins_2249
                                       , keep_shapes = TRUE) # simplify polygons
basins_final <- sf::st_transform(basins_simp, 4326) # transform to WGS 1984

## centroids ####
AU_Centroids <- sf::st_centroid(basins_2249)
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
polylines_shp <- sf::st_read(dsn = fn_shp, layer = "2022arcs_mergeDRAFT") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84')

polylines_2249 <- sf::st_transform(polylines_shp, 2249) # project data
polylines_simp <- rmapshaper::ms_simplify(polylines_2249
                                       , keep_shapes = TRUE) # simplify polygons
polylines_final <- sf::st_transform(polylines_simp, 4326) # transform to WGS 1984


# Save as RDA for use in package ####
## dams ####
GISlayer_dams <- dams_shp
usethis::use_data(GISlayer_dams, overwrite = TRUE)

## basins ####
GISlayer_AUpoly <- basins_final
usethis::use_data(GISlayer_AUpoly, overwrite = TRUE)

## AU polylines ####
GISlayer_AUflow <- polylines_final
usethis::use_data(GISlayer_AUflow, overwrite = TRUE)

# Helpful links
# https://www.r-bloggers.com/2021/03/simplifying-geospatial-features-in-r-with-sf-and-rmapshaper/
# https://stackoverflow.com/questions/54734771/sf-write-lat-long-from-geometry-into-separate-column-and-keep-id-column


# object.size(polylines_shp)
# object.size(polylines_final)
# 
# leaflet() %>%
#   addPolylines(data = polylines_shp, color = "red", weight = 2)%>%
#   addPolylines(data = polylines_final, color = "blue", weight = 2) %>%
#   addTiles() %>%
#   addProviderTiles(providers$Esri.WorldStreetMap)