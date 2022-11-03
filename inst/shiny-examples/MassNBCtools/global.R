# Shiny Global File

# Version ----
pkg_version <- "0.0.0.9005"

# Packages----
library(shiny)
library(shinydashboard)
library(shinydashboardPlus) # only using for footer
library(shinyjs) # used for download button enable
library(DT)
library(ggplot2)
library(readxl)
library(reshape2)
library(knitr)
library(dplyr)
library(utils)
library(maps)
library(rmarkdown)
library(markdown)
library(tidyr)
library(leaflet)
library(mapview) # used to download leaflet map
# library(rgdal)

# Source ----
db_main_sb           <- source("external/db_main_sb.R", local = TRUE)$value
db_main_body         <- source("external/db_main_body.R", local = TRUE)$value
tab_code_about       <- source("external/tab_about.R", local = TRUE)$value
## Temperature
tab_code_temp_AU<- source("external/tab_temp_AU.R", local = TRUE)$value
tab_code_temp_crit<- source("external/tab_temp_crit.R", local = TRUE)$value
tab_code_temp_LC<- source("external/tab_temp_LC.R", local = TRUE)$value
tab_code_temp_pointsrc<- source("external/tab_temp_pointsrc.R", local = TRUE)$value
tab_code_temp_dams<- source("external/tab_temp_dams.R", local = TRUE)$value
tab_code_temp_withdrwl<- source("external/tab_temp_withdrwl.R", local = TRUE)$value
tab_code_temp_spike<- source("external/tab_temp_spike.R", local = TRUE)$value

## Dissolved oxygen
tab_code_DO_site<- source("external/tab_DO_site.R", local = TRUE)$value
tab_code_DO_disturb<- source("external/tab_DO_disturb.R", local = TRUE)$value

## pH
tab_code_pH_site<- source("external/tab_pH_site.R", local = TRUE)$value
tab_code_pH_disturb<- source("external/tab_pH_disturb.R", local = TRUE)$value

## TP
tab_code_TP_site<- source("external/tab_TP_site.R", local = TRUE)$value
tab_code_TP_disturb<- source("external/tab_TP_disturb.R", local = TRUE)$value

# File Size ----
# By default, the file size limit is 5MB.
mb_limit <- 200
options(shiny.maxRequestSize = mb_limit * 1024^2)

# Import Data ----
df_AULandCover <-read.csv("Data/AU_LandCover.csv")
df_StationLocations <-read.csv("Data/StationLocations.csv")
df_StationsToAUs <-read.csv("Data/StationsToAUs.csv")
df_305BasinMerge2022 <-read.csv("Data/305BasinMerge2022.csv")

#when working:
# df_AULandCover <-read.csv("inst/shiny-examples/MassNBCtools/Data/AU_LandCover.csv")
# df_StationLocations <-read.csv("inst/shiny-examples/MassNBCtools/Data/StationLocations.csv")
# df_StationsToAUs <-read.csv("inst/shiny-examples/MassNBCtools/Data/StationsToAUs.csv")
# df_305BasinMerge2022 <-read.csv("inst/shiny-examples/MassNBCtools/Data/305BasinMerge2022.csv")

AU1 <- df_AULandCover %>%
  select(AU_ID) %>% 
  distinct() %>% 
  mutate(InAULC = "Yes")

AU2 <- df_StationsToAUs %>%
  select(AU_ID) %>% 
  distinct()%>% 
  mutate(InStatAU = "Yes")

AU3 <- df_305BasinMerge2022 %>%
  select(AU_ID) %>% 
  distinct()%>% 
  mutate(InGIS = "Yes")

df_all_AUs <- full_join(AU1, AU2, by = "AU_ID") %>% 
  full_join(., AU3, by = "AU_ID") %>% 
  top_n(10, AU_ID)

# GIS/Map data ----

# MassDEP Dams
GISlayer_dams <- rgdal::readOGR(file.path(".","GIS_Data", "DAMS_PT.shp"))

# MassDEP AU polygons
GISlayer_AUpoly <- rgdal::readOGR(file.path(".","GIS_Data"
                                            , "305BasinMerge2022.shp"))
# MassDEP AU flowlines
GISlayer_AUflow <- rgdal::readOGR(file.path(".","GIS_Data"
                                            , "2022arcs_mergeDRAFT.shp"))