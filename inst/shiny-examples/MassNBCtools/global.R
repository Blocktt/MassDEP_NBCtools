# Shiny Global File

# Version ----
pkg_version <- "v0.0.1.9400"

# Packages----
library(shiny)
library(shinydashboard)
library(shinydashboardPlus) # only using for footer
library(shinyjs) # used for download button enable
library(DT)
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
library(capture)
library(sf)

# Source ----
db_main_sb           <- source("external/db_main_sb.R", local = TRUE)$value
db_main_body         <- source("external/db_main_body.R", local = TRUE)$value
tab_code_about       <- source("external/tab_about.R", local = TRUE)$value
tab_code_FAQ       <- source("external/tab_FAQ.R", local = TRUE)$value
tab_code_contact       <- source("external/tab_contact.R", local = TRUE)$value

## General Evaluation
tab_code_Gen_input<- source("external/tab_Gen_input.R", local = TRUE)$value
tab_code_Gen_output<- source("external/tab_Gen_output.R", local = TRUE)$value

## Parameter specifics
tab_code_Gen_temp<- source("external/tab_Gen_temp.R", local = TRUE)$value
tab_code_Gen_DO<- source("external/tab_Gen_DO.R", local = TRUE)$value
tab_code_Gen_TP<- source("external/tab_Gen_TP.R", local = TRUE)$value
tab_code_Gen_pH<- source("external/tab_Gen_pH.R", local = TRUE)$value


# File Size ----
# By default, the file size limit is 5MB.
mb_limit <- 200
options(shiny.maxRequestSize = mb_limit * 1024^2)

# Import Data ----
df_305BasinMerge2022 <-read.csv("Data/305BasinMerge2022.csv")
df_AU_Data <- read.csv("Data/AU_Data.csv")

df_all_AUs <- df_305BasinMerge2022 %>%
  select(AU_ID) %>% 
  distinct()%>% 
  mutate(InGIS = "Yes")

# GIS/Map data ####
# AU polygons
load(file.path(".","GIS_Data", "GISlayer_AUpoly.rda"))

# AU polygon centroids
df_AUCentroids <- read.csv("Data/AU_Centroids.csv")

# AU flowlines
load(file.path(".","GIS_Data", "GISlayer_AUflow.rda"))

# Zone II polygons
load(file.path(".","GIS_Data", "GISlayer_Zone2.rda"))

# Dams
load(file.path(".","GIS_Data", "GISlayer_dams.rda"))

# Superfunds
load(file.path(".","GIS_Data", "GISlayer_SEMS.rda"))

# NPDES
load(file.path(".","GIS_Data", "GISlayer_NPDES.rda"))

# TRIs
load(file.path(".","GIS_Data", "GISlayer_TRIs.rda"))

# PWS
load(file.path(".","GIS_Data", "GISlayer_PWS_SW.rda"))

