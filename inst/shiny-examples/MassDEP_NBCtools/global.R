# Shiny Global File

# Version ----
pkg_version <- "0.0.0.9000"

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

# Source ----
db_main_sb           <- source("external/db_main_sb.R", local = TRUE)$value
db_main_body         <- source("external/db_main_body.R", local = TRUE)$value
tab_code_about       <- source("external/tab_about.R", local = TRUE)$value
tab_code_temp_site<- source("external/tab_temp_site.R", local = TRUE)$value
tab_code_temp_disturb<- source("external/tab_temp_disturb.R", local = TRUE)$value
tab_code_DO_site<- source("external/tab_DO_site.R", local = TRUE)$value
tab_code_DO_disturb<- source("external/tab_DO_disturb.R", local = TRUE)$value
tab_code_pH_site<- source("external/tab_pH_site.R", local = TRUE)$value
tab_code_pH_disturb<- source("external/tab_pH_disturb.R", local = TRUE)$value
tab_code_TP_site<- source("external/tab_TP_site.R", local = TRUE)$value
tab_code_TP_disturb<- source("external/tab_TP_disturb.R", local = TRUE)$value

# File Size ----
# By default, the file size limit is 5MB.
mb_limit <- 200
options(shiny.maxRequestSize = mb_limit * 1024^2)
