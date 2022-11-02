#Sidebar----
#sb_main <- function(id) {
function(id) {
  dashboardSidebar(
    width = 275
    , HTML("&nbsp;&nbsp;<font size=5><b>Steps</b></font>")
    , sidebarMenu(id = id
                  , menuItem(text = "About"
                             , tabName = "tab_about"
                             , icon = icon("cog", lib = "glyphicon")
                  )## menuItem ~ About ~ END
                  , menuItem(text = "Water Temperature"
                             , icon = icon("cog", lib = "glyphicon")
                             , tabName = "tab_watertemp"
                             , menuSubItem("Step 1: Site selection"
                                           , tabName = "tab_temp_site"
                                           , icon = icon("cog", lib = "glyphicon"))
                             , menuSubItem("Step 2:"
                                           , tabName = "tab_temp_disturb"
                                           , icon = icon("cog", lib = "glyphicon"))
                  )## menuItem ~ Temperature
                  , menuItem(text = "Dissolved Oxygen"
                             , icon = icon("cog", lib = "glyphicon")
                             , tabName = "tab_DO"
                             , menuSubItem("Step 1: Site selection"
                                           , tabName = "tab_DO_site"
                                           , icon = icon("cog", lib = "glyphicon"))
                             , menuSubItem("Step 2:"
                                           , tabName = "tab_DO_disturb"
                                           , icon = icon("cog", lib = "glyphicon"))
                  )## menuItem ~ DO
                  , menuItem(text = "pH"
                             , icon = icon("cog", lib = "glyphicon")
                             , tabName = "tab_pH"
                             , menuSubItem("Step 1: Site selection"
                                           , tabName = "tab_pH_site"
                                           , icon = icon("cog", lib = "glyphicon"))
                             , menuSubItem("Step 2:"
                                           , tabName = "tab_pH_disturb"
                                           , icon = icon("cog", lib = "glyphicon"))
                  )## menuItem ~ pH
                  , menuItem(text = "Total Phosphorus"
                             , icon = icon("cog", lib = "glyphicon")
                             , tabName = "tab_pH"
                             , menuSubItem("Step 1: Site selection"
                                           , tabName = "tab_TP_site"
                                           , icon = icon("cog", lib = "glyphicon"))
                             , menuSubItem("Step 2:"
                                           , tabName = "tab_TP_disturb"
                                           , icon = icon("cog", lib = "glyphicon"))
                  )## menuItem ~ TP
    )## sidebarMenu ~ END
  )## dashboardSidebar ~ END
}## FUNCTION ~ END