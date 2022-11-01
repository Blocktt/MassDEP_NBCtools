
# Main

function(id) {
  
  tabItems(
    tabItem(tabName = "tab_about", tab_code_about())
    , tabItem(tabName = "tab_temp_site", tab_code_temp_site())
    , tabItem(tabName = "tab_temp_disturb", tab_code_temp_disturb())
    , tabItem(tabName = "tab_DO_site", tab_code_DO_site())
    , tabItem(tabName = "tab_DO_disturb", tab_code_DO_disturb())
    , tabItem(tabName = "tab_pH_site", tab_code_pH_site())
    , tabItem(tabName = "tab_pH_disturb", tab_code_pH_disturb())
    , tabItem(tabName = "tab_TP_site", tab_code_TP_site())
    , tabItem(tabName = "tab_TP_disturb", tab_code_TP_disturb())
  )## tabItems
  
}## FUNCTION ~ END