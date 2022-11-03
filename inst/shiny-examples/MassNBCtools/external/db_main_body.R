
# Main

function(id) {
  
  tabItems(
    tabItem(tabName = "tab_about", tab_code_about())
    # Water Temperature
    , tabItem(tabName = "tab_temp_AU", tab_code_temp_AU())
    , tabItem(tabName = "tab_temp_crit", tab_code_temp_crit())
    , tabItem(tabName = "tab_temp_LC", tab_code_temp_LC())
    , tabItem(tabName = "tab_temp_pointsrc", tab_code_temp_pointsrc())
    , tabItem(tabName = "tab_temp_dams", tab_code_temp_dams())
    , tabItem(tabName = "tab_temp_withdrwl", tab_code_temp_withdrwl())
    , tabItem(tabName = "tab_temp_spike", tab_code_temp_spike())

    # Dissolved Oxygen
    , tabItem(tabName = "tab_DO_site", tab_code_DO_site())
    , tabItem(tabName = "tab_DO_disturb", tab_code_DO_disturb())
    
    # pH
    , tabItem(tabName = "tab_pH_site", tab_code_pH_site())
    , tabItem(tabName = "tab_pH_disturb", tab_code_pH_disturb())
    
    # Total Phosphorus
    , tabItem(tabName = "tab_TP_site", tab_code_TP_site())
    , tabItem(tabName = "tab_TP_disturb", tab_code_TP_disturb())
    
  )## tabItems
  
}## FUNCTION ~ END