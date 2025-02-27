# Dissolved oxygen

function() {
  tabPanelBody("tabpan_DO"
           , sidebarLayout(
             sidebarPanel(
               width = 5
               ,h2("Dissolved Oxygen Evaluation")
               ,p(strong("Chosen AU:"), textOutput("output_AU_choice3", inline = T)
                  , style = "font-size:20px;")
               # DO spikes
               ,p(paste("Non-statistical, continuous data files (that would"
                        , "show any isolated spikes) are currently available"
                        , "via the WPP internal data warehouse holdings (by OWMID)"
                        , "and via our external data holdings (see Bob Smith)."))
               ,selectInput(inputId = "input_dospike_choice"
                            ,label = "Is the DO exceedance the result of isolated spike(s)?"
                            ,choices = c("","Yes","No")) # selectInput
               
               
               # DO diurnal
               ,selectInput(inputId = "input_dodiurnal_choice"
                            ,label = "Is the diurnal shift in DO concentration ever greater than 3 mg/L?"
                            ,choices = c("","Yes","No")) # selectInput
               
               
               # Wetland land cover
               ,selectInput(inputId = "input_wetland1_choice"
                            ,label = "Is wetland land cover below NBC thresholds (7%)?"
                            ,choices = c("","Yes","No")) # selectInput
               , htmlOutput("output_wetland1")
               
             ) # sidebarPanel
             , mainPanel(
               
             ) # mainPanel
           ) # sidebarLayout
  )##tabPanelBody ~ END
}##FUNCTION ~ END