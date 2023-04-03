# Water temperature

function() {
  tabPanelBody("tabpan_temp"
           , sidebarLayout(
             sidebarPanel(
               width = 5
               ,h2("Water Temperature Evaluation")
               ,p(strong("Chosen AU:"), textOutput("output_AU_choice2", inline = T)
                   , style = "font-size:20px;")
               ,p(strong("AU Temperature Class Qualifier: "), htmlOutput("output_TempClass", inline = T)
                  , style = "font-size:20px;")
               ,p(paste("Non-statistical, continuous data files (that would"
                        , "show any isolated spikes) are currently available"
                        , "via the WPP internal data warehouse holdings (by OWMID)"
                        , "and via our external data holdings (see Bob Smith)."))
               # Temperature criteria
               ,selectInput(inputId = "input_tempcrit_choice"
                            ,label = paste0("Which temperature criteria or thresholds were exceeded,"
                                            , " the warm water (28.3C), cold water (20.0C), or existing"
                                            ," cold water use (20.0C or 21.0C)?")
                            ,choices = c("","Warm-Water","Cold-Water", "Existing Cold-Water")) # selectInput
               
               # Temperature spikes
               ,selectInput(inputId = "input_tempspike_choice"
                            ,label = "Is the temperature exceedance the result of isolated spike(s)?"
                            ,choices = c("","Yes","No")) # selectInput
               
               # Impervious land cover
               ,selectInput(inputId = "input_imperv_choice"
                            ,label = "Does impervious land cover, greater than 4%, substantially influence the temperature exceedance and likely alter NBC?"
                            ,choices = c("","Yes","No")) # selectInput
               , htmlOutput("output_Imperv1")
               
             ) # sidebarPanel
             , mainPanel(
               
             ) # mainPanel
           ) # sidebarLayout
  )##tabPanelBody ~ END
}##FUNCTION ~ END