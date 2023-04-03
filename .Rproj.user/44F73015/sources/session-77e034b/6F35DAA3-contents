# pH panel

function() {
  tabPanelBody("tabpan_pH"
           , sidebarLayout(
             sidebarPanel(
               width = 4
               ,h2("pH Evaluation")
               ,p(strong("Chosen AU:"), textOutput("output_AU_choice5", inline = T)
                  , style = "font-size:20px;")
               # Wetland land cover
               ,selectInput(inputId = "input_wetland2_choice"
                            ,label = "Is wetland land cover below NBC thresholds (7%)? "
                            ,choices = c("","Yes","No")) # selectInput
               , htmlOutput("output_wetland2")
             ) # sidebarPanel
             , mainPanel(
               h3("pH Criteria")
               ,img(src = "pH_Criteria.png", width = 800)
             ) # mainPanel
           ) # sidebarLayout
  )##tabPanelBody ~ END
}##FUNCTION ~ END