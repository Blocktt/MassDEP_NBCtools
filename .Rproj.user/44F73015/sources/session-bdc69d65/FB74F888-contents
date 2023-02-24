# pH panel

function() {
  tabPanel("tabpan_pH"
           , sidebarLayout(
             sidebarPanel(
               width = 3
               ,h2("pH Evaluation")
               ,p(strong("Chosen AU:"), textOutput("output_AU_choice5", inline = T)
                  , style = "font-size:20px;")
               # Wetland land cover
               ,selectInput(inputId = "input_wetland2_choice"
                            ,label = "Does insufficient wetland land cover eliminate natural conditions status (<7%)?"
                            ,choices = c("","Yes","No")) # selectInput
               , htmlOutput("output_wetland2")
             ) # sidebarPanel
             , mainPanel(
               h3("pH Criteria")
               ,img(src = "pH_Criteria.png", width = 800)
             ) # mainPanel
           ) # sidebarLayout
  )##tabPanel ~ END
}##FUNCTION ~ END