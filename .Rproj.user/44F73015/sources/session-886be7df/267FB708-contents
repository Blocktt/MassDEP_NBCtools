# Dissolved oxygen

function() {
  tabPanel("tabpan_DO"
           , sidebarLayout(
             sidebarPanel(
               width = 5
               ,h2("Dissolved Oxygen Evaluation")
               ,p(strong("Chosen AU:"), textOutput("output_AU_choice3", inline = T)
                  , style = "font-size:20px;")
               # DO spikes
               ,selectInput(inputId = "input_dospike_choice"
                            ,label = "Is the DO violation the result of isolated spike(s)?"
                            ,choices = c("","Yes","No","Unclear")) # selectInput
               , htmlOutput("output_dospike")
               
               # DO diurnal
               ,selectInput(inputId = "input_dodiurnal_choice"
                            ,label = "Is the diurnal shift in DO concentration ever greater than 3 mg/L?"
                            ,choices = c("","Yes","No","Unclear")) # selectInput
               , htmlOutput("output_dodiurnal")
               
               # Wetland land cover
               ,selectInput(inputId = "input_wetland1_choice"
                            ,label = "Is wetland land cover a potential source of DO violations (>7%)?"
                            ,choices = c("","Yes","No","Unclear")) # selectInput
               , htmlOutput("output_wetland1")
             ) # sidebarPanel
             , mainPanel(
               
             ) # mainPanel
           ) # sidebarLayout
  )##tabPanel ~ END
}##FUNCTION ~ END