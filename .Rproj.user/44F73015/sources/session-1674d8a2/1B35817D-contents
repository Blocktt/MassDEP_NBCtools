# Water temperature select AU Panel

function() {
  tabPanel("tabpan_temp_AU"
           , sidebarLayout(
             sidebarPanel(
               textInput(inputId = "temp_analyst"
                          ,label = "Analyst Name:"
                          ,placeholder = "Type your name here!")
               ,selectInput(inputId = "temp_AU_choice"
                            ,label = "Assessment Unit (AU):"
                            ,choices = c("",unique(df_all_AUs$AU_ID))) # selectInput
               ,textInput(inputId = "temp_AU_notes"
                          ,label = "Analyst Notes:"
                          ,placeholder = "Type any additional notes here!")
             ) # sidebarPanel
             , mainPanel(
               DT::dataTableOutput("temp_test_DT")
               , leafletOutput("mymap")
             ) # mainPanel
           ) # sidebarLayout
  )##tabPanel ~ END
}##FUNCTION ~ END