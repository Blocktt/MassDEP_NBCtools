# General Evaluation Input Panel

function() {
  tabPanel("tabpan_Gen_input"
           , sidebarLayout(
             sidebarPanel(
               # Analyst Name
               textInput(inputId = "input_analyst"
                         ,label = "Analyst Name:"
                         ,placeholder = "Type your name here!")
               # Choose AU
               ,selectInput(inputId = "input_AU_choice"
                            ,label = "Assessment Unit (AU):"
                            ,choices = c("", unique(df_all_AUs$AU_ID))) # selectInput
               # Q: Land cover
               ,selectInput(inputId = "input_Nat_Land_choice"
                            ,label = "Is the natural land cover high and imperviousness low?"
                            ,choices = c("","Yes","No","Unclear")) # selectInput
               # Q: Dams
               ,selectInput(inputId = "input_Dam_choice"
                            ,label = "Are dams a potential source of impairment?"
                            ,choices = c("","Yes","No","Unclear")) # selectInput
               # Q: Point sources
               ,selectInput(inputId = "input_PtSrc_choice"
                            ,label = "Are point source discharges a potential source of impairment?"
                            ,choices = c("","Yes","No","Unclear")) # selectInput
               # Q: Water Withdrawal
               ,selectInput(inputId = "input_Withdrawal_choice"
                            ,label = "Are water withdrawals a potential source of impairment?"
                            ,choices = c("","Yes","No","Unclear")) # selectInput
               # Notes
               ,textInput(inputId = "input_notes"
                          ,label = "General Notes:"
                          ,placeholder = "Type any additional notes here!")
               
             ) # sidebarPanel
             , mainPanel(
               DT::dataTableOutput("input_DT")
               , leafletOutput("mymap")
               , plotOutput('polyAU_output')
               , plotOutput('dam_output')
             ) # mainPanel
           ) # sidebarLayout
  )##tabPanel ~ END
}##FUNCTION ~ END