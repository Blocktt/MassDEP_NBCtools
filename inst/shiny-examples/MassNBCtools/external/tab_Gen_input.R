# General Evaluation Input Panel

function() {
  tabPanel("tabpan_Gen_input"
           , sidebarLayout(
             sidebarPanel(
               width = 3
               # Analyst Name
               ,textInput(inputId = "input_analyst"
                         ,label = "Analyst Name:"
                         ,placeholder = "Type your name here!")
               # Choose AU
               # ,selectInput(inputId = "input_AU_choice"
               #              ,label = "Assessment Unit (AU):"
               #              ,choices = c("", unique(df_all_AUs$AU_ID))) # selectInput
               ,selectizeInput(inputId = "input_AU_choice"
                               , label = "Assessment Unit (AU):"
                               , choices = NULL)
               # Q: Land cover
               ,selectInput(inputId = "input_Nat_Land_choice"
                            ,label = "Does land cover indicate natural conditions?"
                            ,choices = c("","Yes","No","Unclear")) # selectInput
               , htmlOutput("output_LC_Results1")
               , br()
               # Q: Dams
               ,selectInput(inputId = "input_Dam_choice"
                            ,label = "Are dams a potential source of impairment?"
                            ,choices = c("","Yes","No","Unclear")) # selectInput
               , htmlOutput("output_dam_count1")
               , br()
               # Q: Point sources
               ,selectInput(inputId = "input_PtSrc_choice"
                            ,label = "Are point source discharges a potential source of impairment?"
                            ,choices = c("","Yes","No","Unclear")) # selectInput
               , htmlOutput("output_ptsrc_counts1")
               , br()
               # Q: Water Withdrawal
               ,selectInput(inputId = "input_Withdrawal_choice"
                            ,label = "Are water withdrawals a potential source of impairment?"
                            ,choices = c("","Yes","No","Unclear")) # selectInput
               , htmlOutput("output_pctZone2_1")
               , br()
               # Notes
               ,textInput(inputId = "input_notes"
                          ,label = "General Notes:"
                          ,placeholder = "Type any additional notes here!")
               #https://github.com/dreamRs/capture
               ,capture::capture(selector = "body"
                                 , filename = "all-page.png"
                                 , icon("camera-retro")
                                 , "Take screenshot of entire page"
                                 , class = "btn-primary")
               , br()
               ,capture::capture(selector = "#mymap"
                                 , filename = "mymap.png"
                                 , icon("camera-retro")
                                 , "Take screenshot of map only"
                                 , class = "btn-info")
               ,img(src = "Map_Legend.png", height = 200)
               
             ) # sidebarPanel
             , mainPanel(
               width = 9
               , DT::dataTableOutput("input_DT")
               , leafletOutput("mymap", height = 600)
             ) # mainPanel
           ) # sidebarLayout
  )##tabPanel ~ END
}##FUNCTION ~ END