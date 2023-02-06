# General Evaluation Output Panel

function() {
  tabPanel("tabpan_Gen_output"
           , sidebarLayout(
             sidebarPanel(
               h2("Output information")
               # ,p("Analyst name:", textOutput(outputId = "output_analyst"))
               # ,p("Chosen AU:", textOutput(outputId = "output_AU_choice"))
               # ,p("Land cover issue:", textOutput(outputId = "output_Nat_Land_choice"))
               # ,p("Dam issue:", textOutput(outputId = "output_Dam_choice"))
               # ,p("Point source issue:", textOutput(outputId = "output_PtSrc_choice"))
               # ,p("Water withdrawal issue:", textOutput(outputId = "output_Withdrawal_choice"))
               # ,p("General Notes:", textOutput(outputId = "output_notes"))
               # 
               # , h4("Analyst name:"), textOutput("output_analyst", inline = T)
               
               , p(strong("Analyst name:"), textOutput("output_analyst", inline = T)
                   , style = "font-size:20px;")
               , p(strong("Chosen AU:"), textOutput("output_AU_choice", inline = T)
                   , style = "font-size:20px;")
               , p(strong("Land cover issue:"), textOutput("output_Nat_Land_choice", inline = T)
                   , style = "font-size:20px;")
               , htmlOutput("output_LC_Results2")
               , br()
               , p(strong("Dam issue:"), textOutput("output_Dam_choice", inline = T)
                   , style = "font-size:20px;")
               , htmlOutput("output_dam_count2")
               , br()
               , p(strong("Point source issue:"), textOutput("output_PtSrc_choice", inline = T)
                   , style = "font-size:20px;")
               , htmlOutput("output_ptsrc_counts2")
               , br()
               , p(strong("Water withdrawal issue:"), textOutput("output_Withdrawal_choice", inline = T)
                   , style = "font-size:20px;")
               , htmlOutput("output_pctZone2_2")
               , br()
               , p(strong("General Notes:"), style = "font-size:20px;")
               , textOutput("output_notes")
             ) # sidebarPanel
             , mainPanel(
               
             ) # mainPanel
           ) # sidebarLayout
  )##tabPanel ~ END
}##FUNCTION ~ END