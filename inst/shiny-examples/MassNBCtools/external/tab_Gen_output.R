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
               , h3("Analyst name:")
               , textOutput("output_analyst")
               , h3("Chosen AU:")
               , textOutput("output_AU_choice")
               , h3("Land cover issue:")
               , textOutput("output_Nat_Land_choice")
               , h3("Dam issue:")
               , textOutput("output_Dam_choice")
               , h3("Point source issue:")
               , textOutput("output_PtSrc_choice")
               , h3("Water withdrawal issue:")
               , textOutput("output_Withdrawal_choice")
               , h3("General Notes:")
               , textOutput("output_notes")
             ) # sidebarPanel
             , mainPanel(
               
             ) # mainPanel
           ) # sidebarLayout
  )##tabPanel ~ END
}##FUNCTION ~ END