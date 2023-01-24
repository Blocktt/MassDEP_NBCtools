# General Evaluation Input Panel

function() {
  tabPanel("tabpan_Gen_input"
           , h2("General Evaluation")
           , p(paste0("Version: ", pkg_version))
           , fluidPage(h2("Gen Eval")
                       , p("Input stuff goes here")
                       
                       
           )## fluidPage ~ END
  )##tabPanel ~ END
}##FUNCTION ~ END