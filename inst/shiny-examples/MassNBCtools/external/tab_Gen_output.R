# General Evaluation Input Panel

function() {
  tabPanel("tabpan_Gen_output"
           , h2("General Evaluation")
           , p(paste0("Version: ", pkg_version))
           , fluidPage(h2("Gen Eval")
                       , p("Here is where the output goes")
           )## fluidPage ~ END
  )##tabPanel ~ END
}##FUNCTION ~ END