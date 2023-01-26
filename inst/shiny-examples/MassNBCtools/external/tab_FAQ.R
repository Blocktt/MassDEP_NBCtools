# FAQ Panel

function() {
  tabPanel("tabpan_FAQ"
           , h2("FAQ")
           , p(paste0("Version: ", pkg_version))
           , fluidPage(h2("Frequently Asked Questions")
           )## fluidPage ~ END
  )##tabPanel ~ END
}##FUNCTION ~ END