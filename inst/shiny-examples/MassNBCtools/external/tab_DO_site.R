# Dissolved oxygen site selection Panel

function() {
  tabPanel("tabpan_DO_site"
           , h2("Site selection")
           , p(paste0("Version: ", pkg_version))
           , fluidPage(h2("About")
                       , p("Background info")
                       
                       
           )## fluidPage ~ END
           # , includeHTML(file.path("www", "rmd_html", "ShinyHTML_About.html"))
  )##tabPanel ~ END
}##FUNCTION ~ END