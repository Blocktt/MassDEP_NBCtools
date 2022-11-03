# Water temperature select AU Panel

function() {
  tabPanel("tabpan_temp_AU"
           , h2("Select AU")
           , p(paste0("Version: ", pkg_version))
           , fluidPage(h2("About")
                       , p("Background info")
                       
                       
           )## fluidPage ~ END
           # , includeHTML(file.path("www", "rmd_html", "ShinyHTML_About.html"))
  )##tabPanel ~ END
}##FUNCTION ~ END