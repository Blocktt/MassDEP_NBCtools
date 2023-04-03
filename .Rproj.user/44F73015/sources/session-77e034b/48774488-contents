# Total Phosphorus

function(){
  tabPanelBody("tabpan_metal",
           sidebarLayout(
             sidebarPanel(
               width = 4
               ,h2("Metals Evaluation")
               ,p(strong("Chosen AU:"), textOutput("output_AU_choice6", inline = T)
                  , style = "font-size:20px;")
               ,h4(paste("Natural erosion of metal-containing minerals"
                          , "within sedimentary rocks or soils contributes"
                          , "metals to streams."))
               ,"Use the"
               ,tags$a(href="https://www.mass.gov/info-details/massgis-data-bedrock-lithology"
                        , "MassGIS Bedrock Lithology")
               ,"layer and work with the Massachusetts Geological Survey or USGS to"
               ,"identify AUs with potentially metal-rich geology."
               ,selectInput(inputId = "input_geoMetal_choice"
                            ,label = "Does a lack of metal source lithology eliminate NBC determination?"
                            ,choices = c("","Yes","No")) # selectInput
             )# sidebarPanel~END
             , mainPanel(
               img(src = "bedlith.png", width = 700)
             )# mainPanel~END
           )# sidebarLayout~END
  ) # tabPanelBody~END
}# FUNCTION~END