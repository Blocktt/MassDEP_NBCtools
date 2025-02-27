# Total Phosphorus

function(){
  tabPanelBody("tabpan_TP",
           sidebarLayout(
             sidebarPanel(
               width = 4
               ,h2("Total Phosphorus Evaluation")
               ,p(strong("Chosen AU:"), textOutput("output_AU_choice4", inline = T)
                  , style = "font-size:20px;")
               ,h4(paste("Natural erosion of phosphorus-containing minerals"
                          , "within sedimentary rocks or soils contributes"
                          , "phosphorus to streams."))
               ,"Use the"
               ,tags$a(href="https://www.mass.gov/info-details/massgis-data-bedrock-lithology"
                        , "MassGIS Bedrock Lithology")
               ,"layer and work with the Massachusetts Geological Survey or USGS to"
               ,"identify AUs with potentially phosphorus-rich geology."
               ,selectInput(inputId = "input_geoTP_choice"
                            ,label = "Does a lack of phosphorus source lithology eliminate NBC determination?"
                            ,choices = c("","Yes","No")) # selectInput
             )# sidebarPanel~END
             , mainPanel(
               img(src = "bedlith.png", width = 700)
             )# mainPanel~END
           )# sidebarLayout~END
  ) # tabPanelBody~END
}# FUNCTION~END