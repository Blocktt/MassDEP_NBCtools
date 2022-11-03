#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic
shinyServer(function(input, output) {
  
  myData <- reactive({
    df_StationsToAUs %>%
      select(AU_ID, UniqueID) %>%
      filter(AU_ID == input$temp_AU_choice) %>% 
      filter(complete.cases(.))
  })#reactive ~ END
  
    output$temp_test_DT <- renderDT({
      
      return(myData())
    })#renderDT ~ END
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>% 
        addProviderTiles(providers$Esri.WorldStreetMap, group="Esri WSM") %>%
        addProviderTiles("CartoDB.Positron", group="Positron") %>%
        addProviderTiles(providers$Stamen.TonerLite, group="Toner Lite") %>% 
        addCircleMarkers(data = GISlayer_dams, color = "red", radius = 5
                         , label = GISlayer_dams$DAMNAME, group = "Dams"
                         , popup = paste("<b> Dam Name:</b>", GISlayer_dams$DAMNAME, "<br>"
                                         ,"<b> NATID:</b>", GISlayer_dams$NATID, "<br>"
                                         ,"<b> REGAUTH:</b>", GISlayer_dams$REGAUTH, "<br>"
                                         ,"<b> OWNTYPE1:</b>", GISlayer_dams$OWNTYPE1, "<br>"
                                         ,"<b> OWNTYPE2:</b>", GISlayer_dams$OWNTYPE2, "<br>"
                                         ,"<b> OWNTYPE3:</b>", GISlayer_dams$OWNTYPE3, "<br>"
                                         ,"<b> HAZCODE:</b>", GISlayer_dams$HAZCODE)) %>%
        addPolygons(data = GISlayer_AUpoly, color = "green", weight = 2
                    , fill = FALSE, label = GISlayer_AUpoly$AU_ID, group = "AU_Polygons"
                    , popup = paste("<b> AU_ID:</b>", GISlayer_AUpoly$AU_ID, "<br>"
                                    ,"<b> Area_SQ_MI:</b>", GISlayer_AUpoly$Area_SQ_MI)) %>%
        addPolylines(data = GISlayer_AUflow, color = "blue", weight = 3
                     , label = GISlayer_AUflow$AU_ID, group = "AU_Polylines"
                     , popup = paste("<b> AU_ID:</b>", GISlayer_AUflow$AU_ID, "<br>"
                                     ,"<b> AU_Name:</b>", GISlayer_AUflow$AU_Name, "<br>"
                                     ,"<b> AU_DESC1:</b>", GISlayer_AUflow$AU_DESC1, "<br>"
                                     ,"<b> AU_DESC2:</b>", GISlayer_AUflow$AU_DESC2, "<br>"
                                     ,"<b> AU_TYPE:</b>", GISlayer_AUflow$AU_TYPE, "<br>"
                                     ,"<b> AU_Size:</b>", GISlayer_AUflow$AU_Size, "<br>"
                                     ,"<b> AU_Unit:</b>", GISlayer_AUflow$AU_Unit, "<br>"
                                     ,"<b> AU_Class:</b>", GISlayer_AUflow$AU_Class, "<br>"
                                     ,"<b> AU_ClassQu:</b>", GISlayer_AUflow$AU_ClassQu, "<br>"
                                     ,"<b> AU_ClassRe:</b>", GISlayer_AUflow$AU_ClassRe)) %>% 
        addLayersControl(overlayGroups = c("Dams", "AU_Polygons", "AU_Polylines")
                         ,baseGroups = c("Esri WSM", "Positron", "Toner Lite")
                         ,options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup(c("Dams", "AU_Polygons", "AU_Polylines")) %>%
        addMiniMap(toggleDisplay = TRUE, tiles = providers$Esri.WorldStreetMap)
    })#renderLeaflet ~ END
    
})##shinyServer ~ END