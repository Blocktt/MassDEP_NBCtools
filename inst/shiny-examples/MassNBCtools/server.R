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
  
  # select AUs ####
  myData <- reactive({
    df_AU_Data %>%
      select(AU_ID, Area_SQ_MI, PERCAG, PERCDEV
             , PERCNAT, PERCWET, PERCAG_PR, PERCDEV_PR, PERCNAT_PR, PERCWET_PR
             , PERCAG_ST, PERCDEV_ST, PERCNAT_ST, PERCWET_ST, PERCAG_SP
             , PERCDEV_SP, PERCNAT_SP, PERCWET_SP, PercImp, nDams, nGWP
             , nNPDES, nPWS_SW, nSEMS, nTRIs)
      # select(AU_ID, Area_SQ_MI, CLASS, QUALIFIER, CATEGORY)
      # filter(AU_ID == input$input_AU_choice) %>% 
      # filter(complete.cases(.))
  })#reactive ~ END
  
  observeEvent(input$input_AU_choice, {
    if(input$input_AU_choice == ""){
      output$input_DT <- renderDT({
        return(myData())
      })#renderDT ~ END
    } else {
      output$input_DT <- renderDT({
        trim_data <- myData()
        trim_data2 <- trim_data %>% 
          filter(AU_ID == input$input_AU_choice)
        return(trim_data2)
      })#renderDT ~ END
    } # end 
  })#observeEvent ~END
  
    # output$input_DT <- renderDT({
    #   myData()
    # })#renderDT ~ END
    
    # Mapping ####
    ## Base Map ####
    output$mymap <- renderLeaflet({
      leaflet("mymap") %>%
        addTiles() %>% 
        addProviderTiles(providers$Esri.WorldStreetMap) %>% 
        setView(lat = 42.3063, lng = -71.8046, zoom = 8) %>%
        addLayersControl(overlayGroups = c("AU_Polygons", "Dams")
                         ,baseGroups = c("Esri WSM")
                         ,options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("AU_Polygons", "Dams")) %>%
        addMiniMap(toggleDisplay = TRUE, tiles = providers$Esri.WorldStreetMap)
    })#renderLeaflet ~ END
    
    ## Load layers ####
    output$polyAU_output <- renderPlot({
      leafletProxy('mymap') %>% # initalize the map
        addPolygons(data = GISlayer_AUpoly, color = "green", weight = 2
                    , fill = FALSE, label = GISlayer_AUpoly$AU_ID, group = "AU_Polygons"
                    , popup = paste("<b> AU_ID:</b>", GISlayer_AUpoly$AU_ID, "<br>"
                                    ,"<b> Area_SQ_MI:</b>", GISlayer_AUpoly$Area_SQ_MI))
    }) #renderPlot ~ END
    
    output$dam_output <- renderPlot({
      leafletProxy('mymap') %>% # initalize the map
        addCircleMarkers(data = GISlayer_dams, color = "red", radius = 5
                         , label = GISlayer_dams$DAMNAME, group = "Dams"
                         , popup = paste("<b> Dam Name:</b>", GISlayer_dams$DAMNAME, "<br>"
                                         ,"<b> NATID:</b>", GISlayer_dams$NATID, "<br>"
                                         ,"<b> REGAUTH:</b>", GISlayer_dams$REGAUTH, "<br>"
                                         ,"<b> OWNTYPE1:</b>", GISlayer_dams$OWNTYPE1, "<br>"
                                         ,"<b> OWNTYPE2:</b>", GISlayer_dams$OWNTYPE2, "<br>"
                                         ,"<b> OWNTYPE3:</b>", GISlayer_dams$OWNTYPE3, "<br>"
                                         ,"<b> HAZCODE:</b>", GISlayer_dams$HAZCODE))
    }) #renderPlot ~ END
    
    ## Map Zoom ####
    # Map that filters output data to a single AU
    observeEvent(input$input_AU_choice, {
      req(input$input_AU_choice != "")
      
      myAU <- input$input_AU_choice

      GISlayer_AUpoly_select <- GISlayer_AUpoly[GISlayer_AUpoly$AU_ID == myAU,]
      
      # https://r-spatial.github.io/sf/articles/sf1.html#crs
      # https://spatialreference.org/ref/?search=nad+83+massachusetts&srtext=Search
      AU_transform <- sf::st_transform(GISlayer_AUpoly_select, 2249)
      AU_Centroid <- suppressWarnings(sf::st_centroid(AU_transform))
      backtransform <- sf::st_transform(AU_Centroid, 4326)
      AU_geom <- backtransform$geometry[[1]]
      AU_long <- AU_geom[1] # longitude
      AU_lat <- AU_geom[2] # latitude
      
      # modfiy map
      leafletProxy("mymap") %>%
        removeShape("layer_AU_selected")  %>%
        addPolygons(data = GISlayer_AUpoly_select, color = "black", weight = 2
                    , fillColor = "red", label = GISlayer_AUpoly_select$AU_ID
                    , group = "AU_selected", layerId = "layer_AU_selected") %>%
        setView(lng = AU_long, lat = AU_lat, zoom = 12)
    })#observeEvent ~ END

    # output$mymap <- renderLeaflet({
    #   leaflet() %>%
    #     addTiles() %>%
    #     addProviderTiles(providers$Esri.WorldStreetMap, group="Esri WSM") %>%
    #     addProviderTiles("CartoDB.Positron", group="Positron") %>%
    #     addProviderTiles(providers$Stamen.TonerLite, group="Toner Lite") %>%
    #     addCircleMarkers(data = GISlayer_dams, color = "red", radius = 5
    #                      , label = GISlayer_dams$DAMNAME, group = "Dams"
    #                      , popup = paste("<b> Dam Name:</b>", GISlayer_dams$DAMNAME, "<br>"
    #                                      ,"<b> NATID:</b>", GISlayer_dams$NATID, "<br>"
    #                                      ,"<b> REGAUTH:</b>", GISlayer_dams$REGAUTH, "<br>"
    #                                      ,"<b> OWNTYPE1:</b>", GISlayer_dams$OWNTYPE1, "<br>"
    #                                      ,"<b> OWNTYPE2:</b>", GISlayer_dams$OWNTYPE2, "<br>"
    #                                      ,"<b> OWNTYPE3:</b>", GISlayer_dams$OWNTYPE3, "<br>"
    #                                      ,"<b> HAZCODE:</b>", GISlayer_dams$HAZCODE)) %>%
    #     addPolygons(data = GISlayer_AUpoly, color = "green", weight = 2
    #                 , fill = FALSE, label = GISlayer_AUpoly$AU_ID, group = "AU_Polygons"
    #                 , popup = paste("<b> AU_ID:</b>", GISlayer_AUpoly$AU_ID, "<br>"
    #                                 ,"<b> Area_SQ_MI:</b>", GISlayer_AUpoly$Area_SQ_MI)) %>%
    #     addPolylines(data = GISlayer_AUflow, color = "blue", weight = 3
    #                  , label = GISlayer_AUflow$AU_ID, group = "AU_Polylines"
    #                  , popup = paste("<b> AU_ID:</b>", GISlayer_AUflow$AU_ID, "<br>"
    #                                  ,"<b> AU_Name:</b>", GISlayer_AUflow$AU_Name, "<br>"
    #                                  ,"<b> AU_DESC1:</b>", GISlayer_AUflow$AU_DESC1, "<br>"
    #                                  ,"<b> AU_DESC2:</b>", GISlayer_AUflow$AU_DESC2, "<br>"
    #                                  ,"<b> AU_TYPE:</b>", GISlayer_AUflow$AU_TYPE, "<br>"
    #                                  ,"<b> AU_Size:</b>", GISlayer_AUflow$AU_Size, "<br>"
    #                                  ,"<b> AU_Unit:</b>", GISlayer_AUflow$AU_Unit, "<br>"
    #                                  ,"<b> AU_Class:</b>", GISlayer_AUflow$AU_Class, "<br>"
    #                                  ,"<b> AU_ClassQu:</b>", GISlayer_AUflow$AU_ClassQu, "<br>"
    #                                  ,"<b> AU_ClassRe:</b>", GISlayer_AUflow$AU_ClassRe)) %>%
    #     addLayersControl(overlayGroups = c("Dams", "AU_Polygons", "AU_Polylines")
    #                      ,baseGroups = c("Esri WSM", "Positron", "Toner Lite")
    #                      ,options = layersControlOptions(collapsed = FALSE)) %>%
    #     hideGroup(c("Dams", "AU_Polygons", "AU_Polylines")) %>%
    #     addMiniMap(toggleDisplay = TRUE, tiles = providers$Esri.WorldStreetMap)
    # })#renderLeaflet ~ END
    
})##shinyServer ~ END