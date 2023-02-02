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
    
  # Mapping ####
  ## Base Map ####
  output$mymap <- renderLeaflet({
    leaflet("mymap") %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>% 
      setView(lat = 42.3063, lng = -71.8046, zoom = 8) %>%
      addPolygons(data = GISlayer_AUpoly, color = "black", weight = 2, opacity = 1
                  , fill = FALSE, label = GISlayer_AUpoly$AU_ID, group = "AU Watersheds"
                  , popup = paste("<b> AU_ID:</b>", GISlayer_AUpoly$AU_ID, "<br>"
                                  ,"<b> Area_SQ_MI:</b>", GISlayer_AUpoly$Area_SQ_MI)) %>%
      addCircleMarkers(data = GISlayer_dams, radius = 7, stroke = TRUE
                       , fillOpacity = 1, fillColor = "red", color = "black"
                       , opacity = 1, weight = 2
                       , label = GISlayer_dams$DAMNAME, group = "Dams"
                       , popup = paste("<b> Dam Name:</b>", GISlayer_dams$DAMNAME, "<br>"
                                       ,"<b> NATID:</b>", GISlayer_dams$NATID, "<br>"
                                       ,"<b> REGAUTH:</b>", GISlayer_dams$REGAUTH, "<br>"
                                       ,"<b> OWNTYPE1:</b>", GISlayer_dams$OWNTYPE1, "<br>"
                                       ,"<b> OWNTYPE2:</b>", GISlayer_dams$OWNTYPE2, "<br>"
                                       ,"<b> OWNTYPE3:</b>", GISlayer_dams$OWNTYPE3, "<br>"
                                       ,"<b> HAZCODE:</b>", GISlayer_dams$HAZCODE)) %>%
      addCircleMarkers(data = GISlayer_SEMS, radius = 7, stroke = TRUE
                       , fillOpacity = 1, fillColor = "#beaed4", color = "black"
                       , opacity = 1, weight = 2
                       , label = GISlayer_SEMS$PRIMARY_NA, group = "Superfunds"
                       , popup = paste("<b> Primary Name:</b>", GISlayer_SEMS$PRIMARY_NA, "<br>"
                                       ,"<b> Registry ID:</b>", GISlayer_SEMS$REGISTRY_I, "<br>"
                                       ,"<b> Latitude:</b>", GISlayer_SEMS$LATITUDE83, "<br>"
                                       ,"<b> Longitude:</b>", GISlayer_SEMS$LONGITUDE8)) %>%
      addCircleMarkers(data = GISlayer_NPDES, radius = 7, stroke = TRUE
                       , fillOpacity = 1, fillColor = "orange", color = "black"
                       , opacity = 1, weight = 2
                       , label = GISlayer_NPDES$PRIMARY_NA, group = "NPDES"
                       , popup = paste("<b> Primary Name:</b>", GISlayer_NPDES$PRIMARY_NA, "<br>"
                                       ,"<b> Registry ID:</b>", GISlayer_NPDES$REGISTRY_I, "<br>"
                                       ,"<b> Latitude:</b>", GISlayer_NPDES$LATITUDE83, "<br>"
                                       ,"<b> Longitude:</b>", GISlayer_NPDES$LONGITUDE8)) %>%
      addCircleMarkers(data = GISlayer_TRIs, radius = 7, stroke = TRUE
                       , fillOpacity = 1, fillColor = "#7fc97f", color = "black"
                       , opacity = 1, weight = 2
                       , label = GISlayer_TRIs$PRIMARY_NA, group = "TRIs"
                       , popup = paste("<b> Primary Name:</b>", GISlayer_TRIs$PRIMARY_NA, "<br>"
                                       ,"<b> Registry ID:</b>", GISlayer_TRIs$REGISTRY_I, "<br>"
                                       ,"<b> Latitude:</b>", GISlayer_TRIs$LATITUDE83, "<br>"
                                       ,"<b> Longitude:</b>", GISlayer_TRIs$LONGITUDE8)) %>%
      addPolylines(data = GISlayer_AUflow, color = "blue", weight = 3
                   , label = GISlayer_AUflow$AU_ID, group = "AU Polylines"
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
      addPolygons(data = GISlayer_Zone2, color = "black", weight = 2, opacity = 1
                  , fillColor = "#df65b0", fillOpacity = 0.5, group = "Zone 2 WPA") %>%  
      addLayersControl(overlayGroups = c("AU Polylines", "AU Watersheds", "Dams"
                                         , "NPDES", "Superfunds", "TRIs"
                                         , "Zone 2 WPA")
                       ,baseGroups = c("Esri WSM")
                       ,options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(c("AU Polylines", "AU Watersheds", "Dams","NPDES", "Superfunds"
                  , "TRIs", "Zone 2 WPA")) %>%
      addMiniMap(toggleDisplay = TRUE, tiles = providers$Esri.WorldStreetMap
                 , position = "bottomleft")
  })#renderLeaflet ~ END

    ## Map Zoom ####
    # Map that filters output data to a single AU
    observeEvent(input$input_AU_choice, {
      req(input$input_AU_choice != "")
      
      myAU <- input$input_AU_choice
      
      df_AUCentroids_select <- df_AUCentroids %>% 
        filter(AU_ID == myAU)
      AU_long <- df_AUCentroids_select$Longitude # longitude
      AU_lat <- df_AUCentroids_select$Latitude # latitude

      GISlayer_AUpoly_select <- GISlayer_AUpoly[GISlayer_AUpoly$AU_ID == myAU,]
      
      # modfiy map
      leafletProxy("mymap") %>%
        removeShape("layer_AU_selected")  %>%
        addPolygons(data = GISlayer_AUpoly_select, color = "black", weight = 2
                    , fillColor = "red", label = GISlayer_AUpoly_select$AU_ID
                    , group = "AU_selected", layerId = "layer_AU_selected") %>%
        setView(lng = AU_long, lat = AU_lat, zoom = 12)
    })#observeEvent ~ END


    # Output info ####
    output$output_analyst <- renderText({input$input_analyst})
    output$output_AU_choice <- renderText({input$input_AU_choice})
    output$output_Nat_Land_choice <- renderText({input$input_Nat_Land_choice})
    output$output_Dam_choice <- renderText({input$input_Dam_choice})
    output$output_PtSrc_choice <- renderText({input$input_PtSrc_choice})
    output$output_Withdrawal_choice <- renderText({input$input_Withdrawal_choice})
    output$output_notes <- renderText({input$input_notes})

})##shinyServer ~ END