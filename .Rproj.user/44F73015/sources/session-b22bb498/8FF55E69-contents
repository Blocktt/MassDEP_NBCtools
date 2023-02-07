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
  
  # modal dialog ####
  myModal <- modalDialog(
    title = "Welcome to the Massachusetts Natural Background Conditions App!"
    ,HTML("MassNBCtools was developed to assess Massachusetts Assessement Units
           (AUs) for natural background conditions.<br> This app was developed by
          Ben Block, Tetra Tech (Ben.Block@tetratech.com), for use by the
          Massachusetts Department of Environmental Protection (MassDEP).<br> 
          Please contact Anna Mayor (anna.mayor@mass.gov), Laurie Kennedy 
          (laurie.kennedy@mass.gov) or Richard Chase (richard.f.chase@mass.gov)
           should any issues or questions arise.<br>")
    ,HTML('<center><img src="MassDEPlogo.png" height="100"></center>')
    ,easyClose = T)
  
  # Show the model on start up
  showModal(myModal)
  
  # select AUs ####
  myData <- reactive({
    df_AU_Data %>%
      select(AU_ID, AU_TempClassQual, Area_SQ_MI, PERCAG, PERCDEV
             , PERCNAT, PERCWET, PERCAG_PR, PERCDEV_PR, PERCNAT_PR, PERCWET_PR
             , PERCAG_ST, PERCDEV_ST, PERCNAT_ST, PERCWET_ST, PERCAG_SP
             , PERCDEV_SP, PERCNAT_SP, PERCWET_SP, PercImp, nDams, nGWP
             , nNPDES, nPWS_SW, nSEMS, nTRIs, pctZone2)
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
  
  ## AU select data ####
  observeEvent(input$input_AU_choice, {
    req(input$input_AU_choice != "")
    
    myAU <- input$input_AU_choice
    AU_Data <- myData()
    AU_Data_trimmed <- AU_Data %>% 
      filter(AU_ID == myAU)
    
    ### stats ####
    # land cover stats
    Area_SQ_MI <- AU_Data_trimmed$Area_SQ_MI
    PERCNAT <- AU_Data_trimmed$PERCNAT
    PERCWET <- AU_Data_trimmed$PERCWET
    PERCNAT_PR <- AU_Data_trimmed$PERCNAT_PR
    PERCWET_PR <- AU_Data_trimmed$PERCWET_PR
    PERCNAT_ST <- AU_Data_trimmed$PERCNAT_ST
    PERCWET_ST <- AU_Data_trimmed$PERCWET_ST
    PERCNAT_SP <- AU_Data_trimmed$PERCNAT_SP
    PERCWET_SP <- AU_Data_trimmed$PERCWET_SP
    PercImp <- AU_Data_trimmed$PercImp
    
    PctNat_CompWs <- PERCNAT + PERCWET
    PctNat_ProxWs <- PERCNAT_PR + PERCWET_PR
    Min_PctNat_CompWs_ProxWs <- min(PctNat_CompWs, PctNat_ProxWs)
    
    PctNat_CompBuf <- PERCNAT_ST + PERCWET_ST
    PctNat_ProxBuf <- PERCNAT_SP + PERCWET_SP
    
    # per Appendix A Table A1 (2022 CALM)
    if (Area_SQ_MI >= 25 & Min_PctNat_CompWs_ProxWs > 80 
        & PctNat_ProxBuf >90) {
      NatResult <- paste("Natural land cover exceeds CALM NBC thresholds.")
    } else if (Area_SQ_MI < 25 & Min_PctNat_CompWs_ProxWs > 80 
               & PctNat_CompBuf >90) {
      NatResult <- paste("NNatural land cover exceeds CALM NBC thresholds.")
    } else {
      NatResult <- paste("Natural land cover does not exceed CALM NBC thresholds."
                         , "Do not consider AU for natural background conditions.")
        
    }# if/else ~ END
  
    # point source counts
    nDams <- AU_Data_trimmed$nDams
    nNPDES <- AU_Data_trimmed$nNPDES
    nSEMS <- AU_Data_trimmed$nSEMS
    nTRIs <- AU_Data_trimmed$nTRIs
    
    # pct Zone II
    pctZone2 <- AU_Data_trimmed$pctZone2
    
    # temperature classes
    AU_TempClassQual <- AU_Data_trimmed$AU_TempClassQual
    
    ## outputs ####
    if (Area_SQ_MI >= 25) {
      output$output_LC_Results1 <- renderUI({
        HTML(paste(paste0("% Natural (Minimum of Watershed Scales): ", Min_PctNat_CompWs_ProxWs)
                   , paste0("% Natural (Proximate Stream Buffer): ",PctNat_ProxBuf)
                   , paste0("% Impervious: ", PercImp)
                   , paste0("Result: ", NatResult)
                   , sep="<br/>"))
      })#renderUI ~ END
      output$output_LC_Results2 <- renderUI({
        HTML(paste(paste0("% Natural (Minimum of Watershed Scales): ", Min_PctNat_CompWs_ProxWs)
                   , paste0("% Natural (Proximate Stream Buffer): ",PctNat_ProxBuf)
                   , paste0("% Impervious: ", PercImp)
                   , paste0("Result: ", NatResult)
                   , sep="<br/>"))
      })#renderUI ~ END
    } else {
      output$output_LC_Results1 <- renderUI({
        HTML(paste(paste0("% Natural (Minimum of Watershed Scales): ", Min_PctNat_CompWs_ProxWs)
                   , paste0("% Natural (Complete Stream Buffer): ",PctNat_CompBuf)
                   , paste0("% Impervious: ", PercImp)
                   , paste0("Result: ", NatResult)
                   , sep="<br/>"))
      })#renderUI ~ END
      output$output_LC_Results2 <- renderUI({
        HTML(paste(paste0("% Natural (Minimum of Watershed Scales): ", Min_PctNat_CompWs_ProxWs)
                   , paste0("% Natural (Complete Stream Buffer): ",PctNat_CompBuf)
                   , paste0("% Impervious: ", PercImp)
                   , paste0("Result: ", NatResult)
                   , sep="<br/>"))
      })#renderUI ~ END
    }# if/else ~ END
    
    output$output_dam_count1 <- renderUI({
      HTML(paste0("Number of Dams in AU: ", nDams))
      })#renderUI ~ END
    output$output_dam_count2 <- renderUI({
      HTML(paste0("Number of Dams in AU: ", nDams))
    })#renderUI ~ END
    
    output$output_ptsrc_counts1 <- renderUI({
      HTML(paste(paste0("Number of NPDES in AU: ", nNPDES)
                 , paste0("Number of Superfunds in AU: ",nSEMS)
                 , paste0("Number of TRIs in AU: ", nTRIs)
                 , sep="<br/>"))
    })#renderUI ~ END
    output$output_ptsrc_counts2 <- renderUI({
      HTML(paste(paste0("Number of NPDES in AU: ", nNPDES)
                 , paste0("Number of Superfunds in AU: ",nSEMS)
                 , paste0("Number of TRIs in AU: ", nTRIs)
                 , sep="<br/>"))
    })#renderUI ~ END
    
    output$output_pctZone2_1 <- renderUI({
      HTML(paste0("% Zone II WPA in AU: ", pctZone2))
    })#renderUI ~ END
    output$output_pctZone2_2 <- renderUI({
      HTML(paste0("% Zone II WPA in AU: ", pctZone2))
    })#renderUI ~ END
    
    output$output_TempClass <- renderUI({
      HTML(paste(paste0("AU Temperature Class Qualifier: ", AU_TempClassQual)
                 , paste0("If temperature exceedance is higher than WWF standard (28.3 DegC),"
                          , "then the AU is not considered natural.")
                 , sep="<br/>"))
      })#renderUI ~ END
    
  })#observeEvent ~ END
  
  # Mapping ####
  ## Base Map ####
  output$mymap <- renderLeaflet({
    leaflet("mymap") %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group="Esri WSM") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri Ortho") %>% 
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
      addCircleMarkers(data = GISlayer_PWS_SW, radius = 7, stroke = TRUE
                       , fillOpacity = 1, fillColor = "#c51b7d", color = "black"
                         , opacity = 1, weight = 2
                       , label = GISlayer_PWS_SW$PWS_ID, group = "PWS SW"
                       , popup = paste("<b> Site Name:</b>", GISlayer_PWS_SW$SITE_NAME, "<br>"
                                       ,"<b> PWS ID:</b>", GISlayer_PWS_SW$PWS_ID, "<br>"
                                       ,"<b> Type:</b>", GISlayer_PWS_SW$TYPE, "<br>"
                                       ,"<b> Latitude:</b>", GISlayer_PWS_SW$LATITUDE, "<br>"
                                       ,"<b> Longitude:</b>", GISlayer_PWS_SW$LONGITUDE)) %>%
      addPolylines(data = GISlayer_AUflow, color = "blue", weight = 3
                   , label = GISlayer_AUflow$AU_ID, group = "AU River Arc"
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
                  , fillColor = "#df65b0", fillOpacity = 0.25, group = "Zone 2 WPA") %>%  
      addLayersControl(overlayGroups = c("AU River Arc", "AU Watersheds", "Dams"
                                         , "NPDES", "Superfunds", "TRIs"
                                         , "Zone 2 WPA")
                       ,baseGroups = c("Esri WSM", "Esri Ortho")
                       ,options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(c("AU River Arc", "AU Watersheds", "Dams","NPDES", "Superfunds"
                  , "TRIs", "Zone 2 WPA")) %>%
      addMiniMap(toggleDisplay = TRUE, tiles = providers$Esri.WorldStreetMap
                 , position = "bottomright")
  })#renderLeaflet ~ END

    ## Map Zoom ####
    # Map that filters output data to a single AU
    observeEvent(input$input_AU_choice, {
      req(input$input_AU_choice != "")
      
      myAU <- input$input_AU_choice
      #myAU <- "MA97-23"
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