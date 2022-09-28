server <- function(input, output, session) {

  # ### SERVER SETUP
  #
  # Server starts by calling DART-package functions in a shiny
  # enrionment set up to interact with dynamically. The app will
  # initialize a fresh DART, its parameters will be set according
  # to user inputs, and the 'Run DART' button should execute the
  # GetDart() function according to the input dynamic DART object.
  #
  # Some core DART functions need to be called ahead of GetDart()
  # in order to provide the visualizations to users that they need
  # to make decisions about parameterizing the DART object.

  # Passed parameters:
  # .debugMessage
  # .directory

  # Load libraries
  require(DART) # called explicitly
  require(shiny) # called implicitly
  require(writexl) # called explicitly
  require(leaflet) # called implicitly
  require(DART) # called explicitly
  require(raster) # called explicitly
  require(shinyjs) # called explicitly
  require(data.table) # called explicitly

  # Initialize a starting DART object
  DART_static <- DART::InitializeDART(dir = .directory, 2L)

  # ### DYNAMIC OBJECTS
  #
  # `reactiveValues` are dynamic - i.e., they can be read from/write to
  # in a way that can be modified as the app is running. The initial
  # values defined here

  DART_dynamic <- reactiveValues(
    msg = "",
    msg2 = "",
    lat = 38.478,
    lng = -110.22,

    DART_reactive = DART_static,
    analysisDirections = "Select 'Run Dart' then 'Analyze' on the Interactive Map Menu to see plots"
  )

  #  Reactive Events

  # A. Map clicking
  #     1. Log coordinates of click

  observeEvent(input$mymap_click, {
    dynamic_DART$msg <- input$mymap_click
    dynamic_DART$lng <- input$mymap_click$lng
    dynamic_DART$lat <- input$mymap_click$lat
  })

  #     2. Re-draw the markers plus search radius

  observe({

    #searchPoly <- DART::projectDART(DART::GetTarget(dynamic_DART$lng, dynamic_DART$lat, input$searchRadius + input$targetRadius))
    searchPoly <- DART::GetTarget(DART_dynamic$DART_reactive, x = DART_dynamic$lng, y = DART_dynamic$lat)
    dartPoly <- DART::projectDART(DART::GetTarget(dynamic_DART$lng, dynamic_DART$lat, input$targetRadius))
    bufferPoly <- DART::projectDART(DART::GetTarget(dynamic_DART$lng, dynamic_DART$lat, input$buffer + input$targetRadius))

    leafletProxy('mymap') %>%
      clearMarkers() %>%
      addMarkers(lng = dynamic_DART$lng, lat = dynamic_DART$lat) %>%
      clearShapes() %>%
      addPolygons(data = dartPoly, fillColor='transparent', group = "Polygons")%>%
      addPolygons(data = searchPoly, color = 'red', fillColor='transparent', group = "Polygons")%>%
      addPolygons(data = bufferPoly, color = 'red', dashArray = '10',fillColor='transparent', group = "Polygons")#%>%
      # TODO: no idea where ss comes from
      #addPolygons(data = ss, fillColor='transparent', group = "Polygons", dashArray = '10', col = 'white')
  })

  # What to do when Run Dart clicked #

  # pull dart results
  observeEvent(input$runDart,
               {
                 showModal(modalDialog('Running Dart', footer = NULL))
                 attempt <- try ({
                   DART::GetDart(x = dynamic_DART$lng, y = dynamic_DART$lat, buffer = input$buffer,
                                 searchRadius = input$searchRadius,
                                 targetRadius = input$targetRadius,
                                 nControl = input$nControl)
                 })

                 removeModal()
                 if(class(attempt) == 'try-error'){

                   showNotification(attempt,duration = NULL, type = 'error')
                   dartResult <- dynamic_DART$dartResult
                 } else {
                   dartResult <- attempt
                 }

                 dynamic_DART$dartResult <- dartResult

                 #cashe params
                 dynamic_DART$dartParams <- list(
                   lng = dynamic_DART$lng,
                   lat = dynamic_DART$lat,
                   buffer = input$buffer,
                   searchRadius = input$searchRadius,
                   targetRadius = input$targetRadius,
                   ncontrol = input$nControl,
                   synthControlStartYear = input$dDate
                 )


                 # extract timeseries for targets + references
                 dynamic_DART$extraction <- DART::extractDart(dynamic_DART$dartResult, 'SATVI' )
                 dynamic_DART$synthControl <- DART::synthCtrl( dynamic_DART$extraction, 1987, 'SATVI' )

                 ## Timeseries plots
                 output$myplot <- renderPlot(DART::ts_plot(dynamic_DART$extraction, 'SATVI'),
                                             height = function() {
                                               #https://github.com/rstudio/shiny/issues/650
                                               pmin(session$clientData$output_myplot_width * 0.6, 800)
                                             }
                 )

                 ## Synthetic Control Plots
                 output$scPlot <- renderPlot(plot(dynamic_DART$synthControl), height = function() {
                   #https://github.com/rstudio/shiny/issues/650
                   pmin(session$clientData$output_scPlot_width * 0.6, 800)
                 })

                 ## Variable importance Plots
                 output$cartPlot <- renderVisNetwork(DART::viewImportance(dynamic_DART$dartResult))


                 showModal(modalDialog('Analysis Completed', footer= tagList(modalButton('OK'))))

                 chosen <- raster::raster(dartResult$chosenPixels['distance'])
                 targetRast <- raster::raster(dartResult$targetRast['refrast'])
                 cz <- colorNumeric(cols, raster::values(chosen), na.color = 'transparent')

                 # plot results on map
                 leafletProxy('mymap') %>%
                   clearImages() %>%
                   addRasterImage(dartResult$candidates, col = 'blue', opacity = 0.15, group = 'Candidates')%>%
                   addRasterImage(dartResult$edaphicRaster, col = 'green', opacity = 0.25, group = 'Edaphic Subset')%>%
                   addRasterImage(targetRast, col = 'purple', opacity = 1, group = 'Target Pixels')%>%
                   addRasterImage(chosen, colors = cz, opacity = 1, group = 'Topo-edaphic Matches')%>%
                   clearControls() %>%
                   addLayersControl(overlayGroups = c("Candidates", "Edaphic Subset", "Target Pixels", "Topo-edaphic Matches")) %>%
                   addLegend( values = values(chosen), pal = cz ,title = "Topo-distance", position = 'bottomright')

               }
  )

  # Toggle Menu
  observeEvent(input$Menu, {
    shinyjs::toggle(id = "Sidebar")
  })


  # What to do when "Analyze" clicked

  # observeEvent(input$analyze,
  # {
  # strt <- format(Sys.time(), '%H:%M:%S')
  # msg <- paste0("Extracting response variables -- this may take a while --
  # started at ",strt)
  # showModal(modalDialog(msg, footer=NULL))

  # extract timeseries for targets + references
  # dynamic_DART$extraction <- extractDart( dynamic_DART$dartResult, 'SATVI' )
  # dynamic_DART$synthControl <- synthCtrl( dynamic_DART$extraction, input$dDate, 'SATVI' )

  # removeModal()

  # generate a timeseries plot

  # output$myplot <- renderPlot( ts_plot(dynamic_DART$extraction, 'SATVI') )
  # output$scPlot <- renderPlot( plot(dynamic_DART$synthControl) )

  # showModal(modalDialog("Analysis Finished", footer= tagList( actionButton('goto', "Go To"), modalButton("Dismiss"))))

  # go to plot
  # dynamic_DART$analysisDirections = ''
  # })

  # When analysis finished and user clicks 'go to'

  observeEvent(input$goto, {
    updateTabsetPanel(session, "nav", selected = "analysisPanel")
  })

  # What to do when 'Export' clicked

  # Downloadable xlsx of selected dataset ----
  output$exportPDF <- downloadHandler(

    filename = function() {
      f <- paste0(
        'DART_',
        round(dynamic_DART$dartParams$lng,3),
        '_',
        round(dynamic_DART$dartParams$lat,3),
        '_',
        format(Sys.time(), '%Y-%m-%d-%H-%M'),
        '.pdf'
      )
    },
    content = function(file) {

      searchPoly <- DART::projectDART(DART::GetTarget(
        dynamic_DART$dartParams$lng,
        dynamic_DART$dartParams$lat,
        dynamic_DART$dartParams$searchRadius + dynamic_DART$dartParams$targetRadius
      ))

      dartPoly <- DART::projectDART(DART::GetTarget(
        dynamic_DART$dartParams$lng,
        dynamic_DART$dartParams$lat,
        dynamic_DART$dartParams$targetRadius
      ))

      bufferPoly <- DART::projectDART(DART::GetTarget(
        dynamic_DART$dartParams$lng,
        dynamic_DART$dartParams$lat,
        dynamic_DART$dartParams$buffer + dynamic_DART$dartParams$targetRadius
      ))

      chosen <- raster::raster(dynamic_DART$dartResult$chosenPixels['distance'])
      targetRast <- raster::raster(dynamic_DART$dartResult$targetRast['refrast'])
      cz <- colorNumeric(cols, values(chosen), na.color = "transparent")

      params = list(
        dartParams = dynamic_DART$dartParams,
        bigMap = leaflet() %>%
          addTiles(urlTemplate = mapTilesUrlTemplate) %>%
          setView(lng = -110.22, lat = 38, zoom = 6) %>%
          addMarkers(lng = dynamic_DART$dartParams$lng, lat = dynamic_DART$dartParams$lat) %>%
          addPolygons(data = ss, fillColor='transparent', dashArray = '10',col = 'white', group = "Polygons"),
        smallMap = leaflet() %>%
          addTiles(urlTemplate = mapTilesUrlTemplate) %>%
          addPolygons(data = dartPoly, fillColor='transparent', group = "Polygons")%>%
          addPolygons(data = searchPoly, color = 'red', fillColor='transparent', group = "Polygons")%>%
          addPolygons(data = bufferPoly, color = 'red', dashArray = '10',fillColor='transparent', group = "Polygons") %>%
          setView(lng = dynamic_DART$dartParams$lng, lat = dynamic_DART$dartParams$lat, zoom = 14),
        dartMap = leaflet() %>%
          addTiles(urlTemplate = mapTilesUrlTemplate) %>%
          addRasterImage(dynamic_DART$dartResult$edaphicRaster, col = 'green', opacity = 0.25, group = 'Edaphic Subset')%>%
          addRasterImage(targetRast, col = 'purple', opacity = 1, group = 'Target Pixels')%>%
          addRasterImage(chosen, colors = cz, opacity = 1, group = 'Topo-edaphic Matches')%>%
          setView(lng = dynamic_DART$dartParams$lng, lat = dynamic_DART$dartParams$lat, zoom = 14),
        extraction = dynamic_DART$extraction,
        dartOutput = dynamic_DART$dartResult,
        sc = dynamic_DART$synthControl
      )

      # TODO Fix this, broken depenency
      #DART::generateRmdReport(file, params)

      invisible()
    }

  )

  output$exportXLS <- downloadHandler(
    filename = function() {
      f <- paste0(
        'DART_',
        round(dynamic_DART$dartParams$lng,3),
        '_', round(dynamic_DART$dartParams$lat,3),'_',
        format(Sys.time(), '%Y-%m-%d-%H-%M'),
        '.xlsx'
      )
    },

    content = function(file) {
      writexl::write_xlsx(datasetInput(), path = file)
    }
  )

  # Data prepper
  datasetInput <- reactive({

    # Dart pixels
    ref <- as.data.frame(DART::projectDART(dynamic_DART$dartResult$chosenPixels))
    target <- as.data.frame(DART::projectDART(dynamic_DART$dartResult$targetRast))
    ref$type <- 'dartReference'
    target$type <- 'target'

    out <- data.table::rbindlist(list(target, ref), fill = TRUE)
    out <- out[ , c( 'type', 'x', 'y', 'freq', 'distance', 'soilps', 'soilec', names(dpar$topoVars)), with =F ]

    data.table::setnames(out, 'freq', 'freqSelected')
    data.table::setnames(out, 'distance', 'avgTopoDist')
    data.table::setnames(out, 'soilps', 'soilParticleSizeClass')

    # Metadata
    dp <- dynamic_DART$dartParams
    kv <-   c(dartVersion = packageVersion('DART'),
              lat = dp$lat,
              lng = dp$lng,
              date = format(Sys.time(), '%Y-%m-%d %H:%M:%S'),
              targetRadius = dp$targetRadius,
              searchRadius = dp$searchRadius,
              buffer = dp$buffer,
              nControl = dp$nControl)
    meta <- data.frame(key = names(kv), value = kv)

    # Quantiles
    quants <- getQuantiles(dynamic_DART$extraction, 'SATVI')
    list(metadata = meta, DartPixels = as.data.frame(out), quantiles = quants)

  })

  #  Webpage Elements

  # Background Map
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = mapTilesUrlTemplate) %>%
      setView(lng = -110.22, lat = 38, zoom = 6) %>%
      clearShapes() #%>%
    # TODO: no idea where 'ss' comes from
      #addPolygons(
      #  data = ss,
      #  fillColor = 'transparent',
      #  dashArray = '10',
      #  col = 'white',
      #  group = "Polygons"
      #)

  })

  # Menu
  output$mymenu <- renderPrint({
    print(dynamic_DART$msg2)
  })

  # Debug Message
  output$mymessage <- renderPrint({
    print(dynamic_DART$msg)
  })

  output$analysisDirections <- renderPrint({
    print(dynamic_DART$analysisDirections)
  })

}
