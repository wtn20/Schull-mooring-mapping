library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(readxl)
library(glue)

server <- function(input, output, session) {
  
  # Read data from up-loaded Excel file
  moorings <- reactive({
    req(input$dataFile)
    
    inFile <- input$dataFile
    
    pathToFile <- inFile$datapath
    
    tryCatch({
      excel_data <- pathToFile %>%
        excel_sheets() %>%
        set_names() %>%
        map(read_excel, path = pathToFile)
        
      parsedData <- bind_rows(excel_data)
      
      # Build the actual latitude and longitude
      parsedData$latitude <- parsedData$`Lat - deg` + (parsedData$`Lat - min` + parsedData$`Lat - dig min`/ 1000.0)/ 60.0
      parsedData$longitude <- - (parsedData$`Long - deg` + (parsedData$`Long - min` + parsedData$`Long - dig min`/ 1000.0)/ 60.0)
      
  #    # Fill any missing `Chain size mm` data with 0
  #    parsedData$`Chain size mm`[is.na(parsedData$`Chain size mm`)] <- 0
  #    
  #    # Fill any missing `Chain length m` data with 0
  #    parsedData$`Chain length m`[is.na(parsedData$`Chain length m`)] <- 0
  #    
  #    # Fill any missing `Max size m` data with 0
  #    parsedData$`Max size m`[is.na(parsedData$`Max size m`)] <- 0
  #    
      # Convert `On request` to a boolean
      parsedData$`On request (bool)` = ifelse(parsedData$`On request` == "x", TRUE, FALSE)
      parsedData$`On request (bool)`[is.na(parsedData$`On request (bool)`)] <- FALSE
      
  #    # Fill any missing `Last major check` data with 1900-01-01
  #    parsedData$`Last major check`[is.na(parsedData$`Last major check`)] <- as.Date("1900-01-01")
      
  #    # Fill any missing `Chain year` data with 1900
  #    parsedData$`Chain year`[is.na(parsedData$`Chain year`)] <- "1900"
      
      parsedData
      },
    warning = function(w){},
    error = function(e){
      parsedData <- NULL
      validate(need(!is.null(parsedData), "Failed to read data from file"))
      },
    finally = {}
  )
  })
  
  # Provide the read data to the UI as as Datatable
  output$parsedData <- renderDataTable({
    validate(need(!is.null(moorings()), "Failed to read data from file"))
      if (is.null(input$dataFile))
        return(NULL)
      moorings()
    }
  )
  
  # Reactive expression to update the filter's min/ max based on the read in data
  observe({
    if (!is.null(moorings())){
      colData <- moorings()$`Chain size mm`
      updateSliderInput(session, "chain_size_range", value = c(min(colData, na.rm = TRUE), max(colData, na.rm = TRUE)),
                        min = min(colData, na.rm = TRUE), max = max(colData, na.rm = TRUE), step = 1)
      
      colData <- moorings()$`Chain length m`
      updateSliderInput(session, "chain_length_range", value = c(min(colData, na.rm = TRUE), max(colData, na.rm = TRUE)),
                        min = min(colData, na.rm = TRUE), max = max(colData, na.rm = TRUE), step = 1)
      
      colData <- moorings()$`Max size m`
      updateSliderInput(session, "max_size_range", value = c(min(colData, na.rm = TRUE), max(colData, na.rm = TRUE)),
                        min = min(colData, na.rm = TRUE), max = max(colData, na.rm = TRUE), step = 1)
      
      colData <- moorings()$`Last major check`
      updateSliderInput(session, "last_major_check_range", value = c(min(colData, na.rm = TRUE), max(colData, na.rm = TRUE)),
                        min = min(colData, na.rm = TRUE), max = max(colData, na.rm = TRUE), step = 1)
    }
  })
  
  
  
  # Reactive expression for the data subsetted to what the user selected
  unknownChainSize <- reactive({ if (!is.null(moorings())){is.na(moorings()$`Chain size mm`)}})
  unknownChainLength <- reactive({ if (!is.null(moorings())){is.na(moorings()$`Chain length m`)}})
  unknownMaxSize <- reactive({ if (!is.null(moorings())){is.na(moorings()$`Max size m`)}})
  unknownLastMajorCheck <- reactive({ if (!is.null(moorings())){is.na(moorings()$`Last major check`)}})
  onRequest <- reactive({ if (!is.null(moorings())){moorings()$`On request (bool)`}})
  
    
  acceptableChainSize <- reactive({if (!is.null(moorings())){
    moorings()$`Chain size mm` >= input$chain_size_range[1] & moorings()$`Chain size mm` <= input$chain_size_range[2]}
    })
  acceptableChainLength <- reactive({if (!is.null(moorings())){
    moorings()$`Chain length m` >= input$chain_length_range[1] & moorings()$`Chain length m` <= input$chain_length_range[2]}
    })
  acceptableMaxSize <- reactive({if (!is.null(moorings())){
    moorings()$`Max size m` >= input$max_size_range[1] & moorings()$`Max size m` <= input$max_size_range[2]}
    })
  acceptableLastMajorCheck <- reactive({if (!is.null(moorings())){
    moorings()$`Last major check` >= input$last_major_check_range[1] & moorings()$`Last major check` <= input$last_major_check_range[2]}
    })
  
  
  filteredData <- reactive({
    validate(
      need(!is.null(moorings()), "Failed to read the data from the file")
             )
    if (!is.null(input$dataFile)){
      mooringsDf <- moorings()
    
      filterMask <- (
        acceptableChainSize() | (input$unknownChainSize & unknownChainSize())
      ) & (
        acceptableChainLength() | (input$unknownChainLength & unknownChainLength())
      ) & (
        acceptableMaxSize() | (input$unknownMaxBoatSize & unknownMaxSize())
      ) & (
        acceptableLastMajorCheck() | (input$unknownLastMajorCheck & unknownLastMajorCheck())
      ) & (
        (if(input$onRequest == 0) {!onRequest()}
         else if (input$onRequest == 1) {TRUE}
         else if (input$onRequest == 2) {onRequest()}
         )
      )
    
      filteredMoorings <- mooringsDf[filterMask,]
      filteredMoorings
    }
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    if (!is.null(input$dataFile)){
      colorNumeric(input$colors, quakes$`Chain size mm`)
    }
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    
    m <- leaflet()
    m <- addTiles(m)
    m <- setView(m, -9.542683, 51.524050, zoom = 16)
    m   
    
#    leaflet(moorings()) %>% addTiles() %>%
#      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    if (!is.null(input$dataFile)){
      getPopUp <- function(initials,
                           surname,
                           firstName,
                           winterBuoyYN,
                           buoy,
                           chainSizeMm,
                           chainLengthM,
                           chainYear,
                           lastMajorCheck,
                           maxSizeM,
                           onRequest,
                           notes,
                           currentInsp
                           ){
        
        html_header = "
        <!DOCTYPE html>
        <html>
          <head>
            <style type=\"text/css\">
              * {
                  box-sizing: border-box;
                }
        
              .leaflet-popup-content {
                  width: 475px;
                  height: 300px;
              }
        
              .column {
                float: left;
                width: 33.33%;
                padding: 5px;
              }
        
              th, td {
                border: 1px solid black;
                border-collapse: collapse;
                padding: 15px;
              }
  
              table {
                float: left;
                width: 50;
                height: 50;
                margin-right: 1%;
                border: 1px solid black;
                border-collapse: collapse;
                padding: 15px;
              }
            </style>
        </head>
        "
        
        html_body = glue("
        <body>
        
          <header><h2>{firstName} {surname}</h2></header>
        
          <div>
            <table>
              <tr>
                <td>Winter buoy?</td>
                <td>{winterBuoyYN}</td>
              </tr>
              <tr>
                <td>Buoy</td>
                <td>{buoy}</td>
              </tr>
              <tr>
                <td>Chain size (mm)</td>
                <td>{chainSizeMm}</td>
              </tr>
              <tr>
                <td>Chain length (m)</td>
                <td>{chainLengthM}</td>
              </tr>
              <tr>
                <td>Chain year</td>
                <td>{chainYear}</td>
              </tr>
            </table>
    
            <table>
                <tr>
                  <td>Last major check</td>
                  <td>{lastMajorCheck}</td>
                </tr>
                <tr>
                  <td>Max boat size (m)</td>
                  <td>{maxSizeM}</td>
                </tr>
                <tr>
                  <td>On request?</td>
                  <td>{onRequest}</td>
                </tr>
                <tr>
                  <td>2019 Inspection?</td>
                  <td>{currentInsp}</td>
                </tr>
                <tr>
                  <td>Notes</td>
                  <td>{notes}</td>
                </tr>  
            </table>
          </div>
        </body>
        "
        )
        
        paste(html_header, html_body)
      }
      
      iframe_width <- 300
      iframe_height<- 500
      
      pal <- colorpal()
      
      mooring_data <- filteredData() %>%
        mutate(maxDist = `Chain length m` + `Max size m`) %>%
        mutate(popUpHtml = getPopUp(initials = `Initials`,
                                    surname = `Surname`,
                                    firstName = `First Name`,
                                    winterBuoyYN = `Winter Buoy (Y/N)`,
                                    buoy = `Buoy`,
                                    chainSizeMm = `Chain size mm`,
                                    chainLengthM = `Chain length m`,
                                    chainYear = `Chain year`,
                                    lastMajorCheck = `Last major check`,
                                    maxSizeM = `Max size m`,
                                    onRequest = `On request (bool)`,
                                    notes = `Notes`,
                                    currentInsp = `2019 insp`
                                    )
        )
      
      leafletProxy("map", data = mooring_data) %>%
        clearMarkers() %>%
        clearShapes() %>%
        addMarkers(~longitude, ~latitude, popup = ~popUpHtml,
                   popupOptions = popupOptions(maxWidth = 500, maxHeight = 500)) %>%
        addCircles(radius = ~maxDist,
                   weight = 1, 
                   color = "#777555",
                   fillColor = ~pal(`Chain size mm`), 
                   fillOpacity = 0.6
        )%>%
        addCircles(radius = ~`Chain length m`,
                   weight = 1, 
                   color = "#777777",
                   fillColor = ~pal(`Chain size mm`), 
                   fillOpacity = 0.8
        )
    }
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    if (!is.null(input$dataFile)){
      proxy <- leafletProxy("map", data = filteredData())
      
      # Remove any existing legend, and only if the legend is
      # enabled, create a new one.
      proxy %>% clearControls()
      if (input$legend) {
        pal <- colorpal()
        proxy %>% addLegend(position = "bottomright",
                            pal = pal, values = ~`Chain size mm`
        )
      }
    }
  })
}
