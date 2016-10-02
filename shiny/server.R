

server <- function(input, output, session) {
  
  # reactive expression representing a string with the name of the KPI changing as the user make changes in UI
  SelectedKpi <- reactive({
    
    paste0(input$kpi, "_", input$period, "_", input$lob)
    
  })
    
  # reactive expression representing the palette function, which changes as the user makes selections in UI
  colorpal <- reactive({
    
    colorBin(input$colors, company@data[,SelectedKpi()], 6, pretty = FALSE)
    
  })
                
  output$map <- renderLeaflet({
    
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated)
    
    leaflet(company) %>% addTiles() %>%
      setView(40.4, 3.7, zoom = 1) %>%
      addPolygons(data = world, weight = 2, col = "grey") 
    
  })
  
  # Incremental changes to the map should be performed in an observer which is like a reactive expression
  # automatically re-executing when its dependencies change
  # Each independent set of things that can change should be managed in its own observer

  # add polygons of emea-latam region with colors changing according to user choice in UI
  observe({
    
    pal <- colorpal()
    
    # leafletproxy customize a map already rendered
    leafletProxy("map", data = company) %>%
      clearShapes() %>%
      addPolygons(color = pal(company@data[,SelectedKpi()])) 
    
  })
    

  # Use a separate observer to recreate the legend as needed.
  observe({
    
    # Remove any existing legend, and only if the legend is enabled, create a new one
    leafletProxy("map", data = company) %>% 
      clearControls()
    
    if (input$legend) {
      pal <- colorpal()
      leafletProxy("map", data = company) %>% 
        addLegend(position = "bottomright", pal = pal, values = company@data[,SelectedKpi()], opacity = 1, title = SelectedKpi(),
                            labFormat = labelFormat(digits = 2, prefix = ""))  
    }
    
  })
    

}

