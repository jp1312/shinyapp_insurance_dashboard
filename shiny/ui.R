
# Define user interace (ui) for application that visualize an interactive map (with leaflet) 
# of a fake multinational insurance company KPIs (GWP, LR, Acquisition ratio, etc.) in its entities around the world 
# by LoB (PL, CL) and different views (Forecast 2, Revised Budget) 

# bootstrapPage creates a UI page that loads the CSS and JavaScript for Bootstrap (the most popular HTML, CSS and JS framework)
# for developing responsive, mobile first projects on the web
# (give a try with fluidPage as well)

ui <- bootstrapPage(
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        
  # Application title
  titlePanel("Technical dashboard insurance business"),
  
  # create a UI leaflet element
  leafletOutput("map", width = "100%", height = "100%"),
        
  # Creates a panel whose contents are absolutely positioned 
  absolutePanel(
   
    top = 10, right = 10, 
    
    # create a select list that can be used to choose a single (default) or multiple items from a list of values
    selectInput("colors", "Color Scheme", 
                rownames(subset(brewer.pal.info, category %in% c("seq", "div")
                                )
                         ),
                multiple = FALSE
    ),
    
    selectInput("period", "Period", 
                c("Forecast 2 2016" = "f2", "Revised Budget 2016" = "rb")
    ),
    
    selectInput("lob", "LoB", c("Commercial Line" = "cl", "Personal Line" = "pl")
                ),

    selectInput("kpi", "KPI", 
                c("GWP" = "gwp", "EP" = "ep", "LOSS RATIO" = "lr", "EXPENSE RATIO" = "er", 
                  "ACQUISITION RATIO" = "ar", "CLAIMS HANDLING COST RATIO" = "chcr", "Combined Ratio" = "cr", 
                  "TECHNICAL RESULT" = "tech_marg")
    ),
    
    
    # create a checkbox that can be used to specify logical values
    checkboxInput("legend", "Show legend", TRUE)
    
  )
                      
)
