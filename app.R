library(shiny)
library(leaflet)
library(RColorBrewer)
library(magrittr)
library(sf)
options(shiny.port=4500)

M = read.csv("./points.csv", sep=",", header=FALSE)
quake <- as.data.frame(
  cbind(
    M[1:1000,1], M[1:1000,2], quakes[,4]/1.2
  )
)
colnames(quake) = c("lat","long","mag")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
    sliderInput("range", "Magnitudes", min(quake$mag), max(quake$mag), 
      value = range(quake$mag), step = 0.1
    ),
    selectInput("colors", "Color Scheme",
      rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
    ),
    checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  filteredData <- reactive({
    quake[quake$mag >= input$range[1] & quake$mag <= input$range[2],]
  })
  # palette function which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quake$mag)
  })

  output$map <- renderLeaflet({
    # only include aspects of the map that won't change dynamically
    # (not unless the entire map is being torn down and recreated).
    greenLeafIcon <- makeIcon(
    iconUrl = 'https://cdn2.iconfinder.com/data/icons/ios-7-icons/50/user_male2-128.png',
    iconWidth = 32, iconHeight = 32,
    iconAnchorX = 32, iconAnchorY = 32#,
    # shadowUrl = 'http://leafletjs.com/examples/custom-icons/leaf-shadow.png',
    # shadowWidth = 50, shadowHeight = 64, shadowAnchorX = 4, shadowAnchorY = 62
  )
  
  leaflet(quake) %>% addTiles() %>%
    addMarkers(
      clusterOptions = markerClusterOptions(),
      icon = greenLeafIcon) %>%
      addMiniMap(
        tiles = providers$Esri.WorldStreetMap,
        toggleDisplay = TRUE
      )
  })

  observe({
    pal <- colorpal()
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles (
        radius = ~10^mag/10, weight = 1, color = "#777777",
        fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })

  # legend
  observe({
    proxy <- leafletProxy("map", data = quake)
    # Remove existing legend, and if enabled, create.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend (
        position = "bottomright",
        pal = pal, values = ~mag
      )
    }
  })
  
  proxy <- leafletProxy("mymap")
  proxy %>% fitBounds(0, 0, 11, 11)
  proxy %>% addCircles(1:10, 1:10, layerId = LETTERS[1:10])
  proxy %>% removeShape(c("B", "F"))
  proxy %>% clearShapes()
}

shinyApp(ui, server)