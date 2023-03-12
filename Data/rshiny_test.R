library(shiny)
library(leaflet)
library(sf)

# setting working directory to data folder
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# Loading the data and transforming to wgs84 and removing z-values
muns <- st_zm(st_transform(read_sf("unprocessed_data/boundaries/Municipalities/municipalities.shp"), crs=4326))
cantons <- st_zm(st_transform(read_sf("unprocessed_data/boundaries/Cantons/cantons.shp"), crs=4326))
ch <- st_zm(st_transform(read_sf("unprocessed_data/boundaries/Switzerland/switzerland.shp"), crs=4326))

ui <- fluidPage(
  h1("This is a map"),
  leafletOutput("map"),
  p(),
  actionButton("display_ch", "Display Switzerland"),
  actionButton("display_cantons", "Display Cantons"),
  actionButton("display_muns", "Display Municipalities"),
  actionButton("reset", "Reset")
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lng=8.26414, lat=46.81543, zoom=7)
  })
  
  observe({
    if (input$display_ch) {
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(data=ch, fill=FALSE, weight=3, color="black")
    }
  })
  
  observe({
    if (input$display_cantons) {
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(data=cantons, fill=FALSE, weight=3, color="black")
    }
  })
  
  observe({
    if (input$display_muns) {
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(data=muns, fill=FALSE, weight=3, color="black")
    }    
  })
  
  observe({
    if (input$reset) {
      leafletProxy("map") %>%
        clearShapes()
    }
  })
}

shinyApp(ui, server)
