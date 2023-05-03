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
typ <- st_make_valid(st_zm(st_transform(read_sf("unprocessed_data/boundaries/Typology/mun_typology.shp"), crs=4326)))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "type_input",
        label = "Choose an administrative unit to display",
        choices = list("Switzerland", "Canton", "Municipality type", "Municipality")
      )
    ),
    leafletOutput("map")
  )
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
    if (input$type_input == "Switzerland") {
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(data=ch, fill=FALSE, weight=3, color="black")
    }
  })
  
  observe({
    if (input$type_input == "Canton") {
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(data=cantons, fill=FALSE, weight=3, color="black")
    }
  })
  
  observe({
    if (input$type_input == "Municipality") {
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(data=muns, fill=FALSE, weight=3, color="black")
    }    
  })
  
  observe({
    if (input$type_input == "Municipality type") {
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(data=typ, fill=FALSE, weight=3, color="black")
    }    
  })
  
  # Figuring out where the user clicks
  observe({
    if (!is.null(input$map_click)) {
      point <- data.frame(x = input$map_click$lng, 
                          y = input$map_click$lat)
      
      # generating point object
      click <- st_as_sf(point, coords = c("x", "y"), crs=4326, agr="constant")
    
      if (input$type_input  == "Switzerland") {
        selected <- ch[click,, op=st_contains]
        print(selected$NAME)
      } else if (input$type_input  == "Municipality") {
        selected <- muns[click,, op=st_contains]
        print(selected$NAME)
      } else if (input$type_input  == "Canton") {
        selected <- cantons[click,, op=st_contains]
        print(selected$NAME)
      } else if (input$type_input  == "Municipality type") {
        selected <- typ[click,, op=st_contains]
        print(selected$NAME)
      }
      
    
    }
  })
}

shinyApp(ui, server)
