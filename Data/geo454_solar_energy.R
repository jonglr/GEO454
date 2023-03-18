library(shiny)
library(scales)
library(ggplot2)
library(dplyr)
library(leaflet)
library(sf)

# Reading data and transforming to WGS84
muns <- st_transform(st_read("processed_data/municipalities/municipalities.shp"), crs=4326)
cantons <-  st_transform(st_read("processed_data/cantons/cantons.shp"), crs=4326)
typ <-  st_transform(st_read("processed_data/typology/typology.shp"), crs=4326)
ch <-  st_transform(st_read("processed_data/switzerland/switzerland.shp"), crs=4326)

years <- seq(2004, 2022)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "type_input",
        label = "Choose an administrative unit to display",
        choices = list("Switzerland", "Canton", "Municipality type", "Municipality")
      )
    ),
    mainPanel(leafletOutput("map"), plotOutput("plot"))
  )
)

server <- function(input, output, session) {
  
  viz <- ch %>%
    select(starts_with("kw") &! ends_with("tot")) %>%
    st_drop_geometry()
  
  power <- cumsum(c(viz[1,]))
  to_visualize <- data.frame(years = years, power = power)

  output$plot <- renderPlot({
    ggplot(to_visualize) +
      geom_line(mapping=aes(x = years, y =power)) +
      labs(title = "Power of photovoltaic plants in Switzerland", x="", y="Total operating power [kW]") +
      scale_y_continuous(labels= comma) +
      theme_classic()
  })
  
  # Other good options for basemap
  # - Esri.WorldShadedRelief --> No borders, just topography and lakes 
  # - CartoDB.PositronNoLabels
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels,
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
  
  # Figuring out where the user clicks and updating the plot accordingly
  observe({
    if (!is.null(input$map_click)) {
      point <- data.frame(x = input$map_click$lng, 
                          y = input$map_click$lat)
      
      # generating point object
      click <- st_as_sf(point, coords = c("x", "y"), crs=4326, agr="constant")
      
      within_ch <- st_intersects(click, ch, sparse=F)
      # check whether click is within Switzerland
      if (within_ch) {
        if (input$type_input  == "Switzerland") {
          # getting geometry of where user clicked if Switzerland
          selected_df <- ch[click,, op=st_contains]
          
          # subsetting the dataframe
          viz <- ch %>%
            select(starts_with("kw") &! ends_with("tot")) %>%
            st_drop_geometry()
  
        } else if (input$type_input  == "Municipality") {
          # getting geometry of where user clicked if municipality
          selected_df <- muns[click,, op=st_contains]
          
          # subsetting the dataframe
          viz <- filter(muns, name == selected_df$name[1]) %>%
            select(starts_with("kw") &! ends_with("tot")) %>%
            st_drop_geometry()   
  
        } else if (input$type_input  == "Canton") {
          # getting geometry of where user clicked if canton
          selected_df <- cantons[click,, op=st_contains]
          
          # subsetting the dataframe
          viz <- filter(cantons, name == selected_df$name[1]) %>%
            select(starts_with("kw") &! ends_with("tot")) %>%
            st_drop_geometry()
  
        } else if (input$type_input  == "Municipality type") {
          # getting geometry of where user clicked if mun type
          selected_df <- typ[click,, op=st_contains]
          
          # subsetting the dataframe
          viz <- filter(typ, name == selected_df$name[1]) %>%
            select(starts_with("kw") &! ends_with("tot")) %>%
            st_drop_geometry()
        }
        
        # constructing the plot
        power <- cumsum(c(viz[1,]))
        to_visualize <- data.frame(years = years, power = power)
        
        if (input$type_input == "Municipality" | input$type_input == "Canton") {
          title = paste("Power of photovoltaic plants in", input$type_input, selected_df$name[1])
        } else {
          title = paste("Power of photovoltaic plants in", selected_df$name[1])
        }
        
        # display the clicked layer on the map in red
        leafletProxy("map") %>%
          # remove currently selected (red layer)
          removeControl(layerId="currently_selected") %>%
          # add currently selected layer as red polygon as indication
          addPolygons(data=selected_df, fill=FALSE, weight=3, color="red", layerId="currently_selected")
        
        # update the plot
        output$plot <- renderPlot({
          ggplot(to_visualize) +
            geom_line(mapping=aes(x = years, y =power)) +
            labs(title = title, x="", y="Total operating power [kW]") +
            scale_y_continuous(labels= comma) +
            theme_classic()
        })
      }
    }
  })
}

shinyApp(ui, server)
