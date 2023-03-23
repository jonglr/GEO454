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
      ),
      actionButton(
        inputId = "select_multiple",
        label = "Start selecting"
      ),
      actionButton(
        inputId = "reset",
        label = "Reset selection"
      )
    ),
    mainPanel(leafletOutput("map"), plotOutput("plot"))
  )
)

server <- function(input, output, session) {
  
  # storing all points that the user has clicked
  lats_clicked <- c()
  lngs_clicked <- c()
  
  # variable indicating whether a new click has been made
  new_click <- T
  
  # variable indicating whether the selection is currently in progress
  selection_in_progress <- F
  
  # Rendering the initial plot that is displayed when the application is launched
  viz <- ch %>%
    select(starts_with("gwh") &! ends_with("tot")) %>%
    st_drop_geometry()
  
  potential <- st_drop_geometry(ch)[1, "p_rf_fac"]
  installed <- cumsum(c(viz[1,]))
  to_visualize <- data.frame(years = years, installed = installed, name="Switzerland", potential=potential)

  output$plot <- renderPlot({
    ggplot(to_visualize) +
      geom_line(mapping=aes(x = years, y =installed/potential, col=name)) +
      labs(title = "Exhaustion of solar energy potential", x="", y="Exhaustion [%]", col="") +
      scale_y_continuous(labels= percent) +
      theme_classic()
  })
  
  # dataframes for multiple selection
  to_vis_graph = data.frame()
  to_vis_map = data.frame()
  
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
  
  # dynmically updating the selection button
  observe({
    if (input$select_multiple){
      if (!selection_in_progress){
        updateActionButton(session,
                           inputId = "select_multiple",
                           label = "Stop selecting")
        selection_in_progress <<- T
      } else {
        updateActionButton(session,
                           inputId = "select_multiple",
                           label = "Start selecting")
        selection_in_progress <<- F
      }
    }
  })
  
  observe({
    if (input$type_input == "Switzerland") {
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(data=ch, fill=FALSE, weight=3, color="black")
      new_click <<- F
    }
  })
  
  observe({
    if (input$type_input == "Canton") {
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(data=cantons, fill=FALSE, weight=3, color="black")
      new_click <<- F
    }
  })
  
  observe({
    if (input$type_input == "Municipality") {
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(data=muns, fill=FALSE, weight=3, color="black")
      new_click <<- F
    }    
  })
  
  observe({
    if (input$type_input == "Municipality type") {
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(data=typ, fill=FALSE, weight=3, color="black")
      new_click <<- F
    }    
  })
  
  # Figuring out where the user clicks and updating the plot accordingly
  observe({
    if (!is.null(input$map_click)) {
      point <- data.frame(x = input$map_click$lng, 
                          y = input$map_click$lat)
      # checking whether the point has been clicked before
      if (!input$map_click$lng %in% lngs_clicked & !input$map_click$lat %in% lats_clicked){
        lngs_clicked <<- append(lngs_clicked, input$map_click$lng)
        lats_clicked <<- append(lats_clicked, input$map_click$lat)
        new_click <<- T
      }
      
      if (new_click) {
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
              select(starts_with("gwh") &! ends_with("tot")) %>%
              st_drop_geometry()
    
          } else if (input$type_input  == "Municipality") {
            # getting geometry of where user clicked if municipality
            selected_df <- muns[click,, op=st_contains]
            
            # subsetting the dataframe
            viz <- filter(muns, name == selected_df$name[1]) %>%
              select(starts_with("gwh") &! ends_with("tot")) %>%
              st_drop_geometry()
    
          } else if (input$type_input  == "Canton") {
            # getting geometry of where user clicked if canton
            selected_df <- cantons[click,, op=st_contains]
            
            # subsetting the dataframe
            viz <- filter(cantons, name == selected_df$name[1]) %>%
              select(starts_with("gwh") &! ends_with("tot")) %>%
              st_drop_geometry()
    
          } else if (input$type_input  == "Municipality type") {
            # getting geometry of where user clicked if mun type
            selected_df <- typ[click,, op=st_contains]
            
            # subsetting the dataframe
            viz <- filter(typ, name == selected_df$name[1]) %>%
              select(starts_with("gwh") &! ends_with("tot")) %>%
              st_drop_geometry()
          }
          
          potential <- st_drop_geometry(selected_df)[1, "p_rf_fac"]
          installed <- cumsum(c(viz[1,]))
          
          if (selection_in_progress) {
            # add to existing
            to_vis_graph <<- rbind(to_vis_graph, data.frame(years = years, installed = installed, name=selected_df$name[1], potential=potential))
            to_vis_map <<- rbind(to_vis_map, selected_df)
          } else {
            # replace
            to_vis_graph <<- data.frame(years = years, installed = installed, name=selected_df$name[1], potential=potential)
            to_vis_map <<- selected_df
          }

          # display the clicked layers on the map in red
          leafletProxy("map") %>%
            clearGroup("current_selection") %>%
            # add currently selected layer as red polygon as indication
            addPolygons(data=to_vis_map, fill=FALSE, weight=3, color="red", group="current_selection")
            
          # update the plot
          output$plot <- renderPlot({
            ggplot(to_vis_graph) +
              geom_line(mapping=aes(x = years, y =installed/potential, col=name)) +
              labs(title = "Exhaustion of solar energy potential", x="", y="Exhaustion [%]", col="") +
              scale_y_continuous(labels= percent) +
              theme_classic()
          })
        }
      }
    }
  })
  
  observe({
    if (input$reset){
      # reset graph to Switzerland
      if (input$type_input == "Switzerland") {
        leafletProxy("map") %>%
          # remove everything
          clearShapes() %>%
          # re-render
          addPolygons(data=ch, fill=FALSE, weight=3, color="black")
      } else if (input$type_input == "Canton") {
        leafletProxy("map") %>%
          # remove everything
          clearShapes() %>%
          # re-render
          addPolygons(data=cantons, fill=FALSE, weight=3, color="black")
      } else if (input$type_input == "Municipality") {
        leafletProxy("map") %>%
          # remove everything
          clearShapes() %>%
          # re-render
          addPolygons(data=muns, fill=FALSE, weight=3, color="black")
      } else if (input$type_input == "Municipality type") {
        leafletProxy("map") %>%
          # remove everything
          clearShapes() %>%
          # re-render
          addPolygons(data=typ, fill=FALSE, weight=3, color="black")
      }
      # reset the map and graph
      to_vis_map <<- data.frame()
      to_vis_graph <<- data.frame()
    }
  })
}

shinyApp(ui, server)
