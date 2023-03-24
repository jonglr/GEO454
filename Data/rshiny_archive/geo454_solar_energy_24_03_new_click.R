library(shiny)
library(scales)
library(ggplot2)
library(dplyr)
library(leaflet)
library(sf)
library(htmltools)

# Reading data and transforming to WGS84
muns <- st_transform(st_read("processed_data/municipalities/municipalities.shp"), crs=4326)
cantons <-  st_transform(st_read("processed_data/cantons/cantons.shp"), crs=4326)
typ <-  st_transform(st_read("processed_data/typology/typology.shp"), crs=4326)
ch <-  st_transform(st_read("processed_data/switzerland/switzerland.shp"), crs=4326)

zoom_lvls <- read.csv("processed_data/zoom_levels.csv")

# bins and color palette for the cantons
#bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal_cantons <- colorBin("YlOrRd", domain = cantons$gwh_tot/cantons$p_rf_fac)

# TODO: Define color scales for municipalities with classes here
# bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
# pal_muns <- colorBin("YlOrRd", domain = muns$gwh_tot/muns$p_rf_fac)

labels_cantons <- sprintf(
  "<strong>Canton %s</strong>
  <br/>Population: %d
  <br/>Installed power: %g kW
  <br/>Installed energy: %g GWh
  <br/>Potential: %g GWh
  <br/>Potential exhausted: %g%%",
  cantons$name, 
  round(cantons$pop,0),
  round(cantons$kw_tot,0),
  round(cantons$gwh_tot,1),
  round(cantons$p_rf_fac,1),
  round(cantons$gwh_tot / cantons$p_rf_fac * 100,1)
) %>% lapply(htmltools::HTML)

years <- seq(2004, 2022)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      actionButton(
        inputId = "back",
        label = "Back"
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
  
  # variable storing the currently selected canton
  cur_canton <<- 0
  # variable indicating whether municipalities are currently shown
  muns_shown <<- F
  # variable storing the municipalities that are currently shown
  muns_shown_geom <<- data.frame()
  
  # variable indicating whether the selection is currently in progress
  selection_in_progress <- F
  
  # Rendering the initial plot that is displayed when the application is launched
  viz <- ch %>%
    select(starts_with("gwh") &! ends_with("tot")) %>%
    st_drop_geometry()
  
  potential <- st_drop_geometry(ch)[1, "p_rf_fac"]
  installed <- cumsum(c(viz[1,]))
  graph_ch <<- data.frame(years = years, installed = installed, name="Switzerland", potential=potential)

  output$plot <- renderPlot({
    ggplot(graph_ch) +
      geom_line(mapping=aes(x = years, y =installed/potential, col=name)) +
      labs(title = "Exhaustion of solar energy potential", x="", y="Exhaustion [%]", col="") +
      scale_y_continuous(labels= percent) +
      theme_classic()
  })
  
  # dataframes for multiple selection
  to_vis_graph <<- data.frame()
  to_vis_map <<- data.frame()
  
  # Other good options for basemap
  # - Esri.WorldShadedRelief --> No borders, just topography and lakes 
  # - CartoDB.PositronNoLabels
  output$map <- renderLeaflet({
    leaflet(options=leafletOptions(zoomControl = F,
                                   zoomSnap = 0.1,
                                   zoomDelta = 1)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addPolygons(data=cantons, 
                  group = "base_cantons",
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillColor = ~pal_cantons(cantons$gwh_tot/cantons$p_rf_fac),
                  fillOpacity=1,
                  highlightOptions = highlightOptions(
                    weight = 2,
                    color = "black",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = T
                  ),
                  label=labels_cantons,
                  labelOptions = labelOptions(direction = "auto")) %>%
      setView(lng=8.227481, lat=46.80137, zoom=7.5)
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
        
      if (new_click & muns_shown) {
        # generating point object
        click <- st_as_sf(point, coords = c("x", "y"), crs=4326, agr="constant")
          
        # check whether click is within currently selected municipalities
        within_ch <- any(st_intersects(click, muns_shown_geom, sparse=F))

        if (within_ch) {
          # getting geometry of where user clicked
          selected_df <- muns_shown_geom[click,, op=st_contains]
              
          # subsetting the dataframe
          viz <- filter(muns_shown_geom, name == selected_df$name[1]) %>%
            select(starts_with("gwh") &! ends_with("tot")) %>%
            st_drop_geometry()
            
          potential <- st_drop_geometry(selected_df)[1, "p_rf_fac"]
          installed <- cumsum(c(viz[1,]))
              
          if (selection_in_progress) {
            # add to existing
            to_vis_graph <<- rbind(to_vis_graph, data.frame(years = years, installed = installed, name=selected_df$name[1], potential=potential))
            to_vis_map <<- rbind(to_vis_map, selected_df)
          } else {
              # replace
            to_vis_graph <<- rbind(graph_ch, data.frame(years = years, installed = installed, name=selected_df$name[1], potential=potential))
            to_vis_map <<- selected_df
          }
    
            # display the clicked layers on the map in red
          leafletProxy("map") %>%
            clearGroup("current_selection") %>%
            # add currently selected layer as red polygon as indication
            addPolygons(data=to_vis_map, 
                        fill=FALSE, 
                        weight=5, 
                        color="black", 
                        group="current_selection",
                        highlightOptions = highlightOptions(weight=5,
                                                            color="black",
                                                            bringToFront = T))
                
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
  
  # observer that zooms to the clicked canton and displays all the municipalities in the chosen canton
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
        
        within_ch <- any(st_intersects(click, cantons, sparse=F))
        # check whether click is within Switzerland
        if (within_ch) {
          # zoom to canton and display municipalities
          # find out number of canton that was clicked
          canton <- cantons[click,,]
          canton_nr <- canton$KANTONSNUM[1]
          
          if (canton_nr != cur_canton) {
            cur_canton <<- canton_nr  
            muns_shown <<- T
            
            # getting longitude, latitude and zoom_level of currently selected canton
            selected_lng <- zoom_lvls %>% filter(KANTONSNUM == canton_nr) %>% select(lng)
            selected_lat <- zoom_lvls %>% filter(KANTONSNUM == canton_nr) %>% select(lat)
            selected_zoom <- zoom_lvls %>% filter(KANTONSNUM == canton_nr) %>% select(zoom_level)
              
            # all municipalities of the currently selected canton
            muns_shown_geom <<- muns %>% filter(knr_x == canton_nr)
            
            labels_muns <- sprintf(
              "<strong>%s</strong>
              <br/>Population: %d
              <br/>Installed power: %g kW
              <br/>Installed energy: %g GWh
              <br/>Potential: %g GWh
              <br/>Potential exhausted: %g%%",
              muns_shown_geom$name, 
              round(muns_shown_geom$pop,0),
              round(muns_shown_geom$kw_tot,0),
              round(muns_shown_geom$gwh_tot,1),
              round(muns_shown_geom$p_rf_fac,1),
              round(muns_shown_geom$gwh_tot / muns_shown_geom$p_rf_fac * 100,1)
            ) %>% lapply(htmltools::HTML)
            
            # bins and color palette for the current municipalities
            #bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
            pal_muns <- colorBin("YlOrRd", domain = muns_shown_geom$gwh_tot/muns_shown_geom$p_rf_fac)
              
            leafletProxy("map") %>%
              # set view and zoom level to selected canton
              setView(lng=selected_lng[1,1], 
                      lat=selected_lat[1,1], 
                      zoom=selected_zoom[1,1]) %>%
              # removing previously displayed municipalities and cantons
              clearGroup("current_municipalities") %>%
              clearGroup("base_cantons") %>%
              # re-render cantons with smaller opacity
              addPolygons(data=cantons, 
                          group = "base_cantons",
                          weight = 2,
                          opacity = 1,
                          color = "white",
                          dashArray = "3",
                          fillColor = ~pal_cantons(cantons$gwh_tot/cantons$p_rf_fac),
                          fillOpacity = 0.25) %>%
              # re-render municipalities
              addPolygons(data=muns_shown_geom, 
                          group="current_municipalities", 
                          weight = 2, 
                          opacity = 1,
                          color = "white",
                          dashArray = "3",
                          fillColor = ~pal_muns(muns_shown_geom$gwh_tot/muns_shown_geom$p_rf_fac), 
                          fillOpacity = 1,
                          highlightOptions = highlightOptions(
                            weight = 2,
                            color = "black",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = T
                          ),
                          label=labels_muns,
                          labelOptions = labelOptions(direction = "auto"))
          }
        }
      }
    }  
  })
  
  # Logic of the reset button
  observe({
    if (input$reset){
      # reset graph to Switzerland
      leafletProxy("map") %>%
        # remove everything
        clearGroup("current_selection")
      # reset the map and graph
      to_vis_map <<- data.frame()
      to_vis_graph <<- graph_ch
    }
  })
  
  # Logic of the back button
  observe({
    if (input$back){
      # reset graph to Switzerland
      leafletProxy("map") %>%
        # remove currently selected municipalities
        clearGroup("current_municipalities") %>%
        addPolygons(data=cantons, 
                    group = "base_cantons",
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillColor = ~pal_cantons(cantons$gwh_tot/cantons$p_rf_fac),
                    fillOpacity=1,
                    highlightOptions = highlightOptions(
                      weight = 2,
                      color = "black",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = T
                    ), 
                    label=labels_cantons,
                    labelOptions = labelOptions(direction = "auto")) %>%
        # reset view to show all cantons
        setView(lng=8.227481, lat=46.80137, zoom=7.5)
      
      # reset graph to display Switzerland
      #to_vis_graph <<- graph_ch
      muns_shown <<- F
      cur_canton <<- 0
    }
  })
  
  # Logic of the multiple selection button
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
}

shinyApp(ui, server)
