library(shiny)
library(scales)
library(ggplot2)
library(dplyr)
library(leaflet)
library(sf)
library(htmltools)
library(htmlwidgets)
library(leaflet.extras)

transf_fracs_to_percs <- function(n) {
  return(n * 100)
}

# Reading data and transforming to WGS84
muns <- st_transform(st_read("processed_data/municipalities/municipalities.shp"), crs=4326)
cantons <-  st_transform(st_read("processed_data/cantons/cantons.shp"), crs=4326)
typ <-  st_transform(st_read("processed_data/typology/typology.shp"), crs=4326)
ch <-  st_transform(st_read("processed_data/switzerland/switzerland.shp"), crs=4326)

zoom_lvls <- read.csv("processed_data/zoom_levels.csv")

# bins and color palette for the cantons
bins_cantons <- c(0, 0.03, 0.04, 0.05, 0.06, 0.07)
pal_cantons <- colorBin("YlOrRd", domain = cantons$gwh_tot/cantons$p_rf_fac, bins=bins_cantons)

# bins and color palette for the municipalities
bins_muns <- c(0, 0.025, 0.05, 0.1, 0.2, 0.4, 0.5)
pal_muns <- colorBin("YlOrRd", domain = muns$gwh_tot/muns$p_rf_fac, bins=bins_muns)

labels_cantons <- sprintf(
  "<strong>Canton %s</strong>
  <br/>Population: %d
  <br/>Installed power: %g kW
  <br/>Current energy output: %g GWh
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
        label = "Clear selection"
      ),
      # choosing municipality type to be rendered in the plot
      selectInput(
        inputId = "municipality_type",
        label = "Choose municipality types",
        choices = as.character(typ$name),
        multiple = T
      ),
      # choosing municipality type to be rendered in the plot
      selectInput(
        inputId = "canton",
        label = "Choose cantons",
        choices = as.character(cantons$name),
        selected = "None",
        multiple = T
      )
    ),
    mainPanel(leafletOutput("map"), plotOutput("plot"))
  )
)

server <- function(input, output, session) {

  # variable storing the currently selected canton
  cur_canton <<- 0
  # variable indicating whether municipalities are currently shown
  muns_shown <<- F
  # variable storing the municipalities that are currently shown
  muns_shown_geom <<- data.frame()
  
  # variable indicating whether the selection is currently in progress
  selection_in_progress <- F
  
  # Rendering the initial plot that is displayed when the application is launched (Switzerland)
  viz <- ch %>%
    select(starts_with("gwh") &! ends_with("tot")) %>%
    st_drop_geometry()
  
  potential <- st_drop_geometry(ch)[1, "p_rf_fac"]
  installed <- cumsum(c(viz[1,]))
  names(installed) <- c()
  graph_ch <<- data.frame(years = years, installed = installed, name="Switzerland", potential=potential)
  # dataframe storing the currently selected municipalities, cantons and municipality types
  graph_muns <<- data.frame()
  graph_cantons <<- data.frame()
  graph_typ <<- data.frame()

  output$plot <- renderPlot({
    ggplot(graph_ch) +
      geom_line(mapping=aes(x = years, y =installed/potential, col=name)) +
      labs(title = "Exhaustion of solar energy potential", x="", y="Exhaustion [%]", col="") +
      scale_y_continuous(labels= percent) +
      theme_classic()
  })
  
  # dataframes for multiple selection
  #to_vis_graph <<- graph_ch
  to_vis_map <<- data.frame()
  
  # Other good options for basemap
  # - Esri.WorldShadedRelief --> No borders, just topography and lakes 
  # - CartoDB.PositronNoLabels
  output$map <- renderLeaflet({
    leaflet(options=leafletOptions(zoomControl = F,
                                   zoomSnap = 0.1,
                                   zoomDelta = 1,
                                   minZoom = 7.5)) %>%
      #addProviderTiles(providers$CartoDB.PositronNoLabels,
      #                 options = providerTileOptions(noWrap = TRUE)
      #) %>%
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
      setView(lng=8.227481, lat=46.80137, zoom=7.5) %>%
      setMaxBounds(lng1=5.861533, lat1=45.72593, lng2=10.60032, lat2=47.85311) %>%
      addLegend(pal = pal_cantons, 
                values = cantons$gwh_tot/cantons$p_rf_fac, 
                opacity = 1, 
                title="Exhaustion of PV potential", 
                position="bottomright",
                labFormat = labelFormat(
                  suffix = " %",
                  transform = transf_fracs_to_percs
                )) %>%
      addSearchOSM(options = searchOptions(collapsed = T, 
                                           autoCollapse=T, 
                                           hideMarkerOnCollapse = T, 
                                           tooltipLimit = 5,
                                           zoom=11,
                                           firstTipSubmit = T)) %>%
      onRender("function(el, x) {$(el).css('background-color', 'white');}")
  })
  
  # Figuring out where the user clicks and updating the plot accordingly
  observe({
    if (!is.null(input$map_click)) {
      point <- data.frame(x = input$map_click$lng, 
                            y = input$map_click$lat)
        
      if (muns_shown) {
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
          names(installed) <- c()
              
          if (selection_in_progress) {
            # add to existing
            graph_muns <<- rbind(graph_muns, data.frame(years = years, installed = installed, name=selected_df$name[1], potential=potential))
            to_vis_map <<- rbind(to_vis_map, selected_df)
          } else {
              # replace
            graph_muns <<- data.frame(years = years, installed = installed, name=selected_df$name[1], potential=potential)
            to_vis_map <<- selected_df
          }
    
          # display the clicked municipalities on the map in red
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
                
          to_vis_graph <<- rbind(graph_muns, graph_typ, graph_cantons, graph_ch)
          
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
            <br/>Current energy output: %g GWh
            <br/>Potential: %g GWh
            <br/>Potential exhausted: %g%%",
            muns_shown_geom$name, 
            round(muns_shown_geom$pop,0),
            round(muns_shown_geom$kw_tot,0),
            round(muns_shown_geom$gwh_tot,1),
            round(muns_shown_geom$p_rf_fac,1),
            round(muns_shown_geom$gwh_tot / muns_shown_geom$p_rf_fac * 100,1)
          ) %>% lapply(htmltools::HTML)
              
          leafletProxy("map") %>%
            # set view and zoom level to selected canton
            setView(lng=selected_lng[1,1], 
                    lat=selected_lat[1,1], 
                    zoom=selected_zoom[1,1]) %>%
            # removing previously displayed municipalities and cantons
            clearGroup("current_municipalities") %>%
            clearGroup("base_cantons") %>%
            # Remove the legend for the cantons
            clearControls() %>%
            # re-render cantons with smaller opacity
            addPolygons(data=cantons, 
                        group = "base_cantons",
                        weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillColor = ~pal_cantons(cantons$gwh_tot/cantons$p_rf_fac),
                        fillOpacity = 0.4) %>%
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
                        labelOptions = labelOptions(direction = "auto")) %>%
                        addLegend(pal = pal_muns, 
                          values = muns_shown_geom$gwh_tot/muns_shown_geom$p_rf_fac, 
                          opacity = 1, 
                          title="Exhaustion of PV potential", 
                          position="topright",
                          labFormat = labelFormat(
                            suffix = " %",
                            transform = transf_fracs_to_percs
                        ))
        }
      }
    }  
  })
  
  # Functionality of the municipality type button
  observe({
    if (length(input$municipality_type)) {
      
      # extracting selected types
      viz <- filter(typ, name %in% input$municipality_type) %>%
        select(starts_with("gwh") &! ends_with("tot")) %>%
        st_drop_geometry()
      
      potentials <- filter(typ, name %in% input$municipality_type) %>% 
        st_drop_geometry() %>%
        pull(p_rf_fac)
      
      # pivoting the dataframe
      installed <- c()
      potential <- c()
      name <- c()
      
      for (i in 1:length(input$municipality_type)) {
        potential <- append(potential, rep(potentials[i], length(years)))
        installed <- append(installed, cumsum(c(viz[i,])))
        names(installed) <- c()
        
        name <- append(name, rep(input$municipality_type[i], length(years)))
      }
      
      graph_typ <<- data.frame(years = years, 
                               installed = installed, 
                               name = name, 
                               potential = potential)
      
      to_vis_graph <<- rbind(graph_muns, graph_typ, graph_cantons, graph_ch)
      
      # re-render plot with variables in to_vis_graph
      output$plot <- renderPlot({
        ggplot(to_vis_graph) +
          geom_line(mapping=aes(x = years, y =installed/potential, col=name)) +
          labs(title = "Exhaustion of solar energy potential", x="", y="Exhaustion [%]", col="") +
          scale_y_continuous(labels= percent) +
          theme_classic()
      })
    } else {
      graph_typ <<- data.frame()
      
      to_vis_graph <<- rbind(graph_muns, graph_typ, graph_cantons, graph_ch)
      
      # re-render plot with variables in to_vis_graph
      output$plot <- renderPlot({
        ggplot(to_vis_graph) +
          geom_line(mapping=aes(x = years, y =installed/potential, col=name)) +
          labs(title = "Exhaustion of solar energy potential", x="", y="Exhaustion [%]", col="") +
          scale_y_continuous(labels= percent) +
          theme_classic()
      })
    }
  })
  
  # Functionality of the canton button
  observe({
    if (length(input$canton)) {
      
      # extracting selected types
      viz <- filter(cantons, name %in% input$canton) %>%
        select(starts_with("gwh") &! ends_with("tot")) %>%
        st_drop_geometry()
      
      potentials <- filter(cantons, name %in% input$canton) %>% 
        st_drop_geometry() %>%
        pull(p_rf_fac)
      
      # pivoting the dataframe
      installed <- c()
      potential <- c()
      name <- c()
      
      for (i in 1:length(input$canton)) {
        potential <- append(potential, rep(potentials[i], length(years)))
        installed <- append(installed, cumsum(c(viz[i,])))
        names(installed) <- c()
        
        name <- append(name, rep(paste("Canton", input$canton[i]), length(years)))
      }
      
      graph_cantons <<- data.frame(years = years, 
                                  installed = installed, 
                                  name = name, 
                                  potential = potential)
      
      to_vis_graph <<- rbind(graph_muns, graph_typ, graph_cantons, graph_ch)
      
      # re-render plot with variables in to_vis_graph
      output$plot <- renderPlot({
        ggplot(to_vis_graph) +
          geom_line(mapping=aes(x = years, y =installed/potential, col=name)) +
          labs(title = "Exhaustion of solar energy potential", x="", y="Exhaustion [%]", col="") +
          scale_y_continuous(labels= percent) +
          theme_classic()
      })
    } else {
      graph_cantons <<- data.frame()
      
      to_vis_graph <<- rbind(graph_muns, graph_typ, graph_cantons, graph_ch)
      
      # re-render plot with variables in to_vis_graph
      output$plot <- renderPlot({
        ggplot(to_vis_graph) +
          geom_line(mapping=aes(x = years, y =installed/potential, col=name)) +
          labs(title = "Exhaustion of solar energy potential", x="", y="Exhaustion [%]", col="") +
          scale_y_continuous(labels= percent) +
          theme_classic()
      })
    }
  })
  
  # Logic of the reset button
  observe({
    if (input$reset){
      # reset graph to Switzerland
      leafletProxy("map") %>%
        # remove everything
        clearGroup("current_selection")
      # reset the map
      to_vis_map <<- data.frame()
      graph_muns <<- data.frame()
      graph_cantons <<- data.frame()
      graph_typ <<- data.frame()
      
      to_vis_graph <<- rbind(graph_muns, graph_typ, graph_cantons, graph_ch)
      
      # re-render plot with variables in to_vis_graph
      output$plot <- renderPlot({
        ggplot(to_vis_graph) +
          geom_line(mapping=aes(x = years, y =installed/potential, col=name)) +
          labs(title = "Exhaustion of solar energy potential", x="", y="Exhaustion [%]", col="") +
          scale_y_continuous(labels= percent) +
          theme_classic()
      })
      
      # reset municipality and canton buttons
      updateSelectInput(session,
                        inputId = "municipality_type",
                        selected = character(0))  
      updateSelectInput(session,
                        inputId = "canton",
                        selected = character(0))  
    }
  })
  
  # Logic of the back button
  observe({
    if (input$back){
      # reset graph to Switzerland
      leafletProxy("map") %>%
        # remove currently selected municipalities
        clearGroup("current_municipalities") %>%
        # remove legend of municipalities
        clearControls() %>%
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
        setView(lng=8.227481, lat=46.80137, zoom=7.5) %>%
        addLegend(pal = pal_cantons, 
                  values = cantons$gwh_tot/cantons$p_rf_fac, 
                  opacity = 1, 
                  title="Exhaustion of PV potential", 
                  position="bottomright",
                  labFormat = labelFormat(
                    suffix = " %",
                    transform = transf_fracs_to_percs
                  ))
      
      # reset graph to display Switzerland
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
