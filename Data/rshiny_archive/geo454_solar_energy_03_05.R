library(shiny)
library(scales)
library(ggplot2)
library(dplyr)
library(leaflet)
library(sf)
library(htmltools)
library(htmlwidgets)
library(leaflet.extras)
library(shinythemes)
library(RColorBrewer)
library(shinyWidgets)

# Function that renders the plot with the evolution of the potential
returnPlot <- function(dataframe) {
  returnedPlot <- ggplot(dataframe) +
    geom_line(mapping=aes(x = years, y =installed/potential, col=name), size=1.5) +
    labs(title = "Development of the exhaustion of solar energy potential", x="", y="Exhaustion [%]", col="") +
    scale_y_continuous(labels= percent_format(accuracy = 1, scale = 100)) +
    theme_classic() +
    theme(legend.position = "bottom", plot.title = element_text(face="bold"), text=element_text(family="Roboto"))
  
  return(returnedPlot)
}

# Function that renders the plot with the political orientation
returnPolPlot <- function() {
  returnedPlot <- ggplot() +
    geom_point(data=muns, mapping=aes(x=pol_or, y = gwh_tot / p_rf_fac, col="Swiss municipalities"), size=0.2) +
    labs(title = "Political orientation vs exhaustion of PV potential at municipality level", x="Political orientation", y="Exhaustion [%]", col="") +
    scale_y_continuous(labels= percent_format(accuracy = 1, scale = 100)) +
    scale_x_continuous(breaks=c(-1, 0, 1),
                       labels=c("left-leaning", "centrist", "right-leaning")) +
    scale_color_manual(values="black") +
    theme_classic() +
    theme(legend.position = "bottom", plot.title = element_text(face="bold"), text=element_text(family="Roboto"))
  
  return(returnedPlot)
}

# Function that renders the plot with the home ownership rate
returnHomePlot <- function() {
  returnedPlot <- ggplot() +
    geom_point(data=muns, mapping=aes(x=own_frac, y = gwh_tot / p_rf_fac, col="Swiss municipalities"), size=0.2) +
    labs(title = "Home ownership rate vs exhaustion of PV potential at municipality level", x="Home ownership rate [%]", y="Exhaustion [%]", col="") +
    scale_y_continuous(labels= percent_format(accuracy = 1, scale = 100)) +
    scale_x_continuous(labels= percent_format(accuracy = 1, scale = 100)) +
    scale_color_manual(values="black") +
    theme_classic() +
    theme(legend.position = "bottom", plot.title = element_text(face="bold"), text=element_text(family="Roboto"))
  
  return(returnedPlot)
}

# size of the map, can be 400, 600 or 800
size <- 800

# Reading data and transforming to WGS84
muns <- st_read("processed_data/municipalities/municipalities.shp")
cantons <-  st_read("processed_data/cantons/cantons.shp")
cantons_simp <-  st_read("processed_data/cantons/cantons_simp.shp")
typ <-  st_read("processed_data/typology/typology.shp")
ch <-  st_read("processed_data/switzerland/switzerland.shp")
ch_simp <-  st_read("processed_data/switzerland/switzerland_simp.shp")
lakes <-  st_read("processed_data/lakes/lakes.shp")

zoom_lvls <- read.csv(paste0("processed_data/zoom_levels/zoom_levels_",size,".csv"))
energy_perspectives <- read.csv("processed_data/energieperspektive_2050.csv")

# bins and color palette for the cantons
bins_cantons <- c(0, 0.03, 0.04, 0.05, 0.06, 0.07)
pal_cantons <- colorBin("YlOrRd", domain = cantons$gwh_tot/cantons$p_rf_fac, bins=bins_cantons)
pal_cantons_man <- brewer.pal(length(bins_cantons)-1, "YlOrRd")
legend_labels_cantons = c(paste("<", bins_cantons[2] * 100, "%"), 
                          paste("<", bins_cantons[3] * 100, "%"), 
                          paste("<", bins_cantons[4] * 100, "%"), 
                          paste("<", bins_cantons[5] * 100, "%"), 
                          paste("<", bins_cantons[6] * 100, "%"))

# bins and color palette for the municipalities
bins_muns <- c(0, 0.025, 0.05, 0.075, 0.1, 0.5)
pal_muns <- colorBin("YlOrRd", domain = muns$gwh_tot/muns$p_rf_fac, bins=bins_muns)
pal_muns_man <- brewer.pal(length(bins_muns)-1, "YlOrRd")
legend_labels_muns = c(paste("<", bins_muns[2] * 100, "%"), 
                       paste("<", bins_muns[3] * 100, "%"), 
                       paste("<", bins_muns[4] * 100, "%"), 
                       paste("<", bins_muns[5] * 100, "%"), 
                       paste("<", bins_muns[6] * 100, "%"))

labels_cantons <- sprintf(
  "<strong>Canton %s</strong>
  <br/>Population: %d
  <br/>Installed PV power: %g kW
  <br/>Current PV energy output: %g GWh
  <br/>PV Potential: %g GWh
  <br/>PV Potential exhausted: %g%%
  <br/>Political orientation: %s
  <br/>Home ownership rate: %g%%",
  cantons$name, 
  round(cantons$pop,0),
  round(cantons$kw_tot,0),
  round(cantons$gwh_tot,1),
  round(cantons$p_rf_fac,1),
  round(cantons$gwh_tot / cantons$p_rf_fac * 100,1),
  cantons$pol_or_cat,
  round(cantons$own_frac * 100, 1)
) %>% lapply(htmltools::HTML)

infobox <- HTML("<div style='text-align: justify;'><h3><strong>Exhaustion of solar energy potential in Switzerland</strong></h3><br/>
                The map shows the exhaustion of photovoltaic potential for every canton of Switzerland. A canton may be selected to reveal the municipalities
                within the canton. The municipalities can once again be selected, which adds them to the plot on the top right and allows for the comparison of the
                development of the exhaustion over time between different municipalities. For additional comparisons, the exhaustion of cantons and municipality 
                types may be added to the plot via the boxes in the bottom right panel. Elements may be removed from the plot through the clear selection button.
                <br/><br/>Additional information in the form of two socioeconomic variables may be displayed by selecting the respective radio button on the bottom 
                right panel, which produces a scatter plot where the municipalities of the currently selected canton are highlighted.
                </div>")

years <- seq(2004, 2022)

# height 400 --> 7.5, height 600 --> 8, height 800 --> 8.5
if (size == 400){
  defaultZoom <- 7.5
} else if (size == 600) {
  defaultZoom <- 8
} else if (size == 800) {
  defaultZoom <- 8.5
}

ui <- fluidPage(theme = shinytheme("sandstone"),
  fluidRow(
    # conditionalPanel with contents depending on the radio button
    column(5,
           conditionalPanel("input.add_info == 'def'", infobox),
           conditionalPanel("input.add_info == 'pol'", plotOutput("pol_or_plot")), 
           conditionalPanel("input.add_info == 'prop'", plotOutput("home_own_plot"))),
    column(5,plotOutput("plot")),
    column(2,
           #div(HTML("<b>Options</b>"), style="font-size: 20px;"),
           p(),
           actionButton(
             inputId = "reset",
             label = "Clear selection"
           ),
           p(),
           div(HTML("<b>Add to the plots</b>"), style="font-size: 15px;"),
           # choosing municipality type to be rendered in the plot
           pickerInput(
             inputId = "municipality_type",
             label = "Municipality types",
             choices = as.character(typ$name),
             options = list(
               `selected-text-format` = "count > 1"), 
             multiple = T
           ),
           # choosing municipality type to be rendered in the plot
           pickerInput(
             inputId = "canton",
             label = "Cantons",
             choices = as.character(cantons$name),
             options = list(
               `selected-text-format` = "count > 1"), 
             multiple = T
           ),
           radioButtons(inputId="add_info", label="Additional information",
                        choices = c("Instructions" = "def", 
                                    "Political orientation" = "pol", 
                                    "Home ownership" = "prop"), selected = "def")
    )
  ),
  fluidRow(
    column(12, leafletOutput("map", height=size))
  )
)

server <- function(input, output, session) {

  # variable storing the currently selected canton
  cur_canton <<- 0
  # variable indicating whether municipalities are currently shown
  muns_shown <<- F
  # variable storing the municipalities that are currently shown
  muns_shown_geom <<- data.frame()
  
  # variable that records whether the back button has been clicked
  back_button_clicked <<- F
  
  # Rendering the initial plot that is displayed when the application is launched (Switzerland)
  viz <- ch %>%
    select(starts_with("gwh") &! ends_with("tot")) %>%
    st_drop_geometry()
  
  potential <- st_drop_geometry(ch)[1, "p_rf_fac"]
  installed <- cumsum(c(viz[1,]))
  names(installed) <- c()
  
  graph_ch <<- data.frame(years = years, installed = installed, name="Switzerland", potential=potential)
  graph_ch <<- rbind(graph_ch, energy_perspectives)
  # dataframe storing the currently selected municipalities, cantons and municipality types
  graph_muns <<- data.frame()
  graph_cantons <<- data.frame()
  graph_typ <<- data.frame()

  output$plot <- renderPlot({returnPlot(graph_ch)})
  
  # displaying the political orientation base plot
  output$pol_or_plot <- renderPlot({returnPolPlot()})
  output$home_own_plot <- renderPlot({returnHomePlot()})
  
  # dataframes for multiple selection
  to_vis_map <<- data.frame()
  
  renderBaseCantons <- function(){
    # Other good options for basemap
    # - Esri.WorldShadedRelief --> No borders, just topography and lakes 
    # - CartoDB.PositronNoLabels
    return (renderLeaflet({
      leaflet(options=leafletOptions(zoomControl = T,
                                     zoomSnap = 0.1,
                                     zoomDelta = 1,
                                     minZoom = defaultZoom)) %>%
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
                      bringToFront = T,
                      sendToBack = T
                    ),
                    label=labels_cantons,
                    labelOptions = labelOptions(direction = "auto")) %>%
        setView(lng=8.227481, lat=46.80137, zoom=defaultZoom) %>%
        setMaxBounds(lng1=5.861533-0.5, lat1=45.72593-0.5, lng2=10.60032+0.5, lat2=47.85311+0.5) %>%
        addLegend(colors = pal_cantons_man, 
                  labels = legend_labels_cantons,
                  opacity = 1, 
                  title=HTML("Exhaustion of<br>PV potential"), 
                  position="topright"
                  ) %>%
        addSearchOSM(options = searchOptions(collapsed = T, 
                                             autoCollapse=T, 
                                             hideMarkerOnCollapse = T, 
                                             tooltipLimit = 5,
                                             zoom=11,
                                             firstTipSubmit = T)) %>%
        onRender("function(el, x) {$(el).css('background-color', 'white');}") %>%
        addPolygons(data=ch_simp,
                    group="ch",
                    fill=F,
                    weight=3,
                    opacity=1,
                    color="black",
                    highlightOptions= highlightOptions(sendToBack=F)) %>%
        addPolygons(data=lakes, 
                    group = "lakes",
                    fillColor="lightblue",
                    stroke = F,
                    fillOpacity=1)
    }))
  }
  
  output$map <- renderBaseCantons()
  
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

          # add to existing
          graph_muns <<- rbind(graph_muns, data.frame(years = years, installed = installed, name=selected_df$name[1], potential=potential))
          to_vis_map <<- rbind(to_vis_map, selected_df)
    
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
                                                            bringToFront = T,
                                                            sendToBack = F))
                
          to_vis_graph <<- rbind(graph_muns, graph_typ, graph_cantons, graph_ch)
          
          # update the plot
          output$plot <- renderPlot({returnPlot(to_vis_graph)})
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
      if (!(back_button_clicked)) {
        click <- st_as_sf(point, coords = c("x", "y"), crs=4326, agr="constant")
      }  else {
        # set click to (0, 0) if back button has been clicked just before
        click <- st_as_sf(data.frame(x = 0, y = 0), coords=c("x", "y"), crs=4326, agr="constant")
      }
      
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
          
          # currently clicked canton
          canton_shown_geom <- cantons_simp %>% filter(knr_x == canton_nr)
            
          labels_muns <- sprintf(
            "<strong>%s</strong>
            <br/>Population: %d
            <br/>Installed PV power: %g kW
            <br/>Current PV energy output: %g GWh
            <br/>PV Potential: %g GWh
            <br/>PV Potential exhausted: %g%%
            <br/>Political orientation: %s
            <br/>Home ownership rate: %g%%",
            muns_shown_geom$name, 
            round(muns_shown_geom$pop,0),
            round(muns_shown_geom$kw_tot,0),
            round(muns_shown_geom$gwh_tot,1),
            round(muns_shown_geom$p_rf_fac,1),
            round(muns_shown_geom$gwh_tot / muns_shown_geom$p_rf_fac * 100,1),
            muns_shown_geom$pol_or_cat,
            round(muns_shown_geom$own_frac * 100, 1)
          ) %>% lapply(htmltools::HTML)
              
          leafletProxy("map") %>%
            # set view and zoom level to selected canton
            setView(lng=selected_lng[1,1], 
                    lat=selected_lat[1,1], 
                    zoom=selected_zoom[1,1]) %>%
            # removing previously displayed municipalities and cantons
            clearGroup("current_municipalities") %>%
            clearGroup("base_cantons") %>%
            clearGroup("lakes") %>%
            clearGroup("current_canton") %>%
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
            # render outline of currently clicked canton
            addPolygons(data = canton_shown_geom,
                        group = "current_canton",
                        fill = F,
                        weight=7,
                        opacity = 1,
                        color="black"
                        ) %>%
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
                          bringToFront = T,
                          sendToBack = F
                        ),
                        label=labels_muns,
                        labelOptions = labelOptions(direction = "auto")) %>%
                        addLegend(colors = pal_muns_man, 
                          labels = legend_labels_muns,
                          opacity = 1, 
                          title=HTML("Exhaustion of<br>PV potential"), 
                          position="topright"
                          ) %>%
            addControl(actionButton(inputId = "back", label="", icon = icon("house"), width="40px")) %>%
            addPolygons(data=lakes, 
                        group = "lakes",
                        fillColor="lightblue",
                        stroke = F,
                        fillOpacity=1)
          
          # updating the political orientation plot
          output$pol_or_plot <- renderPlot({
            returnPolPlot() +
              scale_color_manual(values=c("red", "black")) +
              geom_point(data=muns_shown_geom, 
                         mapping=aes(x=pol_or, 
                                     y = gwh_tot / p_rf_fac, 
                                     col=paste("Municipalities of the canton of", cantons$name[cur_canton])), size=2)
          })
          
          # updating the home ownership plot
          output$home_own_plot <- renderPlot({
            returnHomePlot() +
              scale_color_manual(values=c("red", "black")) +
              geom_point(data=muns_shown_geom, 
                         mapping=aes(x=own_frac, 
                                     y = gwh_tot / p_rf_fac, 
                                     col=paste("Municipalities of the canton of", cantons$name[cur_canton])), size=2)
          })
        }
      }
      back_button_clicked <<- F
    }  
  })
  
  # Functionality of the municipality type button
  observe({
    if (length(input$municipality_type)) {
      # pivoting the dataframe
      installed <- c()
      potential <- c()
      name <- c()
      
      for (i in 1:length(input$municipality_type)) {
        # extracting selected types
        viz <- filter(typ, name == input$municipality_type[i]) %>%
          select(starts_with("gwh") &! ends_with("tot")) %>%
          st_drop_geometry()
        
        potentials <- filter(typ, name == input$municipality_type[i]) %>% 
          st_drop_geometry() %>%
          pull(p_rf_fac)
        
        potential <- append(potential, rep(potentials, length(years)))
        installed <- append(installed, cumsum(c(viz[1,])))
        names(installed) <- c()
        
        name <- append(name, rep(input$municipality_type[i], length(years)))
      }
      
      graph_typ <<- data.frame(years = years, 
                               installed = installed, 
                               name = name, 
                               potential = potential)
      
      to_vis_graph <<- rbind(graph_muns, graph_typ, graph_cantons, graph_ch)
      
      # re-render plot with variables in to_vis_graph
      output$plot <- renderPlot({returnPlot(to_vis_graph)})
    } else {
      graph_typ <<- data.frame()
      
      to_vis_graph <<- rbind(graph_muns, graph_typ, graph_cantons, graph_ch)
      
      # re-render plot with variables in to_vis_graph
      output$plot <- renderPlot({returnPlot(to_vis_graph)})
    }
  })
  
  # Functionality of the canton button
  observe({
    if (length(input$canton)) {
      # pivoting the dataframe
      installed <- c()
      potential <- c()
      name <- c()
      
      for (i in 1:length(input$canton)) {
        # extracting selected cantons
        viz <- filter(cantons, name == input$canton[i]) %>%
          select(starts_with("gwh") &! ends_with("tot")) %>%
          st_drop_geometry()
        
        potentials <- filter(cantons, name == input$canton[i]) %>% 
          st_drop_geometry() %>%
          pull(p_rf_fac)
        
        potential <- append(potential, rep(potentials, length(years)))
        installed <- append(installed, cumsum(c(viz[1,])))
        names(installed) <- c()
        
        name <- append(name, rep(paste("Canton", input$canton[i]), length(years)))
      }
      
      graph_cantons <<- data.frame(years = years, 
                                  installed = installed, 
                                  name = name, 
                                  potential = potential)
      
      to_vis_graph <<- rbind(graph_muns, graph_typ, graph_cantons, graph_ch)
      
      # re-render plot with variables in to_vis_graph
      output$plot <- renderPlot({returnPlot(to_vis_graph)})
    } else {
      graph_cantons <<- data.frame()
      
      to_vis_graph <<- rbind(graph_muns, graph_typ, graph_cantons, graph_ch)
      
      # re-render plot with variables in to_vis_graph
      output$plot <- renderPlot({returnPlot(to_vis_graph)})
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
      output$plot <- renderPlot({returnPlot(to_vis_graph)})
      
      # reset municipality and canton buttons
      updateSelectInput(session,
                        inputId = "municipality_type",
                        selected = character(0))  
      updateSelectInput(session,
                        inputId = "canton",
                        selected = character(0))  
    }
  })
  
  observeEvent(input$back, {
    back_button_clicked <<- T

    # reset graph to Switzerland
    leafletProxy("map") %>%
      # remove currently selected municipalities
      clearGroup("current_municipalities") %>%
      clearGroup("lakes") %>%
      clearGroup("ch") %>%
      clearGroup("base_cantons") %>%
      clearGroup("current_canton") %>%
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
                    bringToFront = T,
                    sendToBack = T
                  ), 
                  label=labels_cantons,
                  labelOptions = labelOptions(direction = "auto")) %>%
      # reset view to show all cantons
      setView(lng=8.227481, lat=46.80137, zoom=defaultZoom) %>%
      addLegend(colors = pal_cantons_man, 
                labels = legend_labels_cantons, 
                opacity = 1, 
                title=HTML("Exhaustion of<br>PV potential"), 
                position="topright"
                ) %>%
      addPolygons(data=ch_simp,
                  group="ch",
                  fill=F,
                  weight=3,
                  opacity=1,
                  color="black",
                  highlightOptions= highlightOptions(sendToBack=F)) %>%
      addPolygons(data=lakes, 
                  group = "lakes",
                  fillColor="lightblue",
                  stroke = F,
                  fillOpacity=1)
    
    # reset graph to display Switzerland
    muns_shown <<- F
    cur_canton <<- 0
    
    # reset political orientation plot
    output$pol_or_plot <- renderPlot({returnPolPlot()})
    output$home_own_plot <- renderPlot({returnHomePlot()})
  })
}

shinyApp(ui, server)
