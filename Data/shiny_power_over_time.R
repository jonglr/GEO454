library(shiny)
library(scales)
library(ggplot2)
library(dplyr)

# Loading the data
muns <- read.csv("processed_data/municipalities.csv")
cantons <- read.csv("processed_data/cantons.csv")
typ <- read.csv("processed_data/typology.csv")
ch <- read.csv("processed_data/switzerland.csv")

years <- seq(2004, 2022)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "type_input",
        label = "Choose an administrative unit",
        choices = list("Switzerland", "Canton", "Municipality type", "Municipality")
      ),
      selectInput(
        inputId = "name_input",
        label = "Choose a name",
        choices = list("Switzerland")
      )
    ),
    mainPanel(plotOutput("plot"))
  )
)

server <- function(input, output, session) {
  # Defining the values of the second slider conditional on the first one
  observe({
    if (input$type_input == "Canton") {
      updateSelectInput(session,
                        inputId = "name_input",
                        choices = as.character(cantons$name))
    } else if (input$type_input == "Municipality type") {
      updateSelectInput(session,
                        inputId = "name_input",
                        choices = as.character(typ$name))      
    } else if (input$type_input == "Municipality") {
      updateSelectizeInput(session,
                        inputId = "name_input",
                        choices = sort(as.character(muns$name)))      
    } else if (input$type_input == "Switzerland") {
      updateSelectInput(session,
                        inputId = "name_input",
                        choices = list("Switzerland"))      
    }
  })
  
  # Update the plot 
  observe({
    if (input$type_input == "Switzerland") {
      viz <- ch %>%
        select(starts_with("installed_kw") &! ends_with("total"))
    } else if (input$type_input == "Canton") {
      viz <- filter(cantons, name == input$name_input) %>%
        select(starts_with("installed_kw") &! ends_with("total"))
    } else if (input$type_input == "Municipality type") {
      viz <- filter(typ, name == input$name_input) %>%
        select(starts_with("installed_kw") &! ends_with("total"))  
    } else if (input$type_input == "Municipality") {
      viz <- filter(muns, name == input$name_input) %>%
        select(starts_with("installed_kw") &! ends_with("total"))    
    }
    
    power <- cumsum(c(viz[1,]))
    to_visualize <- data.frame(years = years, power = power)
    
    if (input$type_input == "Municipality" | input$type_input == "Canton") {
      title = paste("Power of photovoltaic plants in", input$type_input, input$name_input)
    } else {
      title = paste("Power of photovoltaic plants in", input$name_input)
    }
    
    output$plot <- renderPlot({
      ggplot(to_visualize) +
      geom_line(mapping=aes(x = years, y =power)) +
      labs(title = title, x="", y="Total operating power [kW]") +
      scale_y_continuous(labels= comma) +
      theme_classic()
    })
  })
}

shinyApp(ui, server)