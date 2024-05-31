library(shiny)
library(ggplot2)
library(tidyverse)
library(sf)
library(tmap)
library(leaflet)

# Load Data
women_data <- read.csv("/Users/Hanuman zalte/OneDrive/Desktop/Demo_dashb/women's food consumption_ 2019-21.csv")
men_data <- read.csv("/Users/Hanuman zalte/OneDrive/Desktop/Demo_dashb/men's food consumption_2019-21.csv")

print(head(women_data))
print(head(men_data))

women_data$STNAME <- as.character(women_data$STNAME)
men_data$STNAME <- as.character(men_data$STNAME)

india_map <- st_read("/Users/Hanuman zalte/OneDrive/Desktop/Demo_dashb/INDIA_STATES.geojson")

print(head(india_map))

men_data_map <- left_join(india_map, men_data, by = c("STNAME_SH" = "STNAME"))
women_data_map <- left_join(india_map, women_data, by = c("STNAME_SH"="STNAME"))

print(head(men_data_map))
print(head(women_data_map))

# UI
ui <- fluidPage(
  titlePanel("India Food Consumption Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Food Component:", 
                  choices = c("Milk or curd" = "Milk_or_curd", "Pulses or beans" = "Pulses_or_beans",
                              "Dark green leafy vegetables" = "Dark_green_leafy_vegetables", "Fruits" = "Fruits",
                              "Eggs" = "Eggs", "Chicken or meat" = "Chicken_or_meat", 
                              "Fish chicken or meat" = "Fish_chicken_or_meat",
                              "Fried Foods" = "Fried_Foods", "Aerated drinks" = "Aerated_drinks")),
      selectInput("gender", "Women/Men:", choices = c("Men", "Women"))
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Server
server <- function(input, output, session) {
  observe({
    data <- if (input$gender == "Men") men_data_map else women_data_map
    
    data[[paste0(input$variable, "_cat")]] <- cut(data[[input$variable]], 
                                                  breaks = c(-Inf, 25, 75, Inf), 
                                                  labels = c("Below 25", "25-75", "Above 75"))
    
    pal <- colorFactor(palette = c("#a70000", "#f4b400", "#d06002"), domain = data[[paste0(input$variable, "_cat")]])
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s: %g%%",
      data$STNAME_SH, gsub("_", " ", input$variable), data[[input$variable]]
    ) %>% lapply(htmltools::HTML)
    
    output$map <- renderLeaflet({
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal(data[[paste0(input$variable, "_cat")]]),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 2,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        addLegend(pal = pal, values = data[[paste0(input$variable, "_cat")]], opacity = 0.7, title = NULL,
                  position = "topright")
    })
  })
}

shinyApp(ui = ui, server = server)
