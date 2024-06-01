library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(shinydashboard)
library(htmltools)
library(htmlwidgets)
library(reshape2)
library(rsconnect)
library(ggplot2)
library(plotly)

# Load the GeoJSON file for India states
india_states <- st_read("https://raw.githubusercontent.com/ShrutiReddy21/statedata/main/INDIA_STATES.geojson")


india_states1 <- st_read("https://raw.githubusercontent.com/ShrutiReddy21/statedata/main/INDIA_STATES.geojson")


# Load the CSV files for anemia prevalence data
anemia_data_women <- read.csv("https://raw.githubusercontent.com/ShrutiReddy21/anemia_data/main/women%20anemia%20prevalence_2019-21.csv")
anemia_data_men <- read.csv("https://raw.githubusercontent.com/ShrutiReddy21/anemia_data/main/men_anemia.csv")
anemia_data_children <- read.csv("https://raw.githubusercontent.com/ShrutiReddy21/anemia_data/main/children_anemia_2019-21.csv")

# Load India shapefile for first code
india_sf <- st_read("https://raw.githubusercontent.com/ShrutiReddy21/statedata/main/INDIA_STATES.geojson")

# Sample data: region-wise consumption data for first code
data <- read.csv("https://raw.githubusercontent.com/ShrutiReddy21/Calorie-intake-data/main/Daily%20Calorie%20intake%20region%20wise.csv")

# Load the CSV files for anemia prevalence data
iodine_data <- read.csv("https://raw.githubusercontent.com/ShrutiReddy21/Iodine_test/main/Copy%20of%20Iodized_salt(1).csv")

# Load Data
women_data <- read.csv("https://raw.githubusercontent.com/hanuzalte/Nutrition-Dashboard/main/women%27s%20food%20consumption_%202019-21.csv")
men_data <- read.csv("https://raw.githubusercontent.com/hanuzalte/Nutrition-Dashboard/main/men%27s%20food%20consumption_2019-21.csv")

print(head(women_data))
print(head(men_data))

women_data$STNAME <- as.character(women_data$STNAME)
men_data$STNAME <- as.character(men_data$STNAME)

india_map <- st_read("https://raw.githubusercontent.com/ShrutiReddy21/statedata/main/INDIA_STATES.geojson")

print(head(india_map))

men_data_map <- left_join(india_map, men_data, by = c("STNAME_SH" = "STNAME"))
women_data_map <- left_join(india_map, women_data, by = c("STNAME_SH"="STNAME"))

print(head(men_data_map))
print(head(women_data_map))

# Clean and select relevant columns
iodine_data_c <- iodine_data %>%
  filter(!is.na(Children_living_in_iodized_salt_households_below_5_years)) %>%
  select(STNAME, Children_living_in_iodized_salt_households_below_5_years)


yes_iodine_data <- iodine_data %>%
  filter(!is.na(Iodised_Salt)) %>%
  select(STNAME,Iodised_Salt)

not_iodine_data <- iodine_data %>%
  filter(!is.na(Non_Iodised_Salt)) %>%
  select(STNAME,Non_Iodised_Salt)

# Read data from file 
df <- read.csv("https://raw.githubusercontent.com//harshitahp//Nutrition-Prices//main//Copy%20of%20Nutrition_Prices(1).csv", row.names = 1)

# Convert row names to a column 
df$country <- rownames(df) 
rownames(df) <- NULL

# Define state-to-region mapping
state_to_region <- data.frame(
  state = c("Jammu & Kashmir", "Himachal Pradesh", "Punjab", "Uttarakhand", "Haryana", "Delhi", "Uttar Pradesh","Ladakh", 
            "Bihar", "Jharkhand", "Odisha", "West Bengal",
            "Assam", "Arunachal Pradesh", "Manipur", "Meghalaya", "Nagaland", "Tripura", "Sikkim",
            "Goa", "Gujarat", "Maharashtra", "Rajasthan",
            "Chhattisgarh", "Madhya Pradesh",
            "Andhra Pradesh", "Karnataka", "Kerala", "Tamil Nadu", "Telangana"),
  region = c(rep("North", 8),
             rep("East", 4),
             rep("North_East", 7),
             rep("West", 4),
             rep("Central", 2),
             rep("South", 5))
)

# Merge shapefile with state-to-region mapping
india_sf <- india_sf %>%
  left_join(state_to_region, by = c("STNAME_SH" = "state"))

# Merge with data
india_sf <- india_sf %>%
  left_join(data, by = c("region" = "Region"))


# Define a custom color palette
custom_colors <- c("#008080","#088F8F","#40B5AD", "#17becf","#5F9EA0","#9FE2BF","#ADD8E6","#96DED1")
# Define custom color function with explicit bins for first code
customColor1 <- colorBin(
  palette = c("#90cf8e","#d5b60a","#e67e00","#Dc4501", "#A70000","brown"),
  domain = india_sf$Total,
  na.color = "transparent"
)
# Clean and select relevant columns
anemia_data_women <- anemia_data_women %>%
  filter(!is.na(Any_Anaemia)) %>%
  select(STNAME, Any_Anaemia)

anemia_data_men <- anemia_data_men %>%
  filter(!is.na(Any_Anemia)) %>%
  select(STNAME, Any_Anemia)

anemia_data_children <- anemia_data_children %>%
  filter(!is.na(Any_Anemia)) %>%
  select(STNAME, Any_Anemia)

# UI
ui <- dashboardPage(
  dashboardHeader(title = tags$div(
    style = "white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
    "NUTRITION"),titleWidth = 450 ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(
        title = "Daily region wise per capita caloric intake comparison by food groups", 
        status = "primary", 
        solidHeader = TRUE, 
        width = 6,
        leafletOutput("mymap")
      ),
      box(title = "State-wise Anemia Prevalence in India (NFHS 2021)",
          status = "primary", 
          solidHeader = TRUE, 
          width = 6,
          tags$head(
            tags$style(
              HTML("
        body {
          font-family: 'Noto Sans', sans-serif;
        }
      ")
            )
          ),
          sidebarLayout(
            sidebarPanel(
              selectInput("Category", "Select Population Category:", choices = c("Women", "Men", "Children")),
              style = "background-color: #f8f9fa; padding: 20px;"
            ),
            mainPanel(
              leafletOutput("indiaMap")
            )
          )
      ),
      box( title = "India Food Consumption Map",
           status = "primary", 
           solidHeader = TRUE, 
           width = 6,
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
           )),box(title = "State-wise Iodine presence in children in India (NFHS 2021)",
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 6,
                  tags$head(
                    tags$style(
                      HTML("
        body {
          font-family: 'Noto Sans', sans-serif;
        }
      ")
                    )
                  ),
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("Category", "Select Household Category:", choices = c("Households with Iodized Salt", "Households without Iodized Salt", "Households with Children of Age 6-59 Months (With Iodized Salt)")),
                      style = "background-color: #f8f9fa; padding: 20px;"
                    ),
                    mainPanel(
                      leafletOutput("indiaMap1")
                    )
                  )),
      box(title = "Cost comparison of food groups between countries",
          status = "primary", 
          solidHeader = TRUE, 
          width = 6,
          sidebarLayout(
            sidebarPanel(
              selectInput("cost_type", "Select Cost Type:", 
                          choices = c("Cost of a Nutrient Adequate Diet" = "Cost_of_a.nutrient_adequate_diet",
                                      "Cost of Fruits" = "Cost_of_fruits",  
                                      "Cost of Vegetables" = "Cost_of_vegetables", 
                                      "Cost of Starchy Staples" = "Cost_of_starchy_staples",
                                      "Cost of Animal-Source Foods" = "Cost_of_animal.source_foods", 
                                      "Cost of Legumes, Nuts, and Seeds" = "Cost_of_legumes_nuts_and_seeds", 
                                      "Cost of Oils and Fats" = "Cost_of_oils_and_fats"))
            ),
            mainPanel(
              plotlyOutput("barPlot")
            )
          ))
    )
  )
)





# Server logic
server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet(india_sf) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~customColor1(Total),
        weight = 2,
        opacity = 1,
        color = 'white', 
        dashArray = '3', 
        fillOpacity = 0.7,
        label = ~lapply(paste0(
          "<strong>", region, "</strong><br/>",
          "Total: ", Total," Calories","<br/>",
          "<ul>",
          "<li>Added fats: ", Added_fats, "</li>",
          "<li>Animal source proteins: ", Animal_source_proteins, "</li>",
          "<li>Sweeteners: ", sweeteners, "</li>",
          "<li>Dairy Foods: ", Dairy_Foods, "</li>",
          "<li>Fruits: ", Fruits, "</li>",
          "<li>Legumes: ", Legumes, "</li>",
          "<li>Potato and Cassava: ", Potato_and_Cassava, "</li>",
          "<li>Processed Food: ", Processed_Food, "</li>",
          "<li>Spices: ", Spices, "</li>",
          "<li>Nuts: ", Nuts, "</li>",
          "<li>Vegetables: ", Vegetables, "</li>",
          "<li>Whole grains: ", Whole_grains, "</li>",
          "</ul>"
        ), HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "10px",
          direction = "auto",
          offset = c(0, 0),
          opacity = 1
        )
      )%>%
      addLegend(
        position = "topright",
        colors = c("#90cf8e", "#d5b60a","#e67e00","#dc4501","#a70000","brown"),
        labels = c("West", "North East","South","Central","East","North"),
        title = "REGIONS",
        opacity = 1)
  })
  
  # Reactive expression to get the selected category data
  anemia_data <- reactive({
    if (input$Category == "Women") {
      left_join(india_states, anemia_data_women, by = "STNAME")
    } else if (input$Category == "Men") {
      left_join(india_states, anemia_data_men, by = "STNAME")
    } else {
      left_join(india_states, anemia_data_children, by = "STNAME")
    }
  })
  
  # Log output to debug
  observe({
    print(head(anemia_data()))
  })
  
  # Reactive expression to get the appropriate color function
  customColor <- reactive({
    if (input$Category == "Women") {
      colorBin(
        palette = c("#90cf8e","#d5b60a","#e67e00","#Dc4501", "#A70000"),
        domain = anemia_data()$Any_Anaemia,
        bins = c(0, 30, 45, 60, 75, 100),
        na.color = "transparent"
      )
    } else if (input$Category == "Men") {
      colorBin(
        palette = c("#90cf8e","#d5b60a","#e67e00","#Dc4501", "#A70000"),
        domain = anemia_data()$Any_Anemia,
        bins = c(0, 30, 45, 60, 75, 100),
        na.color = "transparent"
      )
    } else {
      colorBin(
        palette = c("#90cf8e","#d5b60a","#e67e00","#Dc4501", "#A70000"),
        domain = anemia_data()$Any_Anemia,
        bins = c(0, 30, 45, 60, 75, 100),
        na.color = "transparent"
      )
    }
  })
  
  # Render the leaflet map
  output$indiaMap <- renderLeaflet({
    leaflet(st_as_sf(anemia_data())) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~customColor()(if(input$Category == "Women") anemia_data_women$Any_Anaemia else if (input$Category == "Men") anemia_data_men$Any_Anemia else anemia_data_children$Any_Anemia),
        weight = 1,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = '#666',
          dashArray = '',
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste0(STNAME, ": ", if(input$Category == "Women") anemia_data_women$Any_Anaemia else if (input$Category == "Men") anemia_data_men$Any_Anemia else anemia_data_children$Any_Anemia, "%"),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = customColor(),
        values = if (input$Category == "Women") anemia_data_women$Any_Anaemia else if (input$Category == "Men") anemia_data_men$Any_Anemia else anemia_data_children$Any_Anemia,
        opacity = 0.7,
        position = "topright"
      ) %>%
      fitBounds(
        lng1 = min(st_bbox(anemia_data())$xmin, na.rm = TRUE),
        lat1 = min(st_bbox(anemia_data())$ymin, na.rm = TRUE),
        lng2 = max(st_bbox(anemia_data())$xmax, na.rm = TRUE),
        lat2 = max(st_bbox(anemia_data())$ymax, na.rm = TRUE)
      ) # Fit the map to the bounds of India
  })
  
  # Reactive expression to get the selected category data
  iodine_data <- reactive({
    if (input$Category == "Households with Iodized Salt") {
      left_join(india_states1, yes_iodine_data, by = "STNAME")
    } else if (input$Category == "Households without Iodized Salt") {
      left_join(india_states1, not_iodine_data, by = "STNAME")
    } else {
      left_join(india_states1, iodine_data_c, by = "STNAME")
    }
  })
  
  # Log output to debug
  observe({
    print(head(iodine_data()))
  })
  
  # Reactive expression to get the appropriate color function
  customColor2 <- reactive({
    if (input$Category == "Households with Iodized Salt") {
      colorBin(
        palette = c("#A70000","#D06002","#D5b60a","#90cf8e" ),
        domain = iodine_data()$Iodised_Salt,
        bins = c(0, 80, 90, 95, 100),
        na.color = "transparent"
      )
    } else if (input$Category == "Households without Iodized Salt") {
      colorBin(
        palette = c("#90cf8e","#D5b60a","#D06002","#A70000"),
        domain = iodine_data()$Non_Iodised_Salt,
        bins = c(0, 5, 10, 20, 100),
        na.color = "transparent"
      )
    } else {
      colorBin(
        palette = c("#A70000","#D06002","#D5b60a","#90cf8e" ),
        domain = iodine_data()$Children_living_in_iodized_salt_households_below_5_years,
        bins = c(0, 80, 90, 95, 100),
        na.color = "transparent"
      )
    }
  })
  
  # Render the leaflet map
  output$indiaMap1 <- renderLeaflet({
    leaflet(st_as_sf(iodine_data())) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~customColor2()(if(input$Category == "Households with Iodized Salt") yes_iodine_data$Iodised_Salt else if (input$Category == "Households without Iodized Salt") not_iodine_data$Non_Iodised_Salt  else iodine_data_c$Children_living_in_iodized_salt_households_below_5_years),
        weight = 1,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = '#666',
          dashArray = '',
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste0(STNAME, ": ",if(input$Category == "Households with Iodized Salt") yes_iodine_data$Iodised_Salt else if (input$Category == "Households without Iodized Salt") not_iodine_data$Non_Iodised_Salt  else iodine_data_c$Children_living_in_iodized_salt_households_below_5_years, "%"),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = customColor2(),
        values = if(input$Category == "Households with Iodized Salt") yes_iodine_data$Iodised_Salt else if (input$Category == "Households without Iodized Salt") not_iodine_data$Non_Iodised_Salt  else iodine_data_c$Children_living_in_iodized_salt_households_below_5_years,
        opacity = 0.7,
        position = "topright"
      ) %>%
      fitBounds(
        lng1 = min(st_bbox(iodine_data())$xmin, na.rm = TRUE),
        lat1 = min(st_bbox(iodine_data())$ymin, na.rm = TRUE),
        lng2 = max(st_bbox(iodine_data())$xmax, na.rm = TRUE),
        lat2 = max(st_bbox(iodine_data())$ymax, na.rm = TRUE)
      ) # Fit the map to the bounds of India
  })
  
  observe({
    data <- if (input$gender == "Men") men_data_map else women_data_map
    
    data[[paste0(input$variable, "_cat")]] <- cut(data[[input$variable]], 
                                                  breaks = c(-Inf, 20, 40, 60, 80, Inf), 
                                                  labels = c("Below 20", "20-40","40-60", "60-80", "Above 80"))
    
    pal <- colorFactor(palette = c( "#ADFF2F","#90EE90", "#8FBC8B", "#228B22","#006400"  ), domain = data[[paste0(input$variable, "_cat")]])
    
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
  
  output$barPlot <- renderPlotly({
    selected_cost <- input$cost_type
    df_melted <- melt(df, id.vars = "country")
    df_filtered <- df_melted[df_melted$variable == selected_cost, ]
    
    p <- ggplot(df_filtered, aes(x = country, y = value, fill = country)) + 
      geom_bar(stat = "identity") + 
      labs(title = paste("", gsub("_", " ", selected_cost)), x = "Country", y = "Cost") + 
      scale_fill_manual(values = custom_colors) + 
      coord_flip() + # Apply custom colors
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels if there are many countries
    
    ggplotly(p, tooltip = "y")  # Add tooltip to show values on hover
  })
}

# Run the application
shinyApp(ui, server)