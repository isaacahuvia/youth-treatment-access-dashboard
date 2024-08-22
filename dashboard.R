## Load packages
library(here)
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(tigris)
library(scales)


## Load data
# US state shapefile
state_shapefile <- tigris::states(cb = TRUE) %>% # Load US state shapefile from tigris
  sf::st_transform(4326)  # Transform to WGS84 for use with Leaflet

# Load state data
state_data <- read.csv(here::here("state_data.csv"))

# Merge state data with shapefile
merged_data <- dplyr::inner_join(
  state_shapefile, 
  state_data, 
  by = "STUSPS"
)


## Define color palette
palette <- leaflet::colorFactor(
  c("#e64747", "#e09c3b", "#e6e22e"), 
  levels = c("Always Required", "Sometimes Required", "Never Required")
)


## UI
ui <- fluidPage(
  
  titlePanel("Parental Consent Requirements for Mental Health Treatment"),
  
  fluidRow(
  
    column(
      
      9,
      leafletOutput("map", height = "600px"),
      HTML("<p>Zoom out to view Alaska and Hawaii.<br>For more information, see our paper: <a href='https://example.com' target='_blank'>State Laws Mandating Parental Consent Relate to Lower Treatment Use among Adolescents</a>.</p>")
      
    ),
    
    column(
      
      3,
      wellPanel(
        h3(textOutput("state_header")),
        textOutput("state_rating"),
        br(),
        textOutput("state_access"),
        br(),
        textOutput("state_details")
      )
      
    )
    
  )
  
)

# Server
server <- function(input, output, session) {
  
  selected_state <- reactiveVal(NULL)
  
  output$map <- renderLeaflet({
    
    leaflet(merged_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ palette(Rating),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "black",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~NAME,
        layerId = ~NAME,
        group = "states"
      ) %>%
      addLegend(
        "bottomright", 
        colors = c("#e64747", "#e09c3b", "#e6e22e"),
        labels = c("Always Required", "Sometimes Required", "Never Required"),
        title = "Consent Rating",
        opacity = 0.7
      ) %>%
      setView(lng = -98, lat = 39, zoom = 4)
    
  })
  
  observeEvent(input$map_shape_click, {
    
    click <- input$map_shape_click
    
    if(!is.null(click)) {
      
      selected_state(click$id)
      
    }
    
  })
  
  output$state_header <- renderText({
    
    state <- selected_state()
    
    if (is.null(state)) {
      
      "State Information"
      
    } else {
      
      state
      
    }
    
  })
  
  output$state_rating <- renderText({
    
    state <- selected_state()
    
    if (is.null(state)) {
      
      "Click on a state to see information."
      
    } else {
      
      paste0(
        "Parental consent: ",
        merged_data$Rating[merged_data$NAME == state]
      )

    }
    
  })
  
  output$state_access <- renderText({
    
    state <- selected_state()
    
    if(!is.null(state)) {
      
      paste0(
        "Access Rate: ",
        scales::percent(merged_data$Access[merged_data$NAME == state], .1)
      )
      
    }
    
  })
  
  output$state_details <- renderText({
    
    state <- selected_state()
    
    if (!is.null(state)) {
      
      merged_data$Details[merged_data$NAME == state]

    }
    
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
