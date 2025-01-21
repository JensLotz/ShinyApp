library(shiny)
library(leaflet)
library(sf)
library(dplyr)

geo_data <- st_read("Wildfire.geojson")

if (st_crs(geo_data)$epsg != 4326) {
  geo_data <- st_transform(geo_data, crs = 4326)
}

geo_data <- geo_data %>% filter(YEAR != -999)

coords <- st_coordinates(geo_data)
geo_data <- cbind(geo_data, lon = coords[, 1], lat = coords[, 2])

classify_size <- function(size) {
  breaks <- c(seq(0, 100000, by = 10000), Inf) 
  labels <- 1:(length(breaks) - 1) 
  cut(size, breaks = breaks, labels = labels, include.lowest = TRUE)
}

geo_data$size_class <- as.numeric(classify_size(geo_data$SIZE_HA))

ui <- fluidPage(
  titlePanel("Interaktive Waldbrandkarte von Kanada"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "year_filter",
        label = "Jahr auswählen:",
        choices = c("Alle", sort(unique(geo_data$YEAR))),
        selected = "Alle",
        multiple = FALSE
      ),
      sliderInput(
        inputId = "size_filter",
        label = "Brandgröße (ha):",
        min = 0,
        max = 300000,
        value = c(0, 300000),
        step = 10000
      ),
      selectInput(
        inputId = "month_filter",
        label = "Monat auswählen:",
        choices = c("Alle", sort(unique(geo_data$MONTH))),
        selected = "Alle",
        multiple = FALSE
      ),
      selectInput(
        inputId = "province_filter",
        label = "Provinz auswählen:",
        choices = c("Alle", sort(unique(geo_data$SRC_AGENCY))),
        selected = "Alle",
        multiple = FALSE
      )
    ),
    mainPanel(
      leafletOutput(outputId = "map"),
      br(),
      textOutput("summary_text")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    geo_data %>% 
      filter(
        (input$year_filter == "Alle" | YEAR == input$year_filter),
        (SIZE_HA >= input$size_filter[1] & SIZE_HA <= input$size_filter[2]) | 
          (input$size_filter[2] == 300000 & SIZE_HA > 300000),
        (input$month_filter == "Alle" | MONTH == input$month_filter),
        (input$province_filter == "Alle" | SRC_AGENCY == input$province_filter)
      )
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -106.3468, lat = 56.1304, zoom = 4)  
  })
  
  observe({
    leafletProxy("map", data = filtered_data()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~pmax(size_class * (7 / 10), 2), 
        color = "darkred",
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~paste("<b>", FIRENAME, "</b><br>Provinz:", SRC_AGENCY, "<br>Größe:", round(SIZE_HA / 100, 2), "km²")
      )
  })
  
  output$summary_text <- renderText({
    data <- filtered_data()
    total_fires <- nrow(data)
    total_area_km2 <- sum(data$SIZE_HA / 100, na.rm = TRUE) 
    paste("Gesamtanzahl Brände:", total_fires, 
          " | Gesamte Brandfläche:", format(ceiling(total_area_km2), big.mark = ",", scientific = FALSE), "km²")

  })
}

shinyApp(ui = ui, server = server)
