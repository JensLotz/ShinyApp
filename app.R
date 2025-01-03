library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# Daten laden
geo_data <- st_read("bbb.geojson")

# Projektion überprüfen und transformieren
if (st_crs(geo_data)$epsg != 4326) {
  geo_data <- st_transform(geo_data, crs = 4326)
}

# -999 Werte im Jahr entfernen
geo_data <- geo_data %>% filter(YEAR != -999)


# Koordinaten extrahieren
coords <- st_coordinates(geo_data)
geo_data <- cbind(geo_data, lon = coords[, 1], lat = coords[, 2])

classify_size <- function(size) {
  breaks <- c(seq(0, 100000, by = 10000), Inf) # 11 Intervalle
  labels <- 1:(length(breaks) - 1) # Labels entsprechend den Intervallen
  cut(size, breaks = breaks, labels = labels, include.lowest = TRUE)
}


# Größe zuweisen
geo_data$size_class <- as.numeric(classify_size(geo_data$SIZE_HA))

# UI definieren
ui <- fluidPage(
  titlePanel("Interaktive Karte mit Filtern"),
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
      )
    ),
    mainPanel(
      leafletOutput(outputId = "map")
    )
  )
)

# Server definieren
server <- function(input, output, session) {
  
  # Gefilterte Daten
  filtered_data <- reactive({
    geo_data %>% 
      filter(
        (input$year_filter == "Alle" | YEAR == input$year_filter),
        (SIZE_HA >= input$size_filter[1] & SIZE_HA <= input$size_filter[2]) | 
          (input$size_filter[2] == 300000 & SIZE_HA > 300000),
        (input$month_filter == "Alle" | MONTH == input$month_filter)
      )
  })
  
  # Karte rendern
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -106.3468, lat = 56.1304, zoom = 4) # Kanada zentrieren
  })
  
  # Punkte aktualisieren
  observe({
    leafletProxy("map", data = filtered_data()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~pmax(size_class * (7 / 10), 2), # Mindestgröße 3 Pixel
        color = "darkred",
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~paste("<b>", FIRENAME, "</b><br>Provinz:", SRC_AGENCY, "<br>Größe:", SIZE_HA, "ha")
      )
    
  })
}

# Shiny App starten
shinyApp(ui = ui, server = server)
