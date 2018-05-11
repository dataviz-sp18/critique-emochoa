#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(sp)
library(rgdal)
library(shiny)
library(leaflet)
library(jsonlite)
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel('Violence by Climate Zone'),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     # Show a plot of the generated distribution
     mainPanel(
       leafletOutput('map.zones')
     ),
     
      sidebarPanel(
      #   sliderInput("bins",
       #              "Number of bins:",
        #             min = 1,
         #            max = 50,
          #           value = 30)
      )
      

   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  #coefs <- read.csv('data/coefs.csv')
  #zones <- readLines('data/climatezones_aug.geojson') %>% paste(collapse = "\n")  %>% fromJSON(simplifyVector = FALSE)
  zones <- readOGR('data/climatezones_aug.shp',layer='climatezones_aug', GDAL1_integer64_policy = TRUE)
  #colors <- c('#D3D3D3','#A9A9A9',brewer.pal(10,'Paired'),'#D8BA97','#B15928')
  
  output$map.zones <- renderLeaflet({
                        leaflet(zones,option=leafletOptions(zoomControl=FALSE,minZoom=4, maxZoom=4)) %>%
                               #setView(lng = -98.583, lat = 39.833, zoom = 4) %>%
                               addTiles() %>%
                               addPolygons(weight = 1, color = '#000000',opacity = 1.0, fillOpacity = 0.6, fill = TRUE, fillColor = ~Color) %>%
                               addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
                               addLabelOnlyMarkers(lng = ~Lon, lat = ~Lat, label = ~as.character(zone),
                                                   labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
                                                                               style = list("font-size" = "14px")))
                               })
}

# Run the application 
shinyApp(ui = ui, server = server)

