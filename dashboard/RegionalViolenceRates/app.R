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
       leafletOutput('map.zones'),
       leafletOutput('map.m0'),
       leafletOutput('map.m3'),
       leafletOutput('map.m4')
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

  zones <- readOGR('data/climatezones_aug.shp',layer='climatezones_aug', GDAL1_integer64_policy = TRUE)
  
  m0.bins = c(-Inf,-6.0,-1.5,-.0001,0.0001,1.5,6.0,Inf)
  m0.pal <- colorBin('RdBu', domain = zones$Coef0Z, bins = m0.bins, reverse=TRUE)
  
  m3.bins = c(-Inf,-.812,-.527,0,0.0001,.326,.611,Inf)
  m3.pal <- colorBin('RdBu', domain = zones$Coef3Z, bins = m3.bins, reverse=TRUE)
  
  m4.bins.zone = c(-Inf,-.640,-.228,-.038,0.0001,.236,.646,Inf)
  m4.pal.zone <- colorBin('RdBu', domain = zones$Coef4Z, bins = m4.bins.zone, reverse=TRUE)
  
  base.labels <- sprintf(
    "<strong>%s</strong><br/>",
    zones$Name
  ) %>% lapply(htmltools::HTML)
  
  m0.labels <- sprintf(
    "<strong>%s</strong><br/>Base Model Coefficient:  %g",
    zones$Name, zones$Coef0Z
  ) %>% lapply(htmltools::HTML)
  
  m3.labels <- sprintf(
    "<strong>%s</strong><br/>Model 3 Zone Coefficient:  %g",
    zones$Name, zones$Coef3Z
  ) %>% lapply(htmltools::HTML)
  
  m4.labels <- sprintf(
    "<strong>%s</strong><br/>Model 4 Zone Coefficient:  %g",
    zones$Name, zones$Coef4Z
  ) %>% lapply(htmltools::HTML)
  
  hopts <- highlightOptions(weight = 4,
                         color = "#555555",
                         dashArray = "",
                         fillOpacity = .6,
                         bringToFront = TRUE)
  
  lopts <- labelOptions(style = list("font-weight" = "normal"),
                        textsize = "15px",direction = "auto",opacity=1)
  
  zones.map <- leaflet(zones,option=leafletOptions(zoomControl=FALSE,minZoom=4, maxZoom=4)) %>%
                 addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
                 addLabelOnlyMarkers(lng = ~Lon, lat = ~Lat, label = ~as.character(zone),
                                       labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
                                                    style = list("font-size" = "14px")))
  
  output$map.zones <- renderLeaflet({zones.map %>% addPolygons(weight = 1, color = '#000000',opacity = 1.0,fill=TRUE,fillOpacity = 0.6, fillColor = ~Color,
                                                               highlight = hopts,
                                                               label = base.labels,
                                                               labelOptions = lopts)})
  
  output$map.m0 <- renderLeaflet({zones.map %>% addPolygons(weight = 1, color = '#000000',opacity = 1.0,fill=TRUE,fillOpacity = 0.6, fillColor = ~m0.pal(zones$Coef0Z),
                                                                highlight = hopts,
                                                                label = m0.labels,
                                                                labelOptions = lopts)})

  output$map.m3 <- renderLeaflet({zones.map %>% addPolygons(weight = 1, color = '#000000',opacity = 1.0,fill=TRUE,fillOpacity = 0.6, fillColor = ~m3.pal(zones$Coef3Z),
                                                            highlight = hopts,
                                                            label = m3.labels,
                                                            labelOptions = lopts)})
  
  output$map.m4 <- renderLeaflet({zones.map %>% addPolygons(weight = 1, color = '#000000',opacity = 1.0,fill=TRUE,fillOpacity = 0.6,
                                                            fillColor = ~m4.pal.zone(zones$Coef4Z),
                                                            highlight = hopts,
                                                            label = m4.labels,
                                                            labelOptions = lopts)})
}





leaflet(map34,option=leafletOptions(zoomControl=FALSE,minZoom=4, maxZoom=4)) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addPolygons(color = 'black', weight = 1, smoothFactor = 1,
              opacity = 1.0, fillOpacity = 0.6,
              fillColor = ~dums.pal(zones$Coef0Z))





# Run the application 
shinyApp(ui = ui, server = server)

