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
library(htmltools)
library(RColorBrewer)




ui <- fluidPage(
  titlePanel('Violence: The Role of Disadvantage by Climate Zone'),
  tabsetPanel(
    tabPanel('1',fluid=TRUE,
             sidebarLayout(
               mainPanel(
                 htmlOutput('title1')
               ),
               sidebarPanel()
             )),
    tabPanel('2',fluid=TRUE,
             sidebarLayout(
               mainPanel(
                 htmlOutput('title2')
               ),
               sidebarPanel()
             )),
    tabPanel('3',fluid=TRUE,
             sidebarLayout(
               mainPanel(
                 htmlOutput('title3')
               ),
               sidebarPanel()
             )),
    tabPanel('4',fluid=TRUE,
             sidebarLayout(
               mainPanel(
                 htmlOutput('title4')
               ),
               sidebarPanel()
             )),
    tabPanel('5',fluid=TRUE,
             sidebarLayout(
               mainPanel(
                 htmlOutput('title5')
               ),
               sidebarPanel()
             )),
    tabPanel('6',fluid=TRUE,
             sidebarLayout(
               mainPanel(
                 htmlOutput('title6')
               ),
               sidebarPanel()
             )),
    tabPanel('7',fluid=TRUE,
             sidebarLayout(
               mainPanel(
                 htmlOutput('title7')
               ),
               sidebarPanel()
             )),
    tabPanel('8',fluid=TRUE,
             sidebarLayout(
               mainPanel(
                 htmlOutput('title8')
               ),
               sidebarPanel()
             )),
    tabPanel('Explore', fluid = TRUE,
             sidebarLayout(
               mainPanel(
                 conditionalPanel(condition = "input.radio == '1'",
                                  leafletOutput('map.zones')),
                 conditionalPanel(condition = "input.radio == '2'",
                                  leafletOutput('map.m0')),
                 conditionalPanel(condition = "input.radio == '3'",
                                  leafletOutput('map.m3')),
                 conditionalPanel(condition = "input.radio == '4'",
                                  leafletOutput('map.m4')),
                 conditionalPanel(condition = "input.radio == '5'",
                                  leafletOutput('map.m4.int')),
                 conditionalPanel(condition = "input.radio == '6'",
                                  leafletOutput('map.m5')),
                 conditionalPanel(condition = "input.radio == '7'",
                                  leafletOutput('map.m6')),
                 conditionalPanel(condition = "input.radio == '8'",
                                  leafletOutput('map.m7'))
               ),
               sidebarPanel(
                 radioButtons('radio', label = h3('Choose Map'),
                              choices = list('Climate Zones' = 1, 'Base Model (no covariates)' = 2, 'Model A: Climate Zone' = 3, 'Model B: Climate Zone' = 4,
                                             'Model B: Climate Zone x Disadvantage' = 5,'Model C: OLS by Regimes (Disadvantage)' = 6,
                                             'Model D: Spatial Lag by Regimes (Disadvantage)' = 7, 'Model E: Spatial Error by Regimes (Disadvantage)' = 8), 
                              selected = 1)
               )
             )
    )
  )
)

server <- function(input, output) {

  #output$value <- renderPrint({ input$radio })
  #output$value <- 1
  
  zones <- readOGR('data/climatezones_aug.shp',layer='climatezones_aug', GDAL1_integer64_policy = TRUE)
  
  m0.bins = c(-Inf,-6.0,-1.5,-.0001,0.0001,1.5,6.0,Inf)
  m0.pal <- colorBin('RdBu', domain = zones$Coef0Z, bins = m0.bins, reverse=TRUE)
  
  m3.bins = c(-Inf,-.812,-.527,0,0.0001,.326,.611,Inf)
  m3.pal <- colorBin('RdBu', domain = zones$Coef3Z, bins = m3.bins, reverse=TRUE)
  
  m4.bins.zone = c(-Inf,-.640,-.228,-.038,0.0001,.236,.646,Inf)
  m4.pal.zone <- colorBin('RdBu', domain = zones$Coef4Z, bins = m4.bins.zone, reverse=TRUE)
  
  m4.bins.int = c(-Inf,-.236,-.059,0,0.001,0.3,0.620,Inf)
  m4.pal.int <- colorBin('RdBu', domain = zones$Coef4Int, bins = m4.bins.int, reverse=TRUE)
  
  m5.bins <- seq(min(zones$Coef5D),max(zones$Coef5D),by=(max(zones$Coef5D) - min(zones$Coef5D))/7)
  m5.pal <- colorBin('YlOrRd', domain = zones$Coef5D, bins = m5.bins, reverse=TRUE)
  
  m6.bins <- seq(min(zones$Coef6D),max(zones$Coef6D),by=(max(zones$Coef6D) - min(zones$Coef6D))/7)
  m6.pal <- colorBin('YlOrRd', domain = zones$Coef6D, bins = m6.bins, reverse=TRUE)
  
  m7.bins <- seq(min(zones$Coef7D),max(zones$Coef7D),by=(max(zones$Coef7D) - min(zones$Coef7D))/7)
  m7.pal <- colorBin('YlOrRd', domain = zones$Coef7D, bins = m7.bins, reverse=TRUE)
  
  base.labels <- sprintf(
    "<strong>%s</strong><br/>",
    zones$Name
  ) %>% lapply(HTML)
  
  m0.labels <- sprintf(
    "<strong>%s</strong><br/>Base Model Coefficient:  %g",
    zones$Name, zones$Coef0Z
  ) %>% lapply(HTML)
  
  m3.labels <- sprintf(
    "<strong>%s</strong><br/>Model 3 Zone Coefficient:  %g",
    zones$Name, zones$Coef3Z
  ) %>% lapply(HTML)
  
  m4.labels <- sprintf(
    "<strong>%s</strong><br/>Model 4 Zone Coefficient:  %g",
    zones$Name, zones$Coef4Z
  ) %>% lapply(HTML)
  
  m4.int.labels <- sprintf(
    "<strong>%s</strong><br/>Model 4 Zone x Disadvantage Coefficient:  %g",
    zones$Name, zones$Coef4Int
  ) %>% lapply(HTML)
  
  m5.labels <- sprintf(
    "<strong>%s</strong><br/>Model 5 (OLS by Regimes) Disadvantage Coefficient:  %g",
    zones$Name, zones$Coef5D
  ) %>% lapply(HTML)
  
  m6.labels <- sprintf(
    "<strong>%s</strong><br/>Model 6 (Spatial Lag by Regimes) Disadvantage Coefficient:  %g",
    zones$Name, zones$Coef6D
  ) %>% lapply(HTML)
  
  m7.labels <- sprintf(
    "<strong>%s</strong><br/>Model 7 (Spatial Error by Regimes) Disadvantage Coefficient:  %g",
    zones$Name, zones$Coef7D
  ) %>% lapply(HTML)
  
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
                                                                labelOptions = lopts) %>%
                                                addLegend(pal=m0.pal, values=~zones$Coef0Z, opacity = 0.7, title = NULL,position='bottomright')})

  output$map.m3 <- renderLeaflet({zones.map %>% addPolygons(weight = 1, color = '#000000',opacity = 1.0,fill=TRUE,fillOpacity = 0.6, fillColor = ~m3.pal(zones$Coef3Z),
                                                            highlight = hopts,
                                                            label = m3.labels,
                                                            labelOptions = lopts) %>%
                                                addLegend(pal=m3.pal, values=~zones$Coef3Z, opacity = 0.7, title = NULL,position='bottomright')})
  
  output$map.m4 <- renderLeaflet({zones.map %>% addPolygons(weight = 1, color = '#000000',opacity = 1.0,fill=TRUE,fillOpacity = 0.6,
                                                            fillColor = ~m4.pal.zone(zones$Coef4Z),
                                                            highlight = hopts,
                                                            label = m4.labels,
                                                            labelOptions = lopts) %>%
                                                addLegend(pal=m4.pal.zone, values=~zones$Coef4Z, opacity = 0.7, title = NULL,position='bottomright')})
  
  output$map.m4.int <- renderLeaflet({zones.map %>% addPolygons(weight = 1, color = '#000000',opacity = 1.0,fill=TRUE,fillOpacity = 0.6,
                                                                fillColor = ~m4.pal.int(zones$Coef4Int),
                                                                highlight = hopts,
                                                                label = m4.int.labels,
                                                                labelOptions = lopts) %>%
                                                    addLegend(pal=m4.pal.int, values=~zones$Coef4Int, opacity = 0.7, title = NULL,position='bottomright')})
  
  output$map.m5 <- renderLeaflet({zones.map %>% addPolygons(weight = 1, color = '#000000',opacity = 1.0,fill=TRUE,fillOpacity = 0.6,
                                                            fillColor = ~m5.pal(zones$Coef5D),
                                                            highlight = hopts,
                                                            label = m5.labels,
                                                            labelOptions = lopts) %>%
                                                addLegend(pal=m5.pal, values=~zones$Coef5D, opacity = 0.7, title = NULL,position='bottomright')})
  
  output$map.m6 <- renderLeaflet({zones.map %>% addPolygons(weight = 1, color = '#000000',opacity = 1.0,fill=TRUE,fillOpacity = 0.6,
                                                            fillColor = ~m6.pal(zones$Coef6D),
                                                            highlight = hopts,
                                                            label = m6.labels,
                                                            labelOptions = lopts) %>%
                                                addLegend(pal=m6.pal, values=~zones$Coef6D, opacity = 0.7, title = NULL,position='bottomright')})
  
  output$map.m7 <- renderLeaflet({zones.map %>% addPolygons(weight = 1, color = '#000000',opacity = 1.0,fill=TRUE,fillOpacity = 0.6,
                                                            fillColor = ~m7.pal(zones$Coef7D),
                                                            highlight = hopts,
                                                            label = m7.labels,
                                                            labelOptions = lopts) %>%
                                                addLegend(pal=m7.pal, values=~zones$Coef7D, opacity = 0.7, title = NULL,position='bottomright')})

  output$text.0 <- renderUI({sprintf("<br><p align='center'>This is some text.")%>% lapply(HTML)})
  
  output$title1 <- renderUI({sprintf("<br><b>1. Introduction</b><br><hr>")%>% lapply(HTML)})
  output$title2 <- renderUI({sprintf("<br><b>2. Climate Zones</b><br><hr>")%>% lapply(HTML)})
  output$title3 <- renderUI({sprintf("<br><b>3. Violence Rate by Region</b><br><hr>")%>% lapply(HTML)})
  output$title4 <- renderUI({sprintf("<br><b>4. Model A: Climate Zone</b><br><hr>")%>% lapply(HTML)})
  output$title5 <- renderUI({sprintf("<br><b>5. Model B: Climate Zone x Disadvantage</b><br><hr>")%>% lapply(HTML)})
  output$title6 <- renderUI({sprintf("<br><b>6. Model C: OLS by Region</b><br><hr>")%>% lapply(HTML)})
  output$title7 <- renderUI({sprintf("<br><b>7. Model D: Spatial Lag by Region</b><br><hr>")%>% lapply(HTML)})
  output$title8 <- renderUI({sprintf("<br><b>8. Model E: Spatial Error by Region</b><br><hr>")%>% lapply(HTML)})
    
}

# Run the application 
shinyApp(ui = ui, server = server)

