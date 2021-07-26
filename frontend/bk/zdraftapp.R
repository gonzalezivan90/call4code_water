#rm(list = ls(all.names = TRUE))
library(leaflet)
library(leaflet.extras)
library(shiny)
library(rgeos)
library(rgdal)
library(raster)
library(maptools)
library('htmltools')
 
library(shinydashboard)
library(highcharter)
library(devtools)
library(shinycssloaders)
library(reshape2)
library(rgee) # ee_Initialize() #ee_install()
ee_Initialize(email = 'gonzalezgarzon.ivan@gmail.com') # as server

# setwd('C:/GoogleDrive/call4code_water/frontend')

#write.csv(data.frame(ID = 1), '1.csv' ) # /srv/shiny-server/call4code/1.csv

# Load some elements
#devtools::source_url("https://raw.githubusercontent.com/gonzalezivan90/biotablero_api/master/biotablero_fun.R")
aws <- 'ec2-3-137-83-192.us-east-2.compute.amazonaws.com' # Prod
# aws_port <- ':8080'
prj_wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

basWWF <- ee$FeatureCollection("WWF/HydroSHEDS/v1/Basins/hybas_9")
#land <- ee$ImageCollection("GLCF/GLS_WATER")$select('water')
rivWWF0 <- ee$FeatureCollection("WWF/HydroSHEDS/v1/FreeFlowingRivers")

icons1 <<- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = c('green')
)

icons2 <<- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = c('red')
)


## Define tempfolder
outDir <- ifelse( Sys.info()["sysname"] == "Linux",  '/home/shiny/tmpR/', 'C:/temp/Rtmp/'); dir.create(outDir)
sapply(grep(paste0(outDir, '//*.+'), list.dirs(outDir), value = TRUE), unlink, recursive = TRUE, force = TRUE)
print(paste('WD: ', getwd()))
isShpLoad <<- FALSE
shp <<- NULL
xls_recs <<- xls_biod <<- xls_uicn <<- data.frame(sp = c('Empty', 'Empty'))

hcNULL <- highchart() %>% hc_chart(type = "pie") %>% hc_title(text = 'El resultado es 0')
hcBIG <- highchart() %>% hc_chart(type = "pie") %>% hc_title(text = 'Área de estudio mayor al umbral permitido')

hcErrors <- highchart() %>% hc_chart(type = "") %>% 
  hc_title(text = paste('<b>Error<\b>' #,  'This is a title with <i>margin</i> and <b>Strong or bold text</b>' 
  ),
  margin = 20, style = list(fontSize = "30px", color = "red", useHTML = TRUE))


# UI  ---------------

ui <- dashboardPage(skin = 'green',
                    dashboardHeader(
                      #title = "Integración de observaciones de la tierra para la toma de decisiones sobre biodiversidad en Colombia", 
                      title = "Forest4Water - Call4Code IBM", 
                      #title = tags$a(#href='http://rsensus.org/en/', "Decision support sytem for Colombian BON v. Beta"),
                      titleWidth = 400
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        
                        
                        # UI Panel  ---------------
                        # exclamation-triangle leaf globe #frog dove charts 
                        # https://getbootstrap.com/docs/3.3/components/ 
                        # http://glyphicons.com/ 
                        # https://fontawesome.com/icons?d=gallery
                        
                        menuItem("Start here", tabName = "intro", startExpanded = TRUE, icon = icon("hand-holding-water"), 
                                 menuSubItem(" -- Goal", tabName = "tab_goal", icon = icon("flag-checkered")),
                                 menuSubItem(" -- Purpouse", tabName = "tab_purp", icon = icon("lightbulb")),
                                 menuSubItem(" -- How it works", tabName = "tab_how", icon = icon("cogs")),
                                 menuSubItem(" -- Future", tabName = "tab_how", icon = icon("rocket")),
                                 menuSubItem(" -- Team", tabName = "tab_how", icon = icon("users-cog"))
                                 ),
                        
                        #menuItem("Definir region de estudio", tabName = "draw") ,
                        
                        menuItem("Find water!", tabName = "tab_findwater", icon = icon("map-pin")),
                        menuItem("Assess water!", tabName = "tab_assesswater", icon = icon("poll"))
                        
                      )
                    ),
                    
                    
                    dashboardBody(
                      tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
      '))),
                      tabItems(
                        tabItem("intro",
                                fluidRow(
                                  tabBox(width = 12,
                                         
                                         tabPanel("Inicio",
                                                  includeMarkdown("md_intro.md")
                                         ),
                                         tabPanel("Equipo",
                                                  includeMarkdown("md_ppl.md")
                                         ),
                                         tabPanel("Objetivos",
                                                  includeMarkdown("md_obj1.md")
                                         ),
                                         tabPanel("Hitos",
                                                  includeMarkdown("md_obj2.md")
                                         ),
                                         tabPanel("Métricas",
                                                  includeMarkdown("md_table.md")
                                         )
                                  )
                                )
                        ),
                        
                        tabItem("contact",
                                includeMarkdown("md_contact.md")
                                
                        ),
                        # UI draw pol   ---------------
                        tabItem("draw",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,  
                                  column(width = 4,
                                         
                                         h4(HTML('<b>Cargue alguno de estos:</b>')),
                                         h5('- ZIP con archivos de polígino ESRI Shapefile'),
                                         h5('- Archivos ESRI Shapefile (al menos .shp, .shx, .dbf)'),
                                         h5('- Archivo GeoJSON'),
                                         h5('- Archivo SQLite'),
                                         h5('- Archivo Geopackage'),
                                         h5('El polígono debe ser de gemoetría sencilla, menor a 5.000 Km de superficie y con proyección real de WGS84 si no tiene esta información en el archivo'),
                                         h5(''),
                                         
                                         shiny::fileInput('shapefile', 'Cargar polígono/AOI',buttonLabel = 'Buscar', placeholder = 'Sin selección',
                                                          accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj", '.zip', '.gpkg', '.SQLite', '.GeoJSON'),
                                                          #accept= '.zip',
                                                          multiple=TRUE),
                                         actionButton("loadShp", "Cargar!"),
                                         br(),
                                         h5(''),
                                         verbatimTextOutput("voutext") %>% withSpinner(color="#0dc5c1")),
                                  column(width = 6, leafletOutput("loadMapLL", height = "600px") %>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        ######## Ecosystems
                        # UI Ecosystems  ---------------
                        tabItem("in_forest",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                  column(width = 4, 
                                         selectInput(inputId = "aoi_forest", label = "Área de estudio: ", choices =  c('Dibujar'), selected = 'Dibujar'),
                                         selectInput(inputId = "forestsour", label = "Fuente: ", choices =  c('ideam', 'hansen'), selected = 'hansen')),
                                  # selectInput(inputId = "forestvar", label = "Metric: ",
                                  #             choices =  c('area'), selected = 'area'),
                                  column(width = 4, sliderInput(inputId = "forestyearrng", label = 'Rango temporal',
                                                                min = 1990, max = 2018, value=c(2000, 2018), sep = ""),
                                         sliderInput(inputId = "forestporcrng", label = 'Porcentaje cobertura de bosque:',
                                                     min = 0, max = 100, value=c(80, 100))),
                                  column(width = 1, actionButton("go_forest", "Ejecutar"))
                                ),
                                fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                  column(width = 6, leafletOutput("leafForest", height = "600px") %>% withSpinner(color="#0dc5c1"))
                                  , column(width = 6, highchartOutput("foresttrend", height = "600px") %>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        tabItem("tab_findwater",
                                #fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                  column(width = 6,
                                         fluidRow(
                                         column(width = 4, actionButton("go_find", "Find water!")),
                                         column(width = 8, verbatimTextOutput("voutdist") %>% withSpinner(color="#0dc5c1"))
                                         ),
                                         # fluidRow(
                                         #   column(width = 6, selectInput(inputId = "aoi_clc", 
                                         #                                 label = "Área de estudio: ", choices =  c('Dibujar'), selected = 'Dibujar')),
                                         #   column(width = 4, selectInput(inputId = "clc_lev", 
                                         #                                 label = "Nivel: ", choices =  c(1:3), selected = 1)),
                                         # ),
                                         leafletOutput("findLeaf", height = "600px") %>% withSpinner(color="#0dc5c1")
                                         #leafletOutput("findLeaf", width = "90%", height = "90%")
                                  ),
                                  
                                  column(width = 6, 
                                         highchartOutput("clcPlot1", height = "600px")%>% withSpinner(color="#0dc5c1")
                                         #, highchartOutput("clcPlot2", height = "600px")%>% withSpinner(color="#0dc5c1")
                                  )
                                )
                        ),
                        
                      
                        ######## UI Managment -----
                        
                       
                        tabItem("in_comp",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_comp", label = "Área de estudio: ", choices =  c('Dibujar'), selected = 'Dibujar')),
                                           column(width = 1, br(), actionButton("go_comp", "Ejecutar"))),
                                         fluidRow(
                                           leafletOutput("compLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, 
                                         highchartOutput("compPlot1", height = "400px") %>% withSpinner(color="#0dc5c1"), 
                                         highchartOutput("compPlot2", height = "300px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        ######## UI Species -----
                        tabItem("in_uicn",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 6, selectInput(inputId = "aoi_uicn", label = "Área de estudio: ", choices =  c('Dibujar'), selected = 'Dibujar')),
                                           column(width = 3, br(), actionButton("go_uicn", "Ejecutar")),
                                           column(width = 3, br(), downloadButton("uicn_xls", "Descargar"))
                                         ),
                                         fluidRow(
                                           leafletOutput("uicnLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, dataTableOutput('tablespuicn')%>% withSpinner(color="#0dc5c1") )
                                )
                        ),
                       
                        
                        ######## UI Indexes -----
                        tabItem("in_sur",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 5 , selectInput(inputId = "aoi_sur", label = "Área de estudio: ", choices =  c('Dibujar'), selected = 'Dibujar')),
                                           column(width = 5 , selectInput(inputId = "sur_size", label = "Longitud lado pixel (Km): ", 
                                                                          choices =  rev(
                                                                            c(1,   2,   5, 10, 20, 50, 100, 200, 500)
                                                                            #c(.8, 3.3, 20, 82, 331, 2070,8289 )
                                                                            # c(1, 4, 25, 100, 400, 2500, 10000, 40000, 250000)
                                                                          ), selected = 20)),
                                           column(width = 2, br(), actionButton("go_sur", "Ejecutar"))
                                         ),
                                         fluidRow(
                                           leafletOutput("surLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")
                                         )
                                         
                                  ),
                                  column(width = 6, 
                                         highchartOutput("surPlot1", height = "800px") %>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        tabItem("in_rli",
                                fluidRow(h3(' ')),
                                h5(''),
                                column(width = 1, 
                                       fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                         column(width = 1, actionButton("go_rli", "Ejecutar"))))
                                ,
                                column(width = 11, 
                                       highchartOutput("rliPlot1", height = "400px") %>% withSpinner(color="#0dc5c1"))
                        )
                        # ,
                        # tabItem("in_test",
                        #         fluidRow(h3(' ')),
                        #         h5(''),
                        #         highchartOutput("biomPlot1", height = "600px")%>% withSpinner(color="#0dc5c1"), 
                        #         highchartOutput("biotPlot1", height = "600px")%>% withSpinner(color="#0dc5c1")
                        # )
                      )
                    )
)


##### SERVER ----------------------
server <- function(input, output, session) {
 
  ##### Default widgets ----------------------
  
  output$foresttrend <- renderHighchart({NULL})
  output$loadMapLL <- renderHighchart({NULL})
  output$paramPlot1 <- renderHighchart({NULL})
  output$paramPlot2 <- renderHighchart({NULL})
  output$dryPlot1 <- renderHighchart({NULL})
  output$dryPlot2 <- renderHighchart({NULL})
  
  output$clcPlot1 <- output$clcPlot2 <- output$redPlot1 <- output$redPlot2 <- output$biotPlot1 <- output$biotPlot2 <- output$biomPlot1 <- output$biomPlot2 <- output$dryPlot1 <- output$dryPlot2 <- 
    output$paramPlot1 <- output$paramPlot2 <- output$wetPlot1 <- output$wetPlot2 <- output$apPlot1 <- output$apPlot2 <- output$colePlot1 <- output$colePlot2 <- output$smaPlot1 <- output$smaPlot2 <- 
    output$compPlot1 <- output$compPlot2 <- output$uicnPlot1 <- output$uicnPlot2 <- output$biodPlot1 <- output$biodPlot2 <- output$recPlot1 <- 
    output$recPlot2 <- output$rliPlot1 <- output$rliPlot2 <- output$surPlot1 <- output$surPlot2 <- renderHighchart({NULL})
  
  output$loadMapLL <- renderLeaflet({
    reactShp$leaf0
  })
  
  output$tablesprec <- output$tablespuicn <- output$tablespbiod <- renderDataTable(NULL)
  
  ##### Leaflets ----------------------
  
  output$findLeaf <- output$clcLeaf <- output$redLeaf <- output$biotLeaf <- output$biomLeaf <- output$wetLeaf <- 
    output$apLeaf <- output$coleLeaf <- 
    output$smaLeaf <- output$compLeaf <- output$uicnLeaf <- output$biodLeaf <- output$recLeaf <- output$surLeaf <- 
    output$dryLeaf <- output$paramLeaf <- output$leafForest <- renderLeaflet({
      reactShp$leaf0 %>%
        leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
                                       rectangleOptions = FALSE, circleOptions = FALSE,
                                       markerOptions = FALSE, circleMarkerOptions = FALSE,
                                       editOptions = leaflet.extras::editToolbarOptions())
      #addDrawToolbar(editOptions = editToolbarOptions())
    })
  
  
  
  reactShp <- reactiveValues(shp = FALSE,
                             leaf0 = leaflet() %>% addTiles() %>% 
                               addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                                                options = layersControlOptions(collapsed = FALSE)) %>%
                               addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
                               setView(lng = -74, lat = 4.6, zoom = 10)
  )
  
  
  
  hc1 <<- hc2 <<- NULL 
  
  ##### Load shp  ----------------------
  

  output$voutdist <- renderText({isolate("Place the map and then hit the button")})
  
  
  ##### Go find water  ----------------------  
  # input <- list(forestyearrng = c(2000, 2005), forestporcrng = c(25, 100), forestsour = 'ideam')

  
  observeEvent(input$findLeaf_click, {
    click <- input$findLeaf_click
    text<-paste("Lattitude:", round(click$lat, 2),
                "<br>Longtitude:", round(click$lng, 4),
                "<br>Date:", Sys.Date())
    proxy <- leafletProxy("findLeaf")
    proxy %>% clearPopups() %>%
      addPopups(click$lng, click$lat, text)
  })
  
  isolate(observeEvent(input$go_find,{
    readyLayer <- FALSE
    #polDraw <- input$leafForest_draw_new_feature
    output$findLeaf <- renderLeaflet({
      
      inClick <- input$findLeaf_click
      #print(str(inClick))
      
      ptsCoord <<- c(lon = inClick$lng, lat = inClick$lat)
      aoiPoint <<- ee$Geometry$Point(ptsCoord);
      basaoi <<- basWWF$filterBounds(aoiPoint);

      
      rivWWF <<- rivWWF0$filterBounds(basaoi);

      system.time(rivWWF_gi <- rivWWF$getInfo())
      
      
      slDf <- SpatialLinesDataFrame(
        sl = SpatialLines(
          lapply(
            rivWWF_gi$features, 
            function(x){
              # x <- rivWWF_gi$features[[9]]
              cx <- x$geometry$coordinates
              Lines(Line(matrix(unlist(cx), ncol = 2, byrow = TRUE)), ID = x$id)
            }
          )),
        data = data.frame(ID = 1:length(rivWWF_gi$features)), match.ID = FALSE)
      
      
      snpDf <<- snapPointsToLines(SpatialPoints(cbind(ptsCoord[1], ptsCoord[2])), slDf)
      
      dstPoint <<- ee$FeatureCollection(ee$Feature(ee$Geometry$Point(snpDf@coords[1, 1:2, drop = TRUE])));
 
      system.time(basaoi_gi <- basaoi$getInfo())
      spDf <- SpatialPolygonsDataFrame(
            SpatialPolygons(
                     list(
                       Polygons(list(Polygon(matrix(unlist(
                         basaoi_gi$features[[1]]$geometry$coordinates[[1]]
                         ), ncol = 2, byrow = TRUE))), 1) # polgons
                     )), data = data.frame(ID = 1), match.ID = FALSE)
      
      
      leafGee <- leaflet() %>% leaflet() %>% addTiles() %>% 
        #setView(lng = inClick$lng, lat = inClick$lat, zoom = 10 ) %>%
        addPolygons(data = spDf, popup = paste0("Basin"),
                    group = "Basin", fillOpacity = .2,
                    color = 'grey60') %>%
        addPolylines(data = slDf,
                    label = ~htmlEscape('Rivers'),
                    popup = paste0("Rivers"),
                    group = "Rivers",
                    fillOpacity = .9,
                    color = '#3181BF') %>%
        
        addAwesomeMarkers(data = data.frame(t(ptsCoord)), icon = icons2, 
                   lng = ~lon, lat = ~lat, group = 'Origin',
                   label = ~htmlEscape('Origin'),
        ) %>% 
        addAwesomeMarkers(data = snpDf, icon = icons1,
                   group = 'Destination', label = ~htmlEscape('Destination'),
        ) %>% addLegend("bottomright", 
                              colors = c("#9099A0", "#3181BF", "#FF0000", "#006600"),
                              labels= c('Basin', 'Rivers', 'Source', 'Destination'), 
                              title = "Legend") %>%
        addLayersControl(overlayGroups = c('Basin', 'Rivers', 'Origin', 'Destination'), 
                         baseGroups = c("OpenStreetMap",
                                        "Esri.WorldImagery",
                                        'CartoDB.Positron', 'CartoDB.DarkMatter',
                                        'OpenTopoMap'),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
        addProviderTiles( "CartoDB.Positron", group = "CartoDB.Positron" ) %>%
        addProviderTiles( "CartoDB.DarkMatter", group = "CartoDB.DarkMatter" ) %>%
        addProviderTiles( "OpenTopoMap", group = "OpenTopoMap" )
        
      leafGee 

    })
    
      output$voutdist <- renderText({
      ## Return text
        paste0("Go to: \n-Lng: ", snpDf@coords[1,1], 
               '\n-Lat: ', snpDf@coords[1,2], 
               "\nDistance to walk: ",
          round(pointDistance(snpDf, ptsCoord, lonlat = TRUE, allpairs=FALSE),2), 
          ' meters')
      })
    
  }))
  
} # End server

shinyApp(ui, server)


