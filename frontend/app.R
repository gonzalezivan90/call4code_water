#rm(list = ls(all.names = TRUE))
library(leaflet)
library(leaflet.extras)
library(shiny)
library(rgeos)
library(rgdal)
library(raster)
library(maptools)
library(htmltools)
library(shinydashboard)
library(highcharter)
library(devtools)
library(shinycssloaders)
library(reshape2)
library(rgee) # ee_Initialize() #ee_install()
#remotes::install_github("r-earthengine/rgeeExtra")
if ( Sys.info()["sysname"] == "Windows" | any(grep('azure', Sys.info()["release"]))){
  ee_Initialize(email = 'gonzalezgarzon.ivan@gmail.com') # as server
} else {
  ee_Initialize(user = 'gonzalezgarzon.ivan@gmail.com') # as server
}

# setwd('C:/GoogleDrive/call4code_water/frontend')

#write.csv(data.frame(ID = 1), '1.csv' ) # /srv/shiny-server/call4code/1.csv

# Load some elements
basWWF <- ee$FeatureCollection("WWF/HydroSHEDS/v1/Basins/hybas_9")
rivWWF0 <- ee$FeatureCollection("WWF/HydroSHEDS/v1/FreeFlowingRivers")

## GEE
chirps <<- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")$select('precipitation'); # mm/hr
temp <<- ee$ImageCollection("MODIS/006/MOD11A2")$select('LST_Day_1km') #  kelvin
tree <<- ee$Image("UMD/hansen/global_forest_change_2019_v1_7")$select('treecover2000') 
loss <<- ee$Image("UMD/hansen/global_forest_change_2019_v1_7")$select('lossyear') 



## Icons 
icons1 <<- awesomeIcons(
  icon = 'ios-close',iconColor = 'black',
  library = 'ion', markerColor = c('green')
)

icons2 <<- awesomeIcons(
  icon = 'ios-close', iconColor = 'black',
  library = 'ion', markerColor = c('red')
)


## Define tempfolder
outDir <- ifelse( Sys.info()["sysname"] == "Linux",  '/home/shiny/tmpR/', 'C:/temp/Rtmp/'); dir.create(outDir)
sapply(grep(paste0(outDir, '//*.+'), list.dirs(outDir), value = TRUE), unlink, recursive = TRUE, force = TRUE)
print(paste('WD: ', getwd()))
isShpLoad <<- FALSE
readyLayer <<- FALSE
shp <<- NULL

hcNULL <- highchart() %>% hc_chart(type = "pie") %>% hc_title(text = 'El resultado es 0')
hcNOP <- highchart() %>% hc_chart(type = "pie") %>% hc_title(text = 'Please use "Find water" tool first to select an area')

hcErrors <- highchart() %>% hc_chart(type = "") %>% 
  hc_title(text = paste('<b>Error<\b>' #,  'This is a title with <i>margin</i> and <b>Strong or bold text</b>' 
  ),
  margin = 20, style = list(fontSize = "30px", color = "red", useHTML = TRUE))


# UI  ---------------

ui <- dashboardPage(skin = 'green',
                    dashboardHeader(
                      title = "Forest4Water - Call4Code IBM", 
                      #title = tags$a(#href='http://rsensus.org/en/', "Decision support sytem for Colombian BON v. Beta"),
                      titleWidth = 400
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        
      
                        menuItem("Start here", tabName = "tab_intro", startExpanded = TRUE, icon = icon("flag-checkered"), 
                                 menuSubItem(" -- Goal", tabName = "tab_goal", icon = icon("flag-checkered")),
                                 menuSubItem(" -- Purpose", tabName = "tab_purp", icon = icon("lightbulb")), # 
                                 menuSubItem(" -- How it works", tabName = "tab_works", icon = icon("cogs")),
                                 menuSubItem(" -- Future", tabName = "tab_future", icon = icon("rocket")),
                                 menuSubItem(" -- Team", tabName = "tab_team", icon = icon("users-cog"))
                        ),
                        
                        menuItem("Find water", tabName = "tab_findwater", icon = icon("map-pin")),
                        menuItem("Assess quality", tabName = "tab_assess", icon = icon("poll")),
                        menuItem("Conserve water", tabName = "tab_forest", icon = icon("hand-holding-water"))
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
        .main-sidebar { font-size: 20px; }
      '))),
                      tabItems(
                        tabItem("tab_goal", 
                                div(style='width:100%;',
                                    includeMarkdown("goal.html")
                                )
                        ),
                        tabItem("tab_purp", includeMarkdown("purple.html")),
                        tabItem("tab_works", includeMarkdown("works.html")),
                        tabItem("tab_future", includeMarkdown("future.html")),
                        tabItem("tab_team", includeMarkdown("team.html")),
                        
                        
                        # UI draw pol   --------------- ^^
                        ######## UI Find water  ---------------
                        tabItem("tab_findwater",
                                #fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                  column(width = 12,
                                         fluidRow(
                                           column(width = 2, actionButton("go_find", "Find water!")),
                                           column(width = 10, verbatimTextOutput("voutdist") %>% withSpinner(color="#0dc5c1"))
                                         ),
                                         leafletOutput("findLeaf", height = "600px") %>% withSpinner(color="#0dc5c1")
                                         #leafletOutput("findLeaf", width = "90%", height = "90%")
                                  )
                                )
                        ),
                        
                        ######## UI Assess -----
                        tabItem("tab_assess",
                                #fluidRow(h3(' ')),
                                #h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 6, selectInput(inputId = "in_days", 
                                                                         label = "Días previos: ", choices =  c(30, 60, 90, 120), selected = 60)),
                                           column(width = 6, br(), actionButton("go_assess", "Assess"))),
                                         fluidRow(
                                           leafletOutput("assessLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")
                                         )
                                  ),
                                  column(width = 6, 
                                         valueBoxOutput("waterquality"),
                                         highchartOutput("assessPlot1", height = "400px") %>% withSpinner(color="#0dc5c1"), 
                                         highchartOutput("assessPlot2", height = "300px")%>% withSpinner(color="#0dc5c1"), 
                                         highchartOutput("assessFor", height = "300px")%>% withSpinner(color="#0dc5c1"),
                                         highchartOutput("assessPlot3", height = "300px")%>% withSpinner(color="#0dc5c1")
                                  )
                                )
                        ),
                        
                        tabItem("tab_forest",
                                #fluidRow(h3(' ')),
                                #h5(''),
                                fluidRow(
                                  column(width = 12,
                                         actionButton("go_forest", "Conserve the basin!"),
                                         tabsetPanel(type = 'pills',
                                                     tabPanel('Tree cover',
                                                              leafletOutput("treeLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")),
                                                     tabPanel('Deforestation',
                                                              leafletOutput("yearLeaf", height = "600px")%>% withSpinner(color="#0dc5c1"))
                                         )
                                  )
                                )
                        )
                      )
                    )
)


##### SERVER ----------------------
server <- function(input, output, session) {
  
  ##### Default widgets ----------------------
  
  
  output$assessFor <- renderHighchart({ NULL})
  output$assessPlot1 <- renderHighchart({ NULL})
  output$assessPlot2 <- renderHighchart({ NULL})
  output$assessPlot3 <- renderHighchart({ NULL})
  output$assessPlot1 <- renderHighchart({NULL})
  output$assessPlot2 <- renderHighchart({NULL})
  
  output$loadMapLL <- renderLeaflet({
    reactShp$leaf0
  })
  
  output$tablesprec <- output$tablespuicn <- output$tablespbiod <- renderDataTable(NULL)
  
  ##### Leaflets ----------------------
  
  output$findLeaf <- output$assessLeaf <- output$leafForest <- output$treeForest <- output$lossForest <- renderLeaflet({
    reactShp$leaf0 %>%
      leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
                                     rectangleOptions = FALSE, circleOptions = FALSE,
                                     markerOptions = FALSE, circleMarkerOptions = FALSE,
                                     editOptions = leaflet.extras::editToolbarOptions())
    #addDrawToolbar(editOptions = editToolbarOptions())
  })
  
  
  reactShp <- reactiveValues(shp = FALSE,
                             leaf0 = leaflet() %>% addTiles() %>% setView(lng = -74, lat = 4.6, zoom = 10) %>% 
                               addLayersControl(baseGroups = c("OpenStreetMap",
                                                               "Esri.WorldImagery",
                                                               'CartoDB.Positron', 'CartoDB.DarkMatter',
                                                               'OpenTopoMap'),
                                                options = layersControlOptions(collapsed = TRUE)) %>%
                               addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
                               addProviderTiles( "CartoDB.Positron", group = "CartoDB.Positron" ) %>%
                               addProviderTiles( "CartoDB.DarkMatter", group = "CartoDB.DarkMatter" ) %>%
                               addProviderTiles( "OpenTopoMap", group = "OpenTopoMap" )
                             
  )
  
  
  
  hc1 <<- hc2 <<- NULL 
  
  
  
  output$voutdist <- renderText({isolate("Place the map and then hit the button")})
  
  
  ##### Go find water  ----------------------  
  # input <- list(forestyearrng = c(2000, 2005), forestporcrng = c(25, 100), forestsour = 'ideam')
  
  ##### Reactive maps  ----------------------  
  observeEvent(input$findLeaf_click, {
    click <- input$findLeaf_click
    text<-paste("Lattitude:", round(click$lat, 4),
                "<br>Longtitude:", round(click$lng, 4),
                "<br>Date:", Sys.Date())
    proxy <- leafletProxy("findLeaf")
    proxy %>% clearPopups() %>%
      addPopups(click$lng, click$lat, text)
  })
  
  observeEvent(input$assessLeaf_click, {
    click <- input$assessLeaf_click
    text<-paste("Lattitude:", round(click$lat, 2),
                "<br>Longtitude:", round(click$lng, 4),
                "<br>Date:", Sys.Date())
    proxy <- leafletProxy("assessLeaf")
    proxy %>% clearPopups() %>%
      addPopups(click$lng, click$lat, text)
  })
  
  
  ##### Go find water  ----------------------  
  isolate(observeEvent(input$go_find,{
    
    so <- Sys.time()
    #polDraw <- input$leafForest_draw_new_feature
    isolate(output$findLeaf <- isolate(renderLeaflet({
      
      isolate(inClick <- isolate(input$findLeaf_click))
      #print(str(inClick))
      #inClick <- list(lat = 9, lng = 2, .nonce = .21)
      
      ptsCoord <<- c(lon = inClick$lng, lat = inClick$lat)
      #ptsCoord <<- c(lon = -74, lat = 4)
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
      spDf <<- SpatialPolygonsDataFrame(
        SpatialPolygons(
          list(
            Polygons(list(Polygon(matrix(unlist(
              basaoi_gi$features[[1]]$geometry$coordinates[[1]]
            ), ncol = 2, byrow = TRUE))), 1) # polgons
          )), data = data.frame(ID = 1), match.ID = FALSE)
      
      
      leafGee <<- leaflet() %>% leaflet() %>% addTiles() %>% 
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
                         options = layersControlOptions(collapsed = TRUE)) %>%
        addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
        addProviderTiles( "CartoDB.Positron", group = "CartoDB.Positron" ) %>%
        addProviderTiles( "CartoDB.DarkMatter", group = "CartoDB.DarkMatter" ) %>%
        addProviderTiles( "OpenTopoMap", group = "OpenTopoMap" )
      
      if ( Sys.info()["sysname"] == "Windows"){
        save(spDf, slDf, snpDf,ptsCoord, leafGee, file = 'temp.RData')
        # load('temp.RData')
      }
      print(so- Sys.time())
      leafGee 
      
    })))
    
    isolate(output$assessLeaf <- isolate(renderLeaflet({leafGee})))
    isolate(output$yearLeaf <- isolate(renderLeaflet({leafGee})))
    isolate(output$treeLeaf <- isolate(renderLeaflet({leafGee})))
    
    isolate(output$voutdist <- isolate(renderText({
      ## Return text
      paste0("Go to Lng: ", snpDf@coords[1,1], 
             ', Lat: ', snpDf@coords[1,2], 
             ", Distance to walk: ",
             round(pointDistance(snpDf, ptsCoord, lonlat = TRUE, allpairs=FALSE),2), 
             ' meters')
    })))
    
    readyLayer <<- TRUE
  }))
  
  
  ##### Go assess water  ----------------------  
  isolate(observeEvent(input$go_assess,{
    # print('ReadyLayer')
    # print(readyLayer)
    
    chirps <<- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")$select('precipitation'); # mm/hr
    temp <<- ee$ImageCollection("MODIS/006/MOD11A2")$select('LST_Day_1km') #  kelvin
    tree <<- ee$Image("UMD/hansen/global_forest_change_2019_v1_7")$select('treecover2000') 
    loss <<- ee$Image("UMD/hansen/global_forest_change_2019_v1_7")$select('lossyear') 
    ptsCoord <<- c(lon = inClick$lng, lat = inClick$lat)
    #ptsCoord <<- c(lon = -74, lat = 4)
    aoiPoint <<- ee$Geometry$Point(ptsCoord);
    basaoi <<- basWWF$filterBounds(aoiPoint);
    
    stat.i <- data.frame(today = Sys.Date(), end = Sys.Date())
    # input <- list(in_days = 60)
    #print(str(input$in_days))
    stat.i$start <- Sys.Date() - 30 - as.numeric(input$in_days)
    stat.i$start_s <- as.character(stat.i$start)
    stat.i$end_s <- as.character(stat.i$end)
    
    
    ####### CHIRPS ###### SUM ------
    
    # basaoi
    #print('Chirps:')
    #print(class(chirps))
    chirps.i <- chirps$filterDate(stat.i$start_s, stat.i$end_s)
    #print('a')
    chirps.red <- chirps.i$map(function(imag, red, sc, bn) {
      # imag <- chirps.i$first()
      val0 <- imag$reduceRegion(reducer = ee$Reducer$sum() ,
                                geometry = basaoi, scale = 5500)$get('precipitation')
      val <- ee$Algorithms$If(val0, val0, ee$Number(-9999))
      #val$getInfo()
      xy <- ee$Feature(NULL, list("val" = val))$#set("system:time_start", imag$get("system:time_start"))$ 
        set("precipitation", val)$set("system:index", imag$get("system:index"))
      return(xy )
    })
    
    (x1 <- chirps.red$aggregate_array('precipitation')) ; #
    system.time(i.ppt <- x1$getInfo())
    (x2 <- chirps.red$aggregate_array('system:index')) ; 
    system.time(i.dates <- x2$getInfo())
    
    # chirps.n <- chirps.i$first()$reduceRegion(reducer = ee$Reducer$count() ,
    #                                geometry = bas.g, scale = 5500)
    
    
    #### TEMP ###### MEAN
    
    temp.i <- temp$filterDate(stat.i$start_s, stat.i$end_s)
    temp.red <- temp.i$map(function(imag, red, sc, bn) {
      # imag <- temp.i$first()
      val <- imag$select('LST_Day_1km')$reduceRegion(reducer = ee$Reducer$mean() ,
                                                     geometry = basaoi, scale = 1000)$get('LST_Day_1km')
      val <- ee$Algorithms$If(val, val, ee$Number(-9999))
      xy <- ee$Feature(NULL, list("val" = val))$#set("system:time_start", imag$get("system:time_start"))$
        set("LST_Day_1km", val)$set("system:index", imag$get("system:index"))
      return(xy )
    })
    
    (x1 <- temp.red$aggregate_array('LST_Day_1km')) ; #
    system.time(i.temp <- x1$getInfo())
    (x2 <- temp.red$aggregate_array('system:index')) ;
    system.time(i.temp.dates <- x2$getInfo())
    
    
    
    
    
    
    ## GEE TS --------------
    tree.i <- tree$reduceRegion(
      reducer = ee$Reducer$histogram(maxBuckets =  10),
      geometry = basaoi,
      scale = 30)
    
    system.time(hist.tree <- tree.i$getInfo()) # faster than in cliping image
    
    #str(hist.tree$treecover2000$bucketMeans)
    #str(hist.tree$treecover2000)
    
    #### LOSS
    loss.i <- loss$reduceRegion(
      reducer = ee$Reducer$histogram(maxBuckets =  30),
      geometry = basaoi,
      scale = 30)
    
    hist.loss <- loss.i$getInfo()
    #str(hist.loss)
    
    
    baskm2 <- raster::area(spDf)/1000000
    treeDf <- data.frame(bins = hist.tree$treecover2000$bucketMeans,
                         his = hist.tree$treecover2000$histogram)
    treeDf$km2 <- treeDf$his/sum(treeDf$his)*baskm2
    treeDf$prop <- treeDf$km2/baskm2 # sum(treeDf$prop)
    
    lossDf <- data.frame(bins = unlist(hist.loss$lossyear$bucketMeans),
                         his = unlist(hist.loss$lossyear$histogram))
    
    lossDf$km2 <- lossDf$his/sum(lossDf$his)*baskm2
    lossDf$prop <- lossDf$his/baskm2
    
    (lossTs <- data.frame(loss = sum(treeDf$prop[treeDf$bins>1])*100 - cumsum(c(0,lossDf$prop[-1])),
                          yy = 2000 + lossDf$bins))
    
    
    ## GEE DF --------------
    
    # ## Chirps
    dchirps <- cbind.data.frame(ymd = unlist(i.dates), chirps = unlist(i.ppt))
    dchirps$chirps[dchirps$chirps == -9999] <- NA
    dchirps$ymd <- as.Date(dchirps$ymd, format = '%Y%m%d')
    dchirps$ym <- substr(0, 7, x = dchirps$ymd)
    
    
    dtemp <- data.frame(ymd = unlist(i.temp.dates), tmp = (unlist(i.temp) * 0.02) - 273.15)
    dtemp$tmp[which(dtemp == -999)] <- NA
    dtemp$ymd <- as.Date(dtemp$ymd, format = '%Y_%m_%d')
    dtemp$ym <- substr(0, 7, x = dtemp$ymd)
    
    
    ## TS  plots --------------
    
    output$assessFor <- renderHighchart({ 
      hcF <<- highchart() %>% hc_add_series(name = 'Area in forest (%)',
                                            type = "line",
                                            color = 'green',
                                            mapping = hcaes(x = yy, y = loss),
                                            data = lossTs) %>%
        hc_title(text = paste( 'Area:', round(baskm2,3), 'km2')) %>%
        hc_xAxis(title = list(text = paste('Year'))) %>%
        hc_exporting(enabled = TRUE)
      
      
    })
    
    #rain
    output$assessPlot1 <- renderHighchart({ 
      hc1 <<- highchart() %>% hc_add_series(name = 'mm/day',
                                            type = "line",
                                            color = 'blue',
                                            mapping = hcaes(x = ymd, y = chirps),
                                            data = dchirps) %>%
        hc_title(text = paste( 'Area:', round(baskm2,3), 'km2')) %>%
        hc_xAxis(title = list(text = paste('Year')),
                 labels = list(format = '{value:%Y-%m-%d}')) %>%
        hc_exporting(enabled = TRUE) %>% hc_yAxis(
          plotLines = list(
            list(color = "#074632",
                 width = 2,
                 value = mean(dchirps$chirps, na.rm = TRUE)), 
            list(color = "#fb6703",
                 width = 2,
                 value =  unname(quantile(dchirps$chirps, probs = c(.8), na.rm = TRUE))),
            list(color = "#fb6703",
                 width = 2,
                 value =  unname(quantile(dchirps$chirps, probs = c(.2), na.rm = TRUE))),
            list(color = "#f0260f ",
                 width = 2,
                 value =  unname(quantile(dchirps$chirps, probs = c(.05), na.rm = TRUE))),
            list(color = "#f0260f ",
                 width = 2,
                 value =  unname(quantile(dchirps$chirps, probs = c(1), na.rm = TRUE)))
          )
        )
    })
    
    
    output$assessPlot2 <- renderHighchart({ 
      dtemp$tmp_2 <- round(dtemp$tmp, 2)
      hc2 <<- highchart() %>% hc_add_series(name = '°C',
                                            type = "line",
                                            color = 'orange',
                                            mapping = hcaes(x = ymd, y = tmp_2),
                                            data = dtemp) %>%
        hc_title(text = paste( 'Area:', round(baskm2,3), 'km2')) %>%
        hc_xAxis(title = list(text = paste('Year')),
                 labels = list(format = '{value:%Y-%m-%d}')) %>%
        hc_exporting(enabled = TRUE) %>% hc_yAxis(
          plotLines = list(
            list(color = "#074632",
                 width = 2,
                 value = mean(dtemp$tmp_2, na.rm = TRUE)), 
            list(color = "#fb6703",
                 width = 2,
                 value =  unname(quantile(dtemp$tmp_2, probs = c(.8), na.rm = TRUE))),
            list(color = "#f0260f ",
                 width = 2,
                 value =  unname(quantile(dtemp$tmp_2, probs = c(1), na.rm = TRUE)))
          )
        )
      
      hc2
  })
  }))
  
  
  output$waterquality <- renderValueBox({
    d.time = 10
    valueBox(
      d.time ,
      "Water quality",
      icon = icon("percent"),
      color = if (d.time <= 10) {'red'}  else
        if(d.time > 10 & d.time <= 20 ) {'orange'} else
          if(d.time > 20 & d.time <= 40 ) {'yellow'} else
            if(d.time > 40 & d.time <= 60 ) {'lime'} else
              if(d.time > 60 & d.time <= 100 ) {'green'}
    )
  })
  
  ##### Go conserve water forest  ----------------------  
  
    
  isolate(observeEvent(input$go_forest,{
    isolate(output$treeLeaf <- isolate(renderLeaflet({
      
      if(!readyLayer ){
        output$assessPlot1 <- renderHighchart({
          hcNOP
        })
        treegee <- reactShp$leaf0
      } else {
        
        tree <<- ee$Image("UMD/hansen/global_forest_change_2019_v1_7")$select('treecover2000') 
        loss <<- ee$Image("UMD/hansen/global_forest_change_2019_v1_7")$select('lossyear') 
        
        #### TREE --- maps leaf
        
        Map$centerObject(basaoi)
        tree.mask <- tree$updateMask(loss$unmask(-999)$eq(-999))
        
        lltree0 <- Map$addLayer(tree$clip(basaoi), name = 'Tree cover0',
                               visParams = list(min = 0, max = 100, 
                                                palette = c("E6EEF5", "1E5A0D"))) 
        
        lltree <- Map$addLayer(tree.mask$clip(basaoi), name = 'Tree cover',
                               visParams = list(min = 0, max = 100, 
                                                palette = c("E6EEF5", "1E5A0D"))) 
        llloss <- Map$addLayer(loss$clip(basaoi), name = 'Deforestation',
                               visParams = list(min = 0, max = 20, 
                                                palette = c("B7D114", "D11439")))
        # lltree2day <- Map$addLayer(tree.clip2day, 
        #                        visParams = list(min = 0, max = 100, 
        #                                         palette = c("E6EEF5", "1E5A0D"))) 
        
        palTree <- colorNumeric(palette = c("#E6EEF5", "#1E5A0D"), domain = c(0, 100))
        palLoss <- colorNumeric(palette = c("#B7D114", "#D11439"), domain = 2000+c(0, 20))
        print('chek2')
        #centerObject
        # names(lltree) # from rgee
        #names(leafGee)
        #str(leafGee)
        # lltree$rgee
        # 
        # class(lltree)
        # class(leafGee)
        # leafGee <- Reduce('+',  list(mapBas, mapRiv, mapOrig, mapDest) )
        
        isolate(output$yearLeaf <- isolate(renderLeaflet({
          llloss
        })))
        
        if ( Sys.info()["sysname"] == "Windows" 
             #| any(grep('azure', Sys.info()["release"]))
        ){
          
        #treegee2 <-  (leafGee + lltree[[names(leafGee)]] + llloss[[names(leafGee)]] )
        treegee <- as(leafGee + lltree + lltree0 + llloss, Class = c("leaflet"))
          
        treegee <-  treegee %>% addLegend("bottomright", pal = palTree, values = c(0, 100), title = "Forest cover", opacity = 1) %>% 
          addLegend("bottomright", pal = palLoss, values = 2000+c(0, 20), title = "Def. year", opacity = 1)
        
        # tempNameLeaf <- paste0(outDir, '/', basename(tempfile()), '.RData')
        # save(treegee, file = tempNameLeaf)
        # load(tempNameLeaf)
          treegee
          print('chek3')
        } else {
          warning('linux rgee leaf output')
          
          treegee <<- lltree  # leafGee
        }
        print('chek1')
        
      } # close if
      treegee
    })
    ))
    
  }))
  
} # End server

shinyApp(ui, server)


