library(leaflet)
library(leaflet.extras)
library(shiny)
library(rgeos)
library(rgdal)
library(shinydashboard)
library(highcharter)
library(devtools)
library(shinycssloaders)
library(reshape2)
# if(!'reshape2' %in% rownames(installed.packages())){installed.packages('reshape2')}
# if(!'write.xlsx' %in% rownames(installed.packages())){install.packages('write.xlsx')}

# install.packages('rgeos')
# install.packages('leaflet.extras'
# install.packages('highcharter')

devtools::source_url("https://raw.githubusercontent.com/gonzalezivan90/biotablero_api/master/biotablero_fun.R")
# aws <- 'ec2-3-12-165-32.us-east-2.compute.amazonaws.com' # Prod
aws <- 'ec2-3-137-83-192.us-east-2.compute.amazonaws.com' # Prod
# aws_port <- ':8080'

prj_wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# setwd('C:/GoogleDrive/IAvH/V2/08_SSD/')

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
                      title = "(v. Beta) Integración de observaciones de la tierra para la toma de decisiones sobre biodiversidad en Colombia", 
                      #title = tags$a(#href='http://rsensus.org/en/', "Decision support sytem for Colombian BON v. Beta"),
                      titleWidth = 1000),
                    dashboardSidebar(
                      sidebarMenu(
                        # UI Panel  ---------------
                        
                        menuItem("Inicio", tabName = "intro"),
                        menuItem("Definir region de estudio", tabName = "draw") ,
                        
                        menuItem("Ecosistemas", tabName = "tab_ecosystem", startExpanded = TRUE,
                                 menuSubItem("Bosques", tabName = "in_forest"),
                                 menuSubItem("Corine Land Cover", tabName = "in_clc"),
                                 menuSubItem("Lista roja ecosistemas", tabName = "in_red"),
                                 menuSubItem("Región biotica", tabName = "in_biot"),
                                 menuSubItem("Biomas", tabName = "in_biom"),
                                 menuSubItem("Bosque seco tropical", tabName = "in_dry"),
                                 menuSubItem("Páramos", tabName = "in_param"),
                                 menuSubItem("Humedales", tabName = "in_wet")
                        ),
                        
                        menuItem("Manejo", tabName = "tab_managment", startExpanded = TRUE,
                                 menuSubItem("Áreas protegidas", tabName = "in_ap"),
                                 menuSubItem("Áreas colectivas", tabName = "in_cole"),
                                 menuSubItem("Manejo especial", tabName = "in_sma"),
                                 menuSubItem("Factor de Compensación", tabName = "in_comp")
                        ),
                        menuItem("Especies", tabName = "tab_species", startExpanded = TRUE,
                                 menuSubItem("UICN", tabName = "in_uicn"),
                                 menuSubItem("Biomodelos", tabName = "in_biod"),
                                 menuSubItem("GBIF", tabName = "in_rec")
                        ),
                        menuItem("Índices", tabName = "tab_index", startExpanded = TRUE,
                              #   menuSubItem("Índice de lista roja", tabName = "in_rli"),
                                 menuSubItem("Superficie y registros", tabName = "in_sur")
                        ),
                        menuItem("Contacto", tabName = "contact")
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
                        
                        tabItem("in_clc",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                  column(width = 6,
                                         fluidRow(
                                           column(width = 6, selectInput(inputId = "aoi_clc", 
                                                                         label = "Área de estudio: ", choices =  c('Dibujar'), selected = 'Dibujar')),
                                           column(width = 4, selectInput(inputId = "clc_lev", 
                                                                         label = "Nivel: ", choices =  c(1:3), selected = 1)),
                                           column(width = 2, actionButton("go_clc", "Ejecutar")) 
                                         ),
                                         leafletOutput("clcLeaf", height = "600px")
                                  ),
                                  
                                  column(width = 6, 
                                         highchartOutput("clcPlot1", height = "600px")%>% withSpinner(color="#0dc5c1")
                                         #, highchartOutput("clcPlot2", height = "600px")%>% withSpinner(color="#0dc5c1")
                                  )
                                )
                        ),
                        
                        tabItem("in_red",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_red", label = "Área de estudio: ", choices =  c('Dibujar'), selected = 'Dibujar')),
                                           column(width = 1, br(), actionButton("go_red", "Ejecutar"))
                                         ),
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           leafletOutput("redLeaf", height = "600px") %>% withSpinner(color="#0dc5c1"))),
                                  column(width = 6, highchartOutput("redPlot1", height = "600px")%>% withSpinner(color="#0dc5c1"),
                                         highchartOutput("redPlot2", height = "600px")%>% withSpinner(color="#0dc5c1")
                                  )
                                )
                        ),
                        
                        tabItem("in_biot",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_biot", label = "Área de estudio: ", choices =  c('Dibujar'), selected = 'Dibujar')),
                                           column(width = 1, br(), actionButton("go_biot", "Ejecutar"))
                                         ),
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           leafletOutput("biotLeaf", height = "600px")%>% withSpinner(color="#0dc5c1"))),
                                  column(width = 6, #highchartOutput("biotPlot1", height = "600px")%>% withSpinner(color="#0dc5c1"),
                                         highchartOutput("biotPlot2", height = "600px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        tabItem("in_biom",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_biom", label = "Área de estudio: ", choices =  c('Dibujar'), selected = 'Dibujar')),
                                           column(width = 1, br(), actionButton("go_biom", "Ejecutar"))
                                         ),
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           leafletOutput("biomLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")
                                         ))
                                  , 
                                  column(width = 6, # highchartOutput("biomPlot1", height = "600px")%>% withSpinner(color="#0dc5c1"),
                                         highchartOutput("biomPlot2", height = "600px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        tabItem("in_dry",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_dry", label = "Área de estudio: ", choices =  c('Dibujar'), selected = 'Dibujar')),
                                           column(width = 1, br(), actionButton("go_dry", "Ejecutar"))),
                                         fluidRow(
                                           leafletOutput("dryLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, 
                                         highchartOutput("dryPlot1", height = "400px") %>% withSpinner(color="#0dc5c1"), 
                                         highchartOutput("dryPlot2", height = "300px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        tabItem("in_param",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_param", label = "Área de estudio: ", choices =  c('Dibujar'), selected = 'Dibujar')),
                                           column(width = 1, br(), actionButton("go_param", "Ejecutar"))),
                                         fluidRow(
                                           leafletOutput("paramLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, 
                                         highchartOutput("paramPlot1", height = "400px") %>% withSpinner(color="#0dc5c1"), 
                                         highchartOutput("paramPlot2", height = "300px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        tabItem("in_wet",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_wet", label = "Área de estudio: ", choices =  c('Dibujar'), selected = 'Dibujar')),
                                           column(width = 1, br(), actionButton("go_wet", "Ejecutar"))),
                                         fluidRow(
                                           leafletOutput("wetLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, 
                                         #highchartOutput("wetPlot1", height = "400px") %>% withSpinner(color="#0dc5c1"), 
                                         highchartOutput("wetPlot2", height = "600px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        ######## UI Managment -----
                        
                        tabItem("in_ap",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_ap", label = "Área de estudio: ", choices =  c('Dibujar'), selected = 'Dibujar')),
                                           column(width = 1, br(), actionButton("go_ap", "Ejecutar"))),
                                         fluidRow(
                                           leafletOutput("apLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, 
                                         highchartOutput("apPlot1", height = "400px") %>% withSpinner(color="#0dc5c1"), 
                                         highchartOutput("apPlot2", height = "300px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        tabItem("in_cole",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_cole", label = "Área de estudio: ", choices =  c('Dibujar'), selected = 'Dibujar')),
                                           column(width = 1, br(), actionButton("go_cole", "Ejecutar"))),
                                         fluidRow(
                                           leafletOutput("coleLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, 
                                         highchartOutput("colePlot1", height = "400px") %>% withSpinner(color="#0dc5c1"), 
                                         highchartOutput("colePlot2", height = "300px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        tabItem("in_sma",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_sma", label = "Área de estudio: ", choices =  c('Dibujar'), selected = 'Dibujar')),
                                           column(width = 1, br(), actionButton("go_sma", "Ejecutar"))),
                                         fluidRow(
                                           leafletOutput("smaLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, 
                                         highchartOutput("smaPlot1", height = "400px") %>% withSpinner(color="#0dc5c1"), 
                                         highchartOutput("smaPlot2", height = "300px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
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
                        tabItem("in_biod",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 6, selectInput(inputId = "aoi_biod", label = "Área de estudio: ", choices =  c('Dibujar'), selected = 'Dibujar')),
                                           column(width = 3, br(), actionButton("go_biod", "Ejecutar")),
                                           column(width = 3, br(), downloadButton("biod_xls", "Descargar"))),
                                         fluidRow(
                                           leafletOutput("biodLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, dataTableOutput('tablespbiod') %>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        tabItem("in_rec",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 6, selectInput(inputId = "aoi_rec", label = "Área de estudio: ", choices =  c('Dibujar'), selected = 'Dibujar')),
                                           column(width = 3, br(), actionButton("go_rec", "Ejecutar")),
                                           column(width = 3, br(), downloadButton("recs_xls", "Descargar"))),
                                         fluidRow(
                                           leafletOutput("recLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, dataTableOutput('tablesprec') %>% withSpinner(color="#0dc5c1"))
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

# forest
# clc
# red
# biot
# biom
# dry
# param
# wet
# ap
# cole
# sma
# comp
# uicn
# biom
# rec
# sur
# rli


##### SERVER ----------------------
server <- function(input, output, session) {
  # leafForest, clcPlot1, clcPlot2, redPlot1, redPlot2, biotcPlot1, biotcPlot2, biomPlot1, biomPlot2, 
  # dryPlot1. dryPlot2, paramPlot1, paramPlot2, wetPlot1, wetPlot2, apPlot1, apPlot2, colePlot1, colePlot2,
  # smaPlot1, smaPlot2, compPlot1, compPlot2, uicnPlot1, uicnPlot2, biodPlot1, biodPlot2, recPlot1, recPlot2, 
  # rliPlot1, rliPlot2, surPlot1, surPlot2
  
  
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
  
  output$clcLeaf <- output$redLeaf <- output$biotLeaf <- output$biomLeaf <- output$wetLeaf <- 
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
  
  isolate(observeEvent(input$loadShp, {
    output$loadMapLL <- renderLeaflet({
      #values$total <- values$total + 1
      inFiles <- isolate(input$shapefile)
      #setwd('../app6_v0_Tabs/')
      #x0 <- inFiles; print(x0); print(class(x0)); print(str(x0))
      
      
      if ( class(inFiles) != "NULL" ){
        
        vtext <<- ''
        
        if ( nrow(inFiles) == 1){
          
          if(grepl('*\\.zip', inFiles$name)){ ## zip files
            outZip <- tempfile(tmpdir = outDir); dir.create(outZip)
            unzip(zipfile = inFiles$datapath, exdir = outZip)
            uZ <- list.files(outZip)
            #x0 <- uZ; print(x0); print(class(x0)); print(str(x0))
            shp <<- tryCatch(readOGR(outZip, layer = tools::file_path_sans_ext(uZ[1])), error = function (e) NULL)
          } else if (grepl('\\.SQLite|\\.gpkg|\\.GeoJSON', inFiles$name)){ ## single
            #save(inFiles, file = 'inFileSingle.RData'); 
            # shp <<- tryCatch(readOGR('C:/GoogleDrive/IAvH/V2/00_aois/_ATLANTICO-SANTO TOMÁS.shp'))
            
            shp <<- tryCatch(readOGR(inFiles$datapath[1]), error = function (e) NULL)
          }
        } else if ( nrow(inFiles) >= 3  & all(sapply(c('\\.shp', '\\.shx', '\\.dbf'), grep, inFiles$name))){ ## shp several
          #save(inFiles, file = 'inFileSeveral.RData');
          inFiles$datapath2 <- gsub('\\/[0-9]\\.', '/1.', inFiles$datapath)
          sapply(inFiles$datapath, USE.NAMES = F, function(x){
            file.rename(x,  gsub('\\/[0-9]\\.', '/1.', x) ) })
          
          shp <<- tryCatch(readOGR(dirname(inFiles$datapath2[1]),
                                   basename(tools::file_path_sans_ext(inFiles$datapath2[1]))), error = function (e) e)
        }
        #tryCatch(sapply(inFiles$datapath, file.remove ))
        
        if(class(shp) == 'SpatialPolygonsDataFrame'){
          if(nrow(shp) > 1 ){
            shp <- shp[1, ]
            vtext <<- 'Más de un polígono. Se usa sólo el primero.\n'
          }
          vtext <<- paste0(vtext, '\nProyección:', shp@proj4string@projargs)
          if(shp@proj4string@projargs != prj_wgs84){ 
            #print(paste0(' Proj old: ', shp@proj4string@projargs))
            shp <<- spTransform(shp, CRSobj = CRS(prj_wgs84)) 
            #print(paste0(' Proj new: ', shp@proj4string@projargs))
            vtext <<- paste0(vtext, '\nProyección no es geográfica WGS84. Intentando reproyectar.')
          }
          
          if( min(shp@bbox['x', ])>-180 & max(shp@bbox['x', ])<180 &
              min(shp@bbox['y', ])>-90 & max(shp@bbox['y', ])<90 )  #><
          {
            polArea <- raster::area(shp)/1000000
            if ( polArea <= 5000){ # Smaller than 5k km
              isShpLoad <<- TRUE
              reactShp$shp <- TRUE
              vtext <<- paste0(vtext, '\nPolígono cargado!')
            } else {
              isShpLoad <<- FALSE
              reactShp$shp <- FALSE
              vtext <<- paste0(vtext, '\nError: Polígono mayor a 5.000 Km. Subir un archivo con menor área')
            }
          } else{
            vtext <<- paste0(vtext, '\nError: Polígono fuera del límite global')
          }
        } else {
          vtext <<- "Error: Intente con otros archivos. No se pudo cargar el polígono"
        }
        
        if (isShpLoad){
          #print("   shp: "); print(shp)
          reactShp$shp <- TRUE
          reactShp$leaf0 <<- leaflet() %>% addTiles() %>% addPolygons(data = shp) %>%
            addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                             options = layersControlOptions(collapsed = FALSE)) %>%
            addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" )
          #output$loadMapLL <- renderLeaflet({reactShp$leaf0})
          # print("   reactShp: "); print(reactShp); print(str(reactShp))
          # save(reactShp, file = paste0('read.RData'))
          
          updateSelectInput(session, 'aoi_forest', choices = c('Dibujar', 'Capa'), selected = 'Capa')
          updateSelectInput(session, 'aoi_clc', choices = c('Dibujar', 'Capa'), selected = 'Capa')
          updateSelectInput(session, 'aoi_red', choices = c('Dibujar', 'Capa'), selected = 'Capa')
          updateSelectInput(session, 'aoi_biot', choices = c('Dibujar', 'Capa'), selected = 'Capa')
          updateSelectInput(session, 'aoi_biom', choices = c('Dibujar', 'Capa'), selected = 'Capa')
          updateSelectInput(session, 'aoi_param', choices = c('Dibujar', 'Capa'), selected = 'Capa')
          updateSelectInput(session, 'aoi_dry', choices = c('Dibujar', 'Capa'), selected = 'Capa')
          updateSelectInput(session, 'aoi_wet', choices = c('Dibujar', 'Capa'), selected = 'Capa')
          updateSelectInput(session, 'aoi_ap', choices = c('Dibujar', 'Capa'), selected = 'Capa')
          updateSelectInput(session, 'aoi_cole', choices = c('Dibujar', 'Capa'), selected = 'Capa')
          updateSelectInput(session, 'aoi_sma', choices = c('Dibujar', 'Capa'), selected = 'Capa')
          updateSelectInput(session, 'aoi_comp', choices = c('Dibujar', 'Capa'), selected = 'Capa')
          updateSelectInput(session, 'aoi_uicn', choices = c('Dibujar', 'Capa'), selected = 'Capa')
          updateSelectInput(session, 'aoi_rec', choices = c('Dibujar', 'Capa'), selected = 'Capa')
          updateSelectInput(session, 'aoi_biod', choices = c('Dibujar', 'Capa'), selected = 'Capa')
          updateSelectInput(session, 'aoi_sur', choices = c('Dibujar', 'Capa'), selected = 'Capa')
          
          
   
          
          ## Validate nchar
          gwkt_orig <<- gsub( ' ', '%20', paste0('POLYGON ((', 
                                          paste(apply(round(shp@polygons[[1]]@Polygons[[1]]@coords, 4), 1, 
                                                      paste, collapse = ' '), collapse = ', '), '))'))
          ## Simplify
          if(nchar(gwkt_orig) > 7795 ){
            vtext <<- paste0(vtext, '\nIntentando simplificar geometría del polígono')
            rng <- seq(0, 10)
            for(i in rng){ # i = 1
              tol.i <- as.numeric(i/1000)
              simpMun <- sp::SpatialPolygonsDataFrame(rgeos::gSimplify(shp, 
                                                                        tol = tol.i, 
                                                                        topologyPreserve = TRUE), 
                                                       data = data.frame(id = 1),match.ID = F)
              gwkt_simp <<- gsub( ' ', '%20', paste0('POLYGON((', 
                                                  paste(apply(round(simpMun@polygons[[1]]@Polygons[[1]]@coords, 4),
                                                              1, paste, collapse = ' '), collapse = ','), '))'))
              if (nchar(gwkt_simp) < 7795){
                gwkt_orig <<- gwkt_simp
                vtext <<- paste0(vtext, '\nPolígono simplificado')
                reactShp$leaf0 <<- leaflet() %>% addTiles() %>% addPolygons(data = simpMun) %>%
                  addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                                   options = layersControlOptions(collapsed = FALSE)) %>%
                  addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" )
                
                break
              }
            }
          }
          
          vtext <<- paste0(vtext, '\nNúmero de vértices del polígono: ', 
                           nrow(shp@polygons[[1]]@Polygons[[1]]@coords),
                           '\nNúmero de caracteres del polígono: ', 
                           nchar(gwkt_orig),
                           '\nÁrea en km2: ', round(polArea, 2))
        }
      }
      
      reactShp$leaf0
    })
    
    isolate(output$voutext <- renderText({isolate(gsub('^\n', '', vtext))}))
    
  }))
  
  ##### Go buttons  ----------------------
  output$voutext <- renderText({isolate("Cargue su polígono")})
  
  
  ##### Go buttons Forest  ----------------------  
  # input <- list(forestyearrng = c(2000, 2005), forestporcrng = c(25, 100), forestsour = 'ideam')
  
  isolate(observeEvent(input$go_forest,{
    readyLayer <- FALSE
    polDraw <- input$leafForest_draw_new_feature
    
    if( (!is.null(polDraw) & input$aoi_forest  == 'Dibujar') |
        (!is.null(shp) & input$aoi_forest  == 'Capa')) {
      
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_forest  == 'Dibujar'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', 
                                        paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
      } else if (!is.null(shp) & input$aoi_forest == 'Capa'){ 
        gwkt <<-  gwkt_orig  # gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
      }
      
      output$foresttrend <- renderHighchart({
        biotKm2 <- biotablero(server = 'web', webURL = aws, 
                              port = aws_port, endpoint = 'polsizekm2', pol = gwkt, printURL = FALSE)
        sprintf('Area: %s', biotKm2$polsizekm2)
        
        
        biotForYearString <- paste0(input$forestyearrng[1], ':', input$forestyearrng[2])
        biotForPorcString <- paste0(input$forestporcrng[1], ':', input$forestporcrng[2])
        
        # input <- list(forestsour = 'ideam')
        biotForest <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'forest',
                                 sour = input$forestsour, ebvstat = 'area',
                                 ebvyear=biotForYearString, ebvporcrange = biotForPorcString,
                                 pol = gwkt, printURL = TRUE)
        
        #save(biotForest, gwkt, biotForYearString, biotForPorcString , file = 'forestMet.RData')
        #load('forestMet.RData')
        #print('Done querying bt\n')
        
        if(class(biotForest) == 'list'){
          if(class(biotForest$result) == 'data.frame'){
            
            hc1 <<- highchart() %>% hc_add_series(name = 'Area (km2)',
                                                  type = "line",
                                                  mapping = hcaes(x = year, y = area),
                                                  data = biotForest$result) %>%
              hc_title(text = paste( 'Area:', biotKm2, ' km2')) %>%
              hc_xAxis(title = list(text = paste('Time: ',
                                                 (biotForest$params['time',])))) %>%
              hc_exporting(enabled = TRUE)
            
          }
        } else if (class(biotForest) == 'character' | class(biotForest)[1] == 'simpleError'){
          
          hc1 <<- highchart() %>% hc_add_series(
            name = 'Error',
            type = "line",
            mapping = hcaes(x = 1, y = 1)) %>%
            hc_title(text = paste( 'Area:', biotKm2, ' km2')) %>%
            hc_xAxis(title = list(text = unlist(biotForest[1]))) %>% 
            hc_exporting(enabled = TRUE)
        }
        hc1
      })
      # input$leafmap_draw_new_feature <- NULL
    }
  }))
  
  
  ##### Go buttons CLC  ----------------------  
  
  observeEvent(input$go_clc,{
    readyLayer <- FALSE
    polDraw <- input$clcLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_clc  == 'Dibujar') |
        (!is.null(shp) & input$aoi_clc  == 'Capa')) {
      #print('clc0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_clc  == 'Dibujar'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        # print('clc1A')
        #save(gwkt, coordMat, file='Polclc.RData') # load('Polclc.RData') 
      } else if (!is.null(shp) & input$aoi_clc  == 'Capa'){ 
        gwkt <<-  gwkt_orig  # gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        # print('clc1B')
        #save(gwkt, shp, file='PolParamB.RData'); load('gwkt.RData')        
      }
      
      output$clcPlot1 <- renderHighchart({
        biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                              endpoint = 'polsizekm2', pol = gwkt, printURL = FALSE)
        print(sprintf('Area: %s',biotKm2$polsizekm2))
        
        #input <- list(clc_lev = 1)
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port, rasterLayer = TRUE,
                                 endpoint = 'biotablero', metric = 'clc', clclevel = input$clc_lev,
                                 pol = gwkt, printURL = TRUE)
        
        # print(getwd());save(biotKm2, gwkt, biotMetric, file='metclc.RData') # load('metclc.RData')
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            print('plots clc')
            clcdf <- reshape2::melt(biotMetric$result)
            clcdf$name2 <- paste(clcdf$legend, clcdf$cod)
            
            hc1 <<- hchart(clcdf, "column", hcaes(x = legend, y = value, group = variable)) %>%
              hc_exporting(enabled = TRUE)  %>% hc_yAxis(title = list(text = 'Area (km2)')) %>% 
              hc_xAxis(title = list(text = 'Categoría CLC'))
            
            
            hc2 <<- hchart(clcdf, "line", hcaes(x = variable, y = value, group = legend)) %>%
              hc_title(text = paste( 'Área total:', biotKm2$polsizekm2, ' km2')) %>%
              hc_exporting(enabled = TRUE) %>% hc_yAxis(title = list(text = 'Area (km2)')) %>% 
              hc_xAxis(title = list(text = 'Year'))
            
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                  'Error: Intenta de nuevo')) #'Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      
      output$clcPlot2 <- renderHighchart({hc2})
    }
  }) ## ending go button
  
  
  ##### Go buttons red  ----------------------  
  
  observeEvent(input$go_red,{
    readyLayer <- FALSE
    polDraw <- input$redLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_red  == 'Dibujar') |
        (!is.null(shp) & input$aoi_red  == 'Capa')) {
      print('red0')
      # Valid option and layer
      if(!is.null(polDraw) & input$aoi_red  == 'Dibujar'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
      } else if (!is.null(shp) & input$aoi_red  == 'Capa'){ 
        gwkt <<-  gwkt_orig  # gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
      }
      
      output$redPlot1 <- renderHighchart({
        biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port, 
                              endpoint = 'polsizekm2', pol = gwkt, printURL = FALSE)
        print(sprintf('Area: %s',biotKm2$polsizekm2))
        
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'ecouicn', rasterLayer = TRUE,
                                 pol = gwkt, printURL = TRUE)
        #print(getwd());save(biotKm2, gwkt, biotMetric, file='metred.RData') # load('metred.RData')
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            hc1 <<- biotMetric$result %>%
              hchart('column', hcaes(x = name, y = km2), 'Lista roja de ecosistemas')  %>% 
              hc_xAxis(title = list(text = 'Lista roja de ecosistemas')) %>% hc_yAxis(title = list(text = 'Km2')) %>%
              hc_exporting(enabled = TRUE)
            
            pieCh <- data.frame(Km2 = biotMetric$result$km2, 
                                label = biotMetric$result$name)
            
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Área total:', biotKm2$polsizekm2, ' km2')) %>%
              hc_exporting(enabled = TRUE)
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL 
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                   #'Área total:', biotKm2$polsizekm2, ' km2', 
                                   #'Error:', unlist(biotMetric [1]), ''))
                                   'Error: Intenta de nuevo')) #'Error:', unlist(biotMetric[1]), ''))
          
        }
        hc1
      }) 
      
      output$redPlot2 <- renderHighchart({hc2})
    }
  }) ## ending go button
  
  
  
  ##### Go buttons biom  ----------------------  
  
  observeEvent(input$go_biom,{
    readyLayer <- FALSE
    polDraw <- input$biomLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_biom  == 'Dibujar') |
        (!is.null(shp) & input$aoi_biom  == 'Capa')) {
      print('biom0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_biom  == 'Dibujar'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_biom  == 'Capa'){ 
        gwkt <<-  gwkt_orig  # gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        
      }
      
      output$biomPlot2 <- renderHighchart({ # output$biomPlot1
        biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                              endpoint = 'polsizekm2', pol = gwkt, printURL = FALSE)
        print(sprintf('Area: %s',biotKm2$polsizekm2))
        
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'biome',
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        #save(biotMetric, file = 'metBiome.RData')
        
        #print('plots biom')
        #print(class(biotMetric))
        #print(biotMetric)
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            
            
            hc1 <<- biotMetric$result %>%
              hchart('column', hcaes(x = name, y = km2), 'Biomas')  %>% 
              hc_xAxis(title = list(text = 'Biomas')) %>% hc_yAxis(title = list(text = 'Km2')) %>%
              hc_exporting(enabled = TRUE)
            
            
            pieCh <- data.frame(Km2 = biotMetric$result$km2, 
                                label = biotMetric$result$name)
            
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Área total:', biotKm2$polsizekm2, ' km2')) %>%
              hc_exporting(enabled = TRUE)
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL 
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                  'Error: Intenta de nuevo')) #'Error:', unlist(biotMetric[1]), ''))
        } else if (class(biotMetric)[1] == 'character' | any(grep('ERROR: Polygon bigger than the threshold', biotMetric)) ){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                   'Error: Intenta con un polígono más pequeño')) #Error:', unlist(biotMetric[1]), ''))
        }
        
        
        hc2 # hc2
      }) 
      
      #output$biomPlot2 <- renderHighchart({hc2})
    }
  }) ## ending go button
  
  
  ##### Go buttons botic  ----------------------  
  
  observeEvent(input$go_biot,{
    readyLayer <- FALSE
    polDraw <- input$biotLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_biot  == 'Dibujar') |
        (!is.null(shp) & input$aoi_biot  == 'Capa')) {
      #print('biot0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_biot  == 'Dibujar'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_biot  == 'Capa'){ 
        gwkt <<-  gwkt_orig  # gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        
      }
      
      output$biotPlot2 <- renderHighchart({ # output$biotPlot1
        biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                              endpoint = 'polsizekm2', pol = gwkt, printURL = FALSE)
        print(sprintf('Area: %s',biotKm2$polsizekm2))
        
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'bioticreg',
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        # print(getwd());save(biotKm2, gwkt, biotMetric, file='metbiot.RData') # load('metbiot.RData')
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            print('plots biot')
            
            hc1 <<- biotMetric$result %>%
              hchart('column', hcaes(x = name, y = km2), 'Región biótica')  %>% 
              hc_xAxis(title = list(text = 'Región biótica')) %>% hc_yAxis(title = list(text = 'Km2')) %>%
              hc_exporting(enabled = TRUE)
            
            pieCh <- data.frame(Km2 = biotMetric$result$km2, 
                                label = biotMetric$result$name)
            
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Área total:', biotKm2$polsizekm2, ' km2')) %>%
              hc_exporting(enabled = TRUE)
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL 
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                  'Error: Intenta de nuevo')) #'Error:', unlist(biotMetric[1]), ''))
        } else if (class(biotMetric)[1] == 'character' | any(grep('ERROR: Polygon bigger than the threshold', biotMetric)) ){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                   'Error: Intenta con un polígono más pequeño')) #Error:', unlist(biotMetric[1]), ''))
        }
        hc2 # hc1
      }) 
      
      #output$biotPlot2 <- renderHighchart({hc2})
    }
  }) ## ending go button
  
  
  
  ##### Go buttons params  ----------------------  
  
  observeEvent(input$go_param,{
    readyLayer <- FALSE
    polDraw <- input$paramLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_param  == 'Dibujar') |
        (!is.null(shp) & input$aoi_param  == 'Capa')) {
      print('Param0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_param  == 'Dibujar'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        print('Param1A')
      } else if (!is.null(shp) & input$aoi_param  == 'Capa'){ 
        gwkt <<-  gwkt_orig  # gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        print('Param1B')   
      }
      
      output$paramPlot1 <- renderHighchart({
        biotKm2 <- biotablero(server = 'web', webURL = aws, 
                              port = aws_port, endpoint = 'polsizekm2', pol = gwkt, printURL = FALSE)
        sprintf('Area: %s', biotKm2$polsizekm2)
        
        #print(getwd());save(biotKm2, gwkt, file='metParam.RData')
        # load('metParam.RData')
        
        
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'param',
                                 pol = gwkt, printURL = TRUE)
        
        if(class(biotMetric)[1] == 'list'){
          
          if(class(biotMetric$result) == 'data.frame'){
            print('plots Param')
            hc1 <<- biotMetric$result %>%
              hchart('column', hcaes(x = param, y = km2), name = 'Km2') %>%
              hc_yAxis(title = list(text = 'Km2')) %>% 
              hc_xAxis(title = list(text = 'Paramos'))
            
            pieCh <- data.frame(Km2 = c(sum(biotMetric$result$km2), biotKm2$polsizekm2 - sum(biotMetric$result$km2)), 
                                label = c('Paramo', 'No Paramo')) 
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            # hc2 <<- highchart() %>% hc_chart(type = "pie") %>% 
            #   hc_add_series_labels_values(labels = pieCh$label, values = pieCh$perc) %>% 
            #   hc_xAxis(title = list(text = 'Paramo')) %>% hc_yAxis(title = list(text = 'Km2'))
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Área total:', biotKm2$polsizekm2, ' km2'))
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL
          }
        } else if (class(biotForest) == 'character' | class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                  'Error: Intenta de nuevo')) #'Error:', unlist(biotMetric[1]), ''))
        } else if (class(biotMetric)[1] == 'character' | any(grep('ERROR: Polygon bigger than the threshold', biotMetric)) ){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                   'Error: Intenta con un polígono más pequeño')) #Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      output$paramPlot2 <- renderHighchart({hc2})
    }
  })
  
  ##### Go buttons dry forest  ----------------------  
  
  observeEvent(input$go_dry,{
    readyLayer <- FALSE
    polDraw <- input$dryLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_dry  == 'Dibujar') |
        (!is.null(shp) & input$aoi_dry  == 'Capa')) {
      print('Dry0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_dry  == 'Dibujar'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        #print('Dry1A')
        #save(gwkt, coordMat, file='PolParamADry.RData') # load('PolParamADry.RData')
      } else if (!is.null(shp) & input$aoi_dry  == 'Capa'){ 
        gwkt <<-  gwkt_orig  # gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        #print('Dry1B')
        #save(gwkt, shp, file='PolParamB.RData')
      }
      
      output$dryPlot1 <- renderHighchart({
        biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                              endpoint = 'polsizekm2', pol = gwkt, printURL = FALSE)
        print(sprintf('Area: %s',biotKm2$polsizekm2))
        
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'tropdryforest',
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        
        #print(getwd());save(biotKm2, gwkt, biotMetric, file='metDry.RData') # load('metDry.RData')
        #load('C:/GoogleDrive/IAvH/V2/Others/polyg_draw/app6_v0_Tabs/metDry.RData')
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            print('plots Dry')
            
            hc1 <<- biotMetric$result %>%
              hchart('column', hcaes(x = name, y = km2), 'Bosque seco')  %>% 
              hc_xAxis(title = list(text = 'Bosque seco')) %>% hc_yAxis(title = list(text = 'Km2'))
            
            
            pieCh <- data.frame(Km2 = c(sum(biotMetric$result$km2), 
                                        biotKm2$polsizekm2 - sum(biotMetric$result$km2)), 
                                label = c('Bosque seco', 'No Bosque'))
            
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            # hc2 <<- highchart() %>% hc_chart(type = "pie") %>% 
            #   hc_add_series_labels_values(labels = pieCh$label, values = pieCh$perc) %>% 
            #   hc_xAxis(title = list(text = 'Paramo')) %>% hc_yAxis(title = list(text = 'Km2'))
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Área total:', biotKm2$polsizekm2, ' km2'))
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL
            
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                  'Error: Intenta de nuevo')) #'Error:', unlist(biotMetric[1]), ''))
        } else if (class(biotMetric)[1] == 'character' | any(grep('ERROR: Polygon bigger than the threshold', biotMetric)) ){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                   'Error: Intenta con un polígono más pequeño')) #Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      
      output$dryPlot2 <- renderHighchart({hc2})
    }
  })
  
  
  
  ##### Go buttons wetlands  ----------------------  
  
  observeEvent(input$go_wet,{
    readyLayer <- FALSE
    polDraw <- input$wetLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_wet  == 'Dibujar') |
        (!is.null(shp) & input$aoi_wet  == 'Capa')) {
      print('wet0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_wet  == 'Dibujar'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        #print('wet1A')
        #save(gwkt, coordMat, file='PolParamAwet.RData') # load('PolParamAwet.RData')
      } else if (!is.null(shp) & input$aoi_wet  == 'Capa'){ 
        gwkt <<-  gwkt_orig  # gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        #print('wet1B')
        #save(gwkt, shp, file='PolParamB.RData') 
        #load('PolParamB.RData')
      }
      
      output$wetPlot2 <- renderHighchart({ #
        biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                              endpoint = 'polsizekm2', pol = gwkt, printURL = FALSE)
        print(sprintf('Area: %s',biotKm2$polsizekm2))
        
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'hum',
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        
        #print(getwd());save(biotKm2, gwkt, biotMetric, file='metwet.RData') # load('metwet.RData')
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            print('plots wet')
            
            hc1 <<- biotMetric$result %>%
              hchart('column', hcaes(x = name, y = km2), 'Humedales')  %>% 
              hc_xAxis(title = list(text = 'Humedales')) %>% hc_yAxis(title = list(text = 'Km2'))  %>%
              hc_exporting(enabled = TRUE)
            
            
            pieCh <- data.frame(Km2 = biotMetric$result$km2, 
                                label = biotMetric$result$name)
            
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Área total:', biotKm2$polsizekm2, ' km2'))  %>%
              hc_exporting(enabled = TRUE)
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL 
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                  'Error: Intenta de nuevo')) #'Error:', unlist(biotMetric[1]), ''))
        } else if (class(biotMetric)[1] == 'character' | any(grep('ERROR: Polygon bigger than the threshold', biotMetric)) ){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                   'Error: Intenta con un polígono más pequeño')) #Error:', unlist(biotMetric[1]), ''))
        }
        hc2
      }) 
      # output$wetPlot2 <- renderHighchart({hc2})
    }
  })
  
  
  ##### Go buttons ap  ----------------------  
  
  observeEvent(input$go_ap,{
    readyLayer <- FALSE
    polDraw <- input$apLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_ap  == 'Dibujar') |
        (!is.null(shp) & input$aoi_ap  == 'Capa')) {
      print('ap0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_ap  == 'Dibujar'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_ap  == 'Capa'){ 
        gwkt <<-  gwkt_orig  # gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        
      }
      
      output$apPlot1 <- renderHighchart({
        biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                              endpoint = 'polsizekm2', pol = gwkt, printURL = FALSE)
        print(sprintf('Area: %s',biotKm2$polsizekm2))
        
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'protectareas',
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        
        #print(getwd()); save(biotKm2, gwkt, biotMetric, file='metap.RData') # load('metap.RData')
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            print('plots biot')
            
            hc1 <<- biotMetric$result %>%
              hchart('column', hcaes(x = protected, y = km2, 
                                     color = category_p, 
                                     group = category_p, fill = category_p), 'Áreas protegidas')  %>% 
              hc_xAxis(title = list(text = 'Áreas protegidas')) %>% hc_yAxis(title = list(text = 'Km2')) %>%
              hc_exporting(enabled = TRUE)
            
            pieCh <- data.frame(Km2 = biotMetric$result$km2, 
                                label = biotMetric$result$protected)
            
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Área total:', biotKm2$polsizekm2, ' km2')) %>%
              hc_exporting(enabled = TRUE)
            
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL 
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                  'Error: Intenta de nuevo')) #'Error:', unlist(biotMetric[1]), ''))
        } else if (class(biotMetric)[1] == 'character' | any(grep('ERROR: Polygon bigger than the threshold', biotMetric)) ){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                   'Error: Intenta con un polígono más pequeño')) #Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      
      output$apPlot2 <- renderHighchart({hc2})
    }
  }) ## ending go button
  
  ##### Go buttons cole  ----------------------  
  
  observeEvent(input$go_cole,{
    readyLayer <- FALSE
    polDraw <- input$coleLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_cole  == 'Dibujar') |
        (!is.null(shp) & input$aoi_cole  == 'Capa')) {
      print('cole0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_cole  == 'Dibujar'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_cole  == 'Capa'){ 
        gwkt <<-  gwkt_orig  # gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        
      }
      
      output$colePlot1 <- renderHighchart({
        biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                              endpoint = 'polsizekm2', pol = gwkt, printURL = FALSE)
        print(sprintf('Area: %s',biotKm2$polsizekm2))
        
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'colectareas',
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        # print(getwd()); metName <- 'metcolle.RData'; save(biotKm2, biotMetric, file=metName) # sss <- load(metName)
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            print('plots cole')
            
            hc1 <<- biotMetric$result %>%
              hchart('column', hcaes(x = name, y = km2, group = terType), 'Territorios colectivas')  %>% 
              hc_xAxis(title = list(text = 'Territorios colectivas')) %>% hc_yAxis(title = list(text = 'Km2')) %>%
              hc_exporting(enabled = TRUE)
            
            pieCh <- data.frame(Km2 = biotMetric$result$km2, 
                                label = biotMetric$result$name)
            
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Área total:', biotKm2$polsizekm2, ' km2')) %>%
              hc_exporting(enabled = TRUE)
            
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL 
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                  'Error: Intenta de nuevo')) #'Error:', unlist(biotMetric[1]), ''))
        } else if (class(biotMetric)[1] == 'character' | any(grep('ERROR: Polygon bigger than the threshold', biotMetric)) ){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                   'Error: Intenta con un polígono más pequeño')) #Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      
      output$colePlot2 <- renderHighchart({hc2})
    }
  }) ## ending go button
  
  
  
  ##### Go buttons sma  ----------------------  
  
  observeEvent(input$go_sma,{
    readyLayer <- FALSE
    polDraw <- input$smaLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_sma  == 'Dibujar') |
        (!is.null(shp) & input$aoi_sma  == 'Capa')) {
      print('sma0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_sma  == 'Dibujar'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_sma  == 'Capa'){ 
        gwkt <<-  gwkt_orig  # gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        
      }
      
      output$smaPlot1 <- renderHighchart({
        biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                              endpoint = 'polsizekm2', pol = gwkt, printURL = FALSE)
        print(sprintf('Area: %s',biotKm2$polsizekm2))
        
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'sma',
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
         # print(getwd()); metName <- 'metsma.RData'; save(biotKm2, biotMetric, file=metName) # sss <- load(metName)
        # print(biotMetric)
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'list'){
            
            print('plots sma')
            
            # biotMetric$result$result2$management 
            # iconv(biotMetric$result$result2$management, from = 'utf8', to = 'ascii', sub='')
            # iconv(biotMetric$result$result2$management, from = 'latin1', to = 'utf8', sub='')
            
            # iconv(iconv(biotMetric$result$result2$management, to = 'utf8'), to = 'ascii')
            # iconv(iconv(biotMetric$result$result2$management, from = 'ascii'), to = 'latin1')
            # iconv(iconv(biotMetric$result$result2$management, from = 'utf8', to = 'latin1'), to = 'latin1')
            
            biotMetric$result$result2$name2 <- paste0(biotMetric$result$result2$management, '/', biotMetric$result$result2$category)
            
            
            hc1 <<- biotMetric$result$result2 %>%
              hchart('column', hcaes(x = management, y = km2, group = category), 'Áreas de manejo especial')  %>% 
              hc_xAxis(title = list(text = 'Áreas de manejo especial')) %>% hc_yAxis(title = list(text = 'Km2')) %>%
              hc_exporting(enabled = TRUE)
            
            pieCh <- data.frame(Km2 = biotMetric$result$result2$km2, 
                                label = biotMetric$result$result2$name2)
            
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Área total:', biotKm2$polsizekm2, ' km2')) %>%
              hc_exporting(enabled = TRUE)
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL 
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                  'Error: Intenta de nuevo')) #'Error:', unlist(biotMetric[1]), ''))
        } else if (class(biotMetric)[1] == 'character' | any(grep('ERROR: Polygon bigger than the threshold', biotMetric)) ){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                   'Error: Intenta con un polígono más pequeño')) #Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      
      output$smaPlot2 <- renderHighchart({hc2})
    }
  }) ## ending go button
  
  
  
  ##### Go buttons comp  ----------------------  
  
  observeEvent(input$go_comp,{
    readyLayer <- FALSE
    polDraw <- input$compLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_comp  == 'Dibujar') |
        (!is.null(shp) & input$aoi_comp  == 'Capa')) {
      print('comp0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_comp  == 'Dibujar'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_comp  == 'Capa'){ 
        gwkt <<-  gwkt_orig  # gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        
      }
      
      output$compPlot1 <- renderHighchart({
        biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                              endpoint = 'polsizekm2', pol = gwkt, printURL = FALSE)
        print(sprintf('Area: %s',biotKm2$polsizekm2))
        
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'faccomp',
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        # print(getwd()); metName <- 'metcompfactor.RData'; save(biotKm2, biotMetric, file=metName) # load(metName)
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            print('plots biot')
            biotMetric$result$name2 <- paste(biotMetric$result$name, biotMetric$result$factor)
            
            hc1 <<- biotMetric$result %>%
              hchart('column', hcaes(x = name2, y = km2), 'Factor de compensación')  %>% 
              hc_xAxis(title = list(text = 'Factor de compensación')) %>% hc_yAxis(title = list(text = 'Km2')) %>%
              hc_exporting(enabled = TRUE)
            
            pieCh <- data.frame(Km2 = biotMetric$result$km2, 
                                label = biotMetric$result$name2)
            
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Área total:', biotKm2$polsizekm2, ' km2')) %>%
              hc_exporting(enabled = TRUE)
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL 
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                  'Error: Intenta de nuevo')) #'Error:', unlist(biotMetric[1]), ''))
        } else if (class(biotMetric)[1] == 'character' | any(grep('ERROR: Polygon bigger than the threshold', biotMetric)) ){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                   'Error: Intenta con un polígono más pequeño')) #Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      
      output$compPlot2 <- renderHighchart({hc2})
    }
  }) ## ending go button
  
  
  ### Tables ---------
  ##### Go buttons uicn  ----------------------  
  
  observeEvent(input$go_uicn,{
    readyLayer <- FALSE
    polDraw <- input$uicnLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_uicn  == 'Dibujar') |
        (!is.null(shp) & input$aoi_uicn  == 'Capa')) {
      print('uicn0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_uicn  == 'Dibujar'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_uicn  == 'Capa'){ 
        gwkt <<-  gwkt_orig  # gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        
      }
      output$tablespuicn <- renderDataTable(options = list(pageLength = 10), {
        biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                              endpoint = 'polsizekm2', pol = gwkt, printURL = FALSE)
        print(sprintf('Area: %s',biotKm2$polsizekm2))
        
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'species', sour ='uicn',
                                 spFormat = 'list', pol = gwkt, printURL = TRUE)
        #print(getwd()); metName <- 'metuicn.RData'; save(biotKm2,gwkt, biotMetric, file=metName) # load(metName)
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            print('plots uicn')
            dfsp <- data.frame(species = biotMetric$result)
            dfsp$species.species <- NULL
            colnames(dfsp) <- gsub('species\\.', '', gsub('species\\.y$', 'species', colnames(dfsp)))
          }
        } else if(class(biotMetric)[1] == 'simpleError'){
            dfsp <- data.frame(id = 0, error = 'error')
        } else if(class(biotMetric) == 'character'){
          if(any(grep('ERROR: Polygon bigger than the threshold', biotMetric))){
            dfsp <- data.frame(id = 0, error = "Error: Polígono mayor al tamaño permitido (5000 km2)")
          }
        }
        xls_uicn <<- dfsp 
        print(head(dfsp))
        dfsp
      })
    }
  }) ## ending go button
  
  ##### Go buttons biod  ----------------------  
  
  observeEvent(input$go_biod,{
    readyLayer <- FALSE
    polDraw <- input$biodLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_biod  == 'Dibujar') |
        (!is.null(shp) & input$aoi_biod  == 'Capa')) {
      print('biod0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_biod  == 'Dibujar'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_biod  == 'Capa'){ 
        gwkt <<-  gwkt_orig  # gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        
      }
      dfsp <- data.frame(sp = 0)
      output$tablespbiod <- renderDataTable(options = list(pageLength = 10), {
        biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                              endpoint = 'polsizekm2', pol = gwkt, printURL = FALSE)
        print(sprintf('Area: %s',biotKm2$polsizekm2))
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'species',
                                 sour = 'biomod', spFormat = 'list', 
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        #print(getwd()); metName <- 'metbiomod.RData'; save(biotKm2,gwkt, biotMetric, file=metName) # load(metName)
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            print('plots biod')
            dfsp <- biotMetric$result[c(
              c('scientificName', 'speciesBlank', 'genusOriginal', 'specificEpithetOriginal', 
                'scientificNameAuthorship', 'kingdom', 'phylum', 'class', 'order', 'family', 'genus', 
              'acceptedNameUsage', 'specificEpithet', 'dbSource', 'TaxVerifSource', 
              'Invasive', 'UICN', 'Endemic', 'Biomodelos', 'level')
            )]
          } else {
            dfsp <- data.frame(error = 'error')
          }
        } else if(class(biotMetric)[1] == 'simpleError'){
          dfsp <- data.frame(error = 'error')
        } else if(class(biotMetric) == 'character'){
          if(any(grep('ERROR: Polygon bigger than the threshold', biotMetric))){
            dfsp <- data.frame(id = 0, error = "Error: Polígono mayor al tamaño permitido (5000 km2)")
          }
        }
        xls_biod <<- dfsp
        dfsp <<- dfsp[, colnames(dfsp) %in% c('error', 'scientificName', 'class', 'order', 'family')]
        print(head(dfsp))
        dfsp
      })
    }
  }) ## ending go button
  
  
  ##### Go buttons rec  ----------------------  
  
  observeEvent(input$go_rec,{
    readyLayer <- FALSE
    polDraw <- input$recLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_rec  == 'Dibujar') |
        (!is.null(shp) & input$aoi_rec  == 'Capa')) {
      print('rec0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_rec  == 'Dibujar'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_rec  == 'Capa'){ 
        gwkt <<-  gwkt_orig  # gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
      }
      
      dfsp <- data.frame(sp = 0)
      output$tablesprec <- renderDataTable(options = list(pageLength = 10), {
        biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                              endpoint = 'polsizekm2', pol = gwkt, printURL = FALSE)
        print(sprintf('Area: %s',biotKm2$polsizekm2))
        
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'species',
                                 sour = 'records', spFormat = 'list', 
                                 pol = gwkt, printURL = TRUE)
        #timeMark <- gsub('[[:punct:]]| ', '', format(as.POSIXct(Sys.time(), tz="CET"), tz="America/Bogota",usetz=TRUE))
        #print(getwd()); metName <- paste0('metrecords_', timeMark ,'.RData'); save(biotKm2, biotMetric, gwkt, file=metName) 
        # load(metName)
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            print('plots recs')
            dfsp <- biotMetric$result
          } else {
            dfsp <- data.frame(error = 'error')
          }
        } else if(class(biotMetric)[1] == 'simpleError'){
          dfsp <- data.frame(error = 'error')
        } else if(class(biotMetric) == 'character'){
          if(any(grep('ERROR: Polygon bigger than the threshold', biotMetric))){
            dfsp <- data.frame(id = 0, error = "Error: Polígono mayor al tamaño permitido (5000 km2)")
          }
        }
        xls_recs <<- dfsp[1:(min(500, nrow(dfsp))), ]
        print(head(dfsp))
        dfsp <<- dfsp[, colnames(dfsp) %in% c('error', 'species', 'decimalLatitude', 
        'decimalLongitude', 'class', 'order', 'family')]
        dfsp
        
      })
    }
  }) ## ending go button
  
  
  
  ##### Go buttons sur  ----------------------  
  
  observeEvent(input$go_sur,{
    readyLayer <- FALSE
    polDraw <- input$surLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_sur  == 'Dibujar') |
        (!is.null(shp) & input$aoi_sur  == 'Capa')) {
      print('sur0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_sur  == 'Dibujar'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_sur  == 'Capa'){ 
        gwkt <<-  gwkt_orig  # gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        
      }
      
      output$surPlot1 <- renderHighchart({
        
        #sizeCell <- c(1, 2, 5, 10, 20, 50, 100, 200, 500)[c(1, 4, 25, 100, 400, 2500, 10000, 40000, 250000) %in% input$sur_size]
        sizeCell <- input$sur_size
        
        biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                              endpoint = 'polsizekm2', pol = gwkt, printURL = FALSE) 
        print(sprintf('Area: %s',biotKm2$polsizekm2))
        
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'surface',
                                 cellSize = sizeCell, 
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        #print(getwd()); metName <- 'metsurf.RData'; save(biotKm2, biotMetric, gwkt, file=metName) 
        #load(metName)
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'list'){
            
            print('plots sur')
            
            hc1 <<- data.frame(var = 'Surface', per = biotMetric$result$percentage) %>%
              hchart('column', hcaes(x = var, y = per), 'Superficie con información')  %>%
              hc_xAxis(title = list(text = 'Superficie con información')) %>% 
              hc_yAxis(title = list(text = '% de pixeles con registros biológicos ')) %>%
              hc_exporting(enabled = TRUE) %>% hc_yAxis(max = 100)
            
            
            
          } else if(length(biotMetric$result) == 0){
            hc1 <<-  hcNULL 
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                  'Error: Intenta de nuevo')) #'Error:', unlist(biotMetric[1]), ''))
        } else if (class(biotMetric)[1] == 'character' | any(grep('ERROR: Polygon bigger than the threshold', biotMetric)) ){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( '', # error
                                   'Error: Intenta con un polígono más pequeño')) #Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      
    }
  }) ## ending go button
  
  
  ##### Go buttons rli  ----------------------  
  
  observeEvent(input$go_rli,{
    readyLayer <- FALSE
    
    output$rliPlot1 <- renderHighchart({
      biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                               endpoint = 'biotablero', metric = 'rli',
                               printURL = TRUE, rasterLayer = TRUE)
      
      if(class(biotMetric)[1] == 'list'){
        if(names(biotMetric$result) %in% 'redListIndex'){
          
          print('plots rli')
          rlidf <- reshape2::melt(biotMetric$result$redListIndex)
          rlidf$variable <- as.numeric(as.character(rlidf$variable))
          rlidf <- rlidf[order(rlidf$variable), ]
          hc1 <<- rlidf %>%
            hchart('line', hcaes(x = variable, y = value, color = 'Grupo', group = 'Grupo', fill = 'Grupo'), 'RLI')  %>% 
            hc_xAxis(title = list(text = 'Índice de lista roja')) %>% hc_yAxis(title = list(text = 'Km2'))  %>%
            hc_exporting(enabled = TRUE) 
          
        } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
          hc1  <<- hcNULL 
        }
      } else if (class(biotMetric)[1] == 'simpleError'){
        hc1 <<-  hcBIG %>%
          hc_title(text = paste( '', # error
                                'Error: Intenta de nuevo')) #'Error:', unlist(biotMetric[1]), ''))
      } 
      hc1
    }) 
  }) ## ending go button
  
  
  # ##### Go buttons XXX  ----------------------  
  # 
  # observeEvent(input$go_XXX,{
  #   readyLayer <- FALSE
  #   polDraw <- input$XXXLeaf_draw_new_feature
  #   if( (!is.null(polDraw) & input$aoi_XXX  == 'Dibujar') |
  #       (!is.null(shp) & input$aoi_XXX  == 'Capa')) {
  #     print('XXX0')
  #     #Valid option and layer
  #     if(!is.null(polDraw) & input$aoi_XXX  == 'Dibujar'){
  #       coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
  #       gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
  #       
  #     } else if (!is.null(shp) & input$aoi_XXX  == 'Capa'){ 
  #       gwkt <<-  gwkt_orig  # gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
  #       
  #     }
  #     
  #     biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
  #                           endpoint = 'polsizekm2', pol = gwkt, printURL = FALSE)
  #     print(sprintf('Area: %s',biotKm2$polsizekm2))
  #     
  #     output$XXXPlot1 <- renderHighchart({
  #       biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
  #                                endpoint = 'biotablero', metric = 'XXX',
  #                                pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
  #       
  #       if(class(biotMetric)[1] == 'list'){
  #         if(class(biotMetric$result) == 'data.frame'){
  #           
  #           print('plots XXX')
  #           
  #           hc1 <<- biotMetric$result %>%
  #             hchart('column', hcaes(x = name, y = km2), 'Bosque seco')  %>% 
  #             hc_xAxis(title = list(text = 'Bosque seco')) %>% hc_yAxis(title = list(text = 'Km2'))
  #           
  #           
  #           pieCh <- data.frame(Km2 = c(sum(biotMetric$result$km2), 
  #                                       biotKm2$polsizekm2 - sum(biotMetric$result$km2)), 
  #                               label = c('Bosque seco', 'No Bosque'))
  #           
  #           pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
  #           pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
  #           
  #           hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
  #             hc_title(text = paste( 'Área total:', biotKm2$polsizekm2, ' km2'))
  #           
  #         } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
  #           hc1 <<- hc2 <<- hcNULL 
  #         }
  #       } else if (class(biotMetric)[1] == 'simpleError'){
  #         hc1 <<- hc2 <<- hcBIG %>%
  #           hc_title(text = paste( '', # error
  #                                 'Error: Intenta de nuevo')) #'Error:', unlist(biotMetric[1]), ''))
  #       }
  #       hc1
  #     }) 
  #     
  #     output$XXXPlot2 <- renderHighchart({hc2})
  #   }
  # }) ## ending go button
  
  
  output$uicn_xls <- downloadHandler(
    filename = paste0('spList_UICN_', Sys.Date(),'.csv'), 
    content = function(file) {
      write.csv(xls_uicn, file, row.names = FALSE)
    })
  
  output$biod_xls <- downloadHandler(
    filename = paste0('spList_Biomodelos_', Sys.Date(),'.csv'), 
    content = function(file) {
      write.csv(xls_biod,  file, row.names = FALSE)
    })
  
  output$recs_xls <- downloadHandler(
    filename = paste0('spListOriginal_recs_', Sys.Date(),'.csv'), 
    content = function(file) {
      write.csv(xls_recs, file, row.names = FALSE)
      #xlsx::write.xlsx
    })
  
} # End server

# setwd('C:/GoogleDrive/IAvH/V2/Others/polyg_draw/app8_upload/')
# http://ec2-3-15-208-233.us-east-2.compute.amazonaws.com:8080/polsizekm2?pol=POLYGON((-74.133545 4.144818,-73.817139 3.741479,-74.572998 3.390597,-74.133545 4.144818))
shinyApp(ui, server)


