#### Library

library(forestChange)
library(gdalUtils)
library(gdalUtilities)
library(rgeos)
library(rgdal)
library(raster)
library(rasterDT)
library(plumber)
library(foreign)

forestyy <- c(1990, 2000, 2005, 2010, 2012, 2013, 2014, 2015, 2016)
ideamyyebm <- c(0, 10, 15, 20, 22, 23, 24, 25, 26)
metrics <- c('biome', 'ecouicn', 'faccomp', 'bioticreg', 'tropdryforest', 'param',
             'hum', 'sma', 'protectareas', 'colectareas', 'clc', 'forest', 'rli', 'species',
             'threatspecies', 'evalspecies', 'knownspecies', 'surface', 
             'test')
prj <- "+proj=tmerc +lat_0=4.596200417 +lon_0=-74.07750791700001 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +units=m +no_defs"
thresholdKm2 <- 75000

(dataPath <- ifelse(any(grep('HP-Z400', Sys.getenv('LOGONSERVER'))), 
                   'H:/iavh_biotablero/data',
                   '/data'))
setwd(dataPath)

#### Params

metric = 'ecouicn' # metric = 'forest' 
lay = NULL
polID = NULL

# http://arthur-e.github.io/Wicket/sandbox-gmaps3.html
pol = 'POLYGON((-67.44781%206.02875,-68.83759%205.73761,-70.15045%205.2597,-70.88104%204.8587,-70.66132%203.24371,-69.28253%203.56232,-67.66754%204.04022,-67.7829%205.42999,-67.44781%206.02875))' # Cumaribo
pol = "POLYGON((-74.816895%201.515936,-74.663086%200.494379,-73.432617%201.274309,-74.816895%201.515936))"
pol = 'POLYGON((-75.06%202.2,-74.14%202.2,-74.14%203.21,-75.06%203.21,-75.06%202.2))' # Mariquita, 
pol = 'POLYGON((-73.47660967773925 5.415229773262239,-73.87211749023925 5.590200893361613,-73.88859698242675 5.251148301573963,-73.47660967773925 5.415229773262239))' # tunja triangle
# Choco
pol = 'POLYGON((-77.04588712341355 3.7229562268432224,-77.46886075622605 3.108817872007526,-77.78330417730078 2.3542116433414217,-76.76706882573828 2.6121477016767036,-76.55832859136328 3.385617244643956,-76.42099948980078 3.988611756164354,-76.86045261480078 4.355677789862958,-77.04588712341355 3.7229562268432224))'
ebvstat = 'lsm_l_tafc'
sour = 'ideam'
cellSize = 1
spFormat = 'area'
ebvyear = '2000:2005'
ebvporcrange = '80:100'
clclevel = 3


# shp <- wkt <- readOGR('C:/GoogleDrive/IAvH/data/testPolygon.shp')
## Create a gdal-readable object from WKT
shp <- wkt <- tryCatch(SpatialPolygonsDataFrame(readWKT(gsub('%20', ' ', pol)), data.frame(ID = 1)),error = function(e) NULL)
if (is.null(wkt)){
  return("ERROR: Not a valid WKT object")
  stop()
}; wkt@proj4string@projargs <- '+proj=longlat +ellps=GRS80 +no_defs'
## Project polygon in order to clip and get areas from projected original layers
wkt_pcs <- spTransform(wkt, CRSobj = CRS(prj))
wkt_pcs$km2 <- sapply(slot(wkt_pcs, "polygons"), function(x) sum(sapply(slot(x, "Polygons"), slot, "area")))/1000000
#plot(wkt_pcs); writeOGR(wkt_pcs, 'H:/iavh_biotablero', 'tunja_pcs', driver = 'ESRI Shapefile')
# writeOGR(obj = wkt, dsn = '.', layer = 'mariquita_shp',  driver = 'ESRI Shapefile')
# writeOGR(obj = wkt, dsn = './mariquita_GeoJSON.GeoJSON', layer = 'mariquita_GeoJSON',  driver = 'GeoJSON')
# writeOGR(obj = wkt, dsn = './mariquita_SQLite.SQLite', layer = 'mariquita_SQLite',  driver = 'SQLite')
# writeOGR(obj = wkt, dsn = './mariquita_GPKG.gpkg', layer = 'mariquita_GPKG',  driver = 'GPKG')
# 
# xxES <- readOGR('.', 'mariquita_shp')
xxES2 <- readOGR('.', 'mariquita_shp - Copy')

# xxgj <- readOGR('mariquita_GeoJSON.GeoJSON')
# xxES <- readOGR('mariquita_SQLite.SQLite')
# xxGP <- readOGR('mariquita_GPKG.gpkg')
### 


metric = 'clc'
clclevel = 1
rasterLayer = TRUE

#metric = 'surface'; cellSize = 100
metric = 'colectareas'
metric = 'protectareas'
metric = 'hum'
metric = 'ecouicn'
metric = 'protectareas'
metric = 'sma'
metric = 'tropdryforest'
metric = 'forest';  ebvstat = 'area'
metric = 'species'; sour = 'uicn'; spFormat = 'list'
cellSize = 50


pol <- paste0("POLYGON%20((-74.0062070200000051%204.8232705300000003,%20-74.0045872999999972%204.8165161200000002,%20-74.0441623100000044%204.8323849900000004,%20-74.0699225900000044%204.8299123300000000,%20-74.0799310900000023%204.8344775300000000,%20-74.0775136500000002%204.8423752100000002,%20-74.0940726299999994%204.8491874299999997,%20-74.0828914800000007%204.8662841300000004,%20-74.0774240700000064%204.9116082399999996,%20-74.0587556499999948%204.9009278099999998,%20-74.0531294500000001%204.9048767800000004,%20-74.0465061099999957%204.9006249500000001,%20-74.0492464499999983%204.8926974400000001,%20-74.0241383999999982%204.8830701200000002,%20-74.0050998199999981%204.9139381400000000,%20-73.9829294700000020%204.9040732699999996,%20-74.0062070200000051%204.8232705300000003))")

(bm <- biotablero(server = 'web', webURL = aws, port = aws_port,
                  endpoint = 'biotablero', metric = 'protectareas',
                  pol = wktg, printURL = F, rasterLayer = TRUE) )


(bmv <- biotablero(server = 'web', webURL = aws, port = aws_port,
                  endpoint = 'biotablero', metric = 'protectareas',
                  pol = wktg, printURL = F, rasterLayer = FALSE) )



plot(wkt)
plot(surf, add = F)
plot(wkt, add = TRUE)
cellSize = 100
suppressWarnings(surf <- raster(paste0(dataPath, '/surface/RecCoun-', cellSize, '.tif')))
(suppressWarnings(msk <- raster::crop(surf, wkt)))
plot(mskC); plot(wkt, add = TRUE)
if(ncell(msk) > 4){
  suppressWarnings(msk <- raster::mask(msk, wkt, ))
  plot(mskM); plot(wkt, add = TRUE)
}

suppressWarnings(msk <- raster::mask(raster::crop(surf, wkt), wkt))
result <- list(percentage = mean(Which(msk > 0)[], na.rm = TRUE) * 100)