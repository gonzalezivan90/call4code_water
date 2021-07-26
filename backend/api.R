### Load required libraries
suppressWarnings(suppressPackageStartupMessages(library(forestChange)))
suppressWarnings(suppressPackageStartupMessages(library(gdalUtils)))
suppressWarnings(suppressPackageStartupMessages(library(rgeos)))
suppressWarnings(suppressPackageStartupMessages(library(rgdal)))
suppressWarnings(suppressPackageStartupMessages(library(raster)))
suppressWarnings(suppressPackageStartupMessages(library(plumber)))
suppressWarnings(suppressPackageStartupMessages(library(foreign)))
suppressWarnings(suppressPackageStartupMessages(library(mongolite)))
suppressWarnings(suppressPackageStartupMessages(library(gdalUtilities)))
suppressWarnings(suppressPackageStartupMessages(library(rasterDT)))
# suppressWarnings()


raster_count <- structure(function # Count the pixels in a given raster
                          ### This function generate a frequency table for a given raster dataset
                          (
                            layer = '',         ##<<\code{character}. Raster object or raster path
                            n256 = FALSE       ##<<\code{boolean} Determines if the raster contains less than 256 unique values, with
                          ) {
                            # layer = rast
                            if (!class(layer) %in% c('RasterLayer', 'character')){
                              stop('Class not RasterLayer or character')
                            }
                            
                            if (class(layer) %in% c('character')){
                              if (!file.exists(layer)){
                                stop('File not found')
                              }
                            }
                            
                            if (n256){
                              
                              suppressWarnings(gdalLog <- capture.output(gdalUtilities::gdalinfo(datasetname = layer, hist = TRUE)))
                              
                              if( any(grep('buckets ', gdalLog)) ) {
                                
                                (bucxml <- as.numeric(sub('buckets.+', '', grep('buckets ', gdalLog, value = TRUE))))
                                (minxml <- as.numeric(gsub('.+from | to.+', '', grep('buckets ', gdalLog, value = TRUE)) ))
                                (maxxml <- as.numeric(gsub('.+to |:', '', grep('buckets ', gdalLog, value = TRUE))))
                                (histxml <- as.numeric(strsplit(split = '[[:space:]]', gsub("^ |^  ", "", gdalLog[grep('buckets', gdalLog)+1]))[[1]]))
                                
                                labs <- seq(from = minxml, to = maxxml, length.out = bucxml)
                                
                                df2 <- data.frame(labs, nwlab = c(ceiling(labs[1]),
                                                                  round(labs[2:(bucxml-1)]),
                                                                  floor(labs[bucxml])),
                                                  val = histxml)
                                hist2 <- aggregate(df2$val, by = list(df2$nwlab), sum)
                                result <- data.frame(id = hist2$Group.1, count = hist2$x, stringsAsFactors = FALSE)
                              } else { 
                                ## In the case gdalinfo doesn't detects the pixels
                                if (class(layer) %in% c('character')){
                                  layer <- tryCatch(raster(layer), error = function (e) stop( "Can't open raster layer"))
                                }
                                
                                freqTable <-  rasterDT::freqDT(layer)
                                result <- data.frame(id = freqTable$ID, count = freqTable$freq, stringsAsFactors = FALSE )
                              }
                              
                              
                            } else {
                              if (class(layer) %in% c('character')){
                                layer <- tryCatch(raster(layer), error = function (e) stop( "Can't open raster layer"))
                              }
                              
                              freqTable <-  rasterDT::freqDT(layer)
                              result <- data.frame(id = freqTable$ID, count = freqTable$freq, stringsAsFactors = FALSE )
                            }
                            
                            return(result)
                            ### \code{data.frame}.
                          } , ex = function() {
                            ## \donttest{
                            ## raster_count(raster(volcano), n256 = FALSE)
                            ## }
                          })


## Metrics allowed inside the API
metrics <- c('biome', 'ecouicn', 'faccomp', 'bioticreg', 'tropdryforest', 'param',
             'hum', 'sma', 'protectareas', 'colectareas', 'clc', 'forest', 'rli', 
             'species', 'threatspecies', 'evalspecies', 'knownspecies', 'surface', 
             'hum_rast', 
             'test')

## Default geographic coordinate reference system
prj <- "+proj=tmerc +lat_0=4.596200417 +lon_0=-74.07750791700001 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +units=m +no_defs"

## Maximum km2 allowed for queries
thresholdKm2 <- 5000

## MongoDB fields
mongoFields <- c("kingdom", "phylum", "class", "order", "family", "genus", "species", 
                 "decimalLatitude", "decimalLongitude", "sortID",
                 "uicn_glob", "cites", "invasive", "endemic")



#* @apiTitle BiotableroAPI

#* Testing the API
#* @param None None parameter
#* @get /test
function(){
  list(msg = "Hello. Glad to see you here! Endpoint 'test'")
}

#* Print argument 
#* @param x any argument
#* @get /class
function(x){
  print(x)
  print(class(x))
  list(x, class(x))
}


#* Provide the current working directory
#* @param None None parameter
#* @get /gwd
function(){
  list(gwd = getwd())
}

#* List files and folders in a given path
#* @param path The path requierd
#* @get /lsFiles
function(path, fpat = '*'){
  list(files = c(list.files(path = path, pattern = fpat, recursive = FALSE)),
       dirs = c(list.dirs(path = path, recursive = FALSE)))
}

#* Evaluate the polygon area in Km2
#* @param pol The WKT geometry
#* @get /polsizekm2
function(pol = NA){
  if (!is.na(pol)){
    
    ## Create a gdal-readable object from WKT
    wkt <- tryCatch(SpatialPolygonsDataFrame(readWKT(gsub('%20', ' ', pol)), data.frame(ID = 1)),
                    error = function(e) NULL)
    
    if (is.null(wkt)){
      return("ERROR: Not a valid WKT object")
      stop()
    }
    
    lr <- suppressWarnings(list(polsizekm2 = area(wkt)/1000000))
    return(lr)
  }
}


#* List the available stpatial templates
#* @param templatesPath The path containign the available layers
#* @get /listTemplates
function(templatesPath = '/data/templates'){
  gsub('.dbf', '', list.files(path = templatesPath, pattern = '.dbf'))
}


#* Get the template table & IDs
#* @param template The table requiered
#* @param dataTemplates The path containign the available layers
#* @get /getTemplate
function(template = NA, templatesPath = '/data/templates'){
  templateTable <- tryCatch(foreign::read.dbf(paste0(templatesPath, '/', template, '.dbf')),
                            error = function(e) NULL)
  if (is.null(templateTable)){
    return("ERROR: Not a valid template. Use the 'listTemplates' function to know available options")
    stop()
  }
  
  return(templateTable)
}


#* Calculate biodiversity metrics
#* @param metric The metric to be consulted. Incluided options are: 'biome', 'ecouicn', 'faccomp', 'bioticreg', 'tropdryforest', 'param', 'hum', 'sma', 'protectareas', 'colectareas', 'clc', 'forest', 'rli', 'species', 'threatspecies', 'evalspecies', 'knownspecies', 'surface'
#* @param lay The layer or spatial templates to be consulted. Optional in all metrics
#* @param polID The ID of polygon or element from the given layer or spatial extent. Optional in all metrics
#* @param pol A polygon in format WKT. Requeried in almost all metrics.
#* @param ebvstat The Essential Biodiversity Variables so be calculated. 'area' or 'all' available 
#* @param sour The data source to calculate the metric. If metric is 'forest', 'hansen' and 'ideam' are allowed. If metric is 'species', the values 'biomod', 'uicn' and 'records' are allowed.
#* @param cellSize The cell size for 'surface' metric. 1, 2, 5, 10, 20, 50, 100, 200, 500 are allowed. Required in 'surface' metric.
#* @param ebvyear The years to estimate the forest metrics. Requierd in 'forest' metric.
#* @param ebvporcrange The threshold range values used to consider the forest cover extent in each pixel as a forest. Requierd in 'forest' metric.
#* @param clclevel The Corine Land Cover level. Required in 'clc' metric.
#* @param dataPath The data folder path.
#* @param spFormat The possible formats for 'species' metric. 
#* @param spRecordsFields Species records fields to extract from MongoDB.
#* @param spRecordsTabulate Tablulating field for species records.
#* @param rasterLayer Try to calculate the metric based on a raster layer
#* @param dataPath The data folder path.
#* @serializer unboxedJSON
#* @get /biotablero




function(metric = NA, lay = NA, polID = NA, pol = NA, 
         ebvstat = NA, sour = NA, ebvyear = NA, ebvporcrange = NA,
         spFormat = 'list', spRecordsFields = NA, spRecordsTabulate = NA, 
         clclevel = NA, cellSize = NA, rasterLayer = FALSE, dataPath = '/data') {
  
  # metric = NULL; lay = NULL; polID = NULL; pol = NULL;
  # ebvstat = NULL; sour = NULL; ebvyear = NULL; ebvporcrange = NULL;
  # spFormat = NULL; spRecordsFields = NULL; spRecordsTabulate = NULL;
  # clclevel = NULL; cellSize = NULL; rasterLayer = FALSE; dataPath = '/data'
  
  dots <- tryCatch(c(...), error = function(e) NULL)
  tStart <- Sys.time()
  print(paste0('====================================================='))
  print(paste0('Metric: ', metric, ' - Date: ', format(as.POSIXct(Sys.time(), tz="CET"), tz="America/Bogota",usetz=TRUE)  ))
  
  ## Testing ------
  if(metric %in% 'test'){
    result <- "Welcome to Biotablero API. Test function from 'biotablero' endpoint"
    return(result)
    stop()
  }
  
  ## Validate metric ------
  # 'metric' into the available metrics?
  if(!all(metric %in% metrics)){
    return(paste0("ERROR: ", metric, " not a valid metric"))
    stop()
  }
  
  
  ## Get into the static metrics ------
  if (metric %in% 'rli') {
    load(file = paste0(dataPath, '/rli/rli.RData'))
  } 
  
  
  ## Validate polygon if given. ------
  # Identify if a polygon is into the arguments. If 'pol' is not NULL, should
  # be a wkt polygon which will cut the layers
  
  if (!is.na(pol)  & (is.na(lay) & is.na(polID) )){
    
    ## Create a gdal-readable object from WKT
    wkt <- suppressWarnings(tryCatch(SpatialPolygonsDataFrame(readWKT(gsub('%20', ' ', pol)), data.frame(ID = 1)),
                    error = function(e) NULL))
    
    if (is.null(wkt)){
      return("ERROR: Not a valid WKT object")
      stop()
    }
    
    wkt@proj4string@projargs <- '+proj=longlat +ellps=GRS80 +no_defs'
    
    ## Project polygon in order to clip and get areas from projected original layers
    wkt_pcs <- suppressWarnings(spTransform(wkt, CRSobj = CRS(prj)))
    wkt_pcs$km2 <- suppressWarnings(sapply(slot(wkt_pcs, "polygons"), function(x) sum(sapply(slot(x, "Polygons"), slot, "area")))/1000000)
    
    ## Establish a treshold for the polygon. Colombia surface is 1,141,748 km2
    if (sum(wkt_pcs$km2) > thresholdKm2){
      return( paste0("ERROR: Polygon bigger than the threshold (", thresholdKm2, " km2)") )
      stop()
    }
    
    ## Get into the crop-like functions for single layers ------------
    
    if (metric %in% c('biome', 'bioticreg' ,'colectareas', 'ecouicn', 'faccomp', 
                      'hum', 'param', 'sma', 'protectareas', 'tropdryforest')){
      
      print(paste0('Calculating results from -', metric, '- metric'))
      
      
      ## Default columns into each shapefile
      layerFields <- c('id', 'name')
      
      ## Use the GDAL histogram, limited to 256 values [0, 255]
      n256 <-  TRUE
      
      ## Customize fields for some layers.
      if(metric == 'faccomp'){
        layerFields <- c('id', 'name', 'factor')
        n256 <-  TRUE
        
      } else if(metric == 'ecouicn'){
        layerFields <- c('id', 'name', 'Bioma', 'Fisionomia', 'Paisaje','Biota')
        
      } else if(metric == 'protectareas'){
        layerFields <- c('id', 'protected', 'id_protect', 'category_p', 'date_resol')
        n256 <-  FALSE
        
      } else if(metric == 'sma'){
        layerFields <- c('id', 'bosque', 'rbiosfera', 'ramsar', 'aicas', 'comunidade', 'pdet', 'resguardos', 'paramos', 'portafolio', 'zonas')
        n256 <-  FALSE
        
      } else if(metric %in% 'colectareas'){
        layerFields <- c('id', 'name', 'terType')
        n256 <-  FALSE
        
      } else if(metric %in% 'biome'){
        n256 <-  FALSE # cuz 400 IDS
      }
      
      
      ## Calc areas based on raster
      if (rasterLayer == TRUE){
        
        layerFieldsRast <- gsub('^id$', 'idrast', layerFields)
        timeMark <- gsub('[[:punct:]]| ', '', format(as.POSIXct(Sys.time(), tz="CET"), tz="America/Bogota",usetz=TRUE))
        tempRastDir <- paste0(dataPath, '/tempR/', timeMark, 
                              '_r1', round(runif(1, 0, 100)), 
                              '_r2', round(runif(1, 0, 100)), 
                              '_r3', basename(tempfile()))
        
        dir.create(tempRastDir, recursive = TRUE)
        print(paste0('Temp dir: ', tempRastDir))
        rasterOptions(tmpdir = tempRastDir, format = 'GTiff')
        rastTemp <- paste0(tempRastDir, '/', basename(tempfile()), 'rast.tif')
        
        print(paste0('Calculating results from -', metric, '- metric'))
        suppressWarnings(rast <- gdalUtilities::gdalwarp(srcfile = paste0(dataPath, '/rasterLayers/', metric, '.tif'),
                                        dstfile = rastTemp,
                                        csql = paste0("select ST_GeomFromText('", writeWKT(wkt_pcs, byid = F), "')"),
                                        cutline = paste0(dataPath,'/singleLayers/tempSQlite.sqlite'),
                                        dstnodata = 999,
                                        crop_to_cutline = TRUE,
                                        overwrite = TRUE))
        
        rastC <- raster_count(rast, n256 = n256)
        rastC <- rastC[which(rastC$count > 0 & !is.na(rastC$id)), ]
          
        if (nrow(rastC) > 0) { # non empty result
          
          suppressWarnings(rastInfo <- capture.output(gdalinfo(datasetname = rast)))
          pixAreaKm2 <- prod(abs(as.numeric(gsub(" |[[:alpha:]]|([.-])|[[:punct:]]", "\\1",
                                                 strsplit(grep('Pixel Size ', rastInfo, value = TRUE)
                                                          , ',')[[1]]))))/1000000
          
          dbf <- read.dbf(paste0(dataPath, '/rasterLayers/', metric, '.tif.vat.dbf'), as.is = TRUE)
          
          #all(na.omit(rastC$id) %in% dbf$idrast)
          #dim(dbf)
          rastCM <- merge(rastC, unique(dbf[, layerFieldsRast]), by.x = 'id', by.y = 'idrast',
                          all.y = FALSE, stringsAsFactors = TRUE)
          
          rastCM$km2 <- rastCM$count * pixAreaKm2
          
          result <- data.frame(rastCM[, c(layerFields, 'km2')])
          
          file.remove(rastTemp)
          unlink(tempRastDir, recursive = TRUE, force = TRUE)
          
          
          ######### Raster post processing start
          
          # if (metric == 'faccomp'){
          #   
          #   result <- with(result, aggregate(km2, list(name, factor), sum))
          #   colnames(result) <- c('name', 'factor', 'km2')
          #   result$name <- iconv(result$name, from ='utf8')
          #   result$factor <- as.numeric(result$factor)
          #   
          # # } else
          #   if(metric == 'ecouicn'){
          #   
          #   result$Bioma <- iconv(result$Bioma, from ='utf8')
          #   result$Paisaje <- iconv(result$Paisaje, from ='utf8')  
          # } 
          # else if(metric == 'bioticreg'){
          #   
          #   result$bioticreg <- iconv(result$name, from ='utf8')
          #   
          # } else
          if(metric == 'protectareas'){
            
            result <- with(result, aggregate(km2, list(id_protect, protected, category_p, date_resol), sum))
            colnames(result) <- c( 'id_protect', 'protected', 'category_p', 'date_resol', 'km2')
            
          } else if(metric == 'sma'){
            
            smaFields <- subset(layerFields, layerFields != 'id')
            
            categs <- apply(result[, smaFields], 1, function(x){
              paste(smaFields[which(!is.na(x))], collapse = ', ')
            })
            
            result1 <- data.frame(xtabs(result$km2 ~ categs))
            colnames(result1) <- c('categs', 'km2')
            
            entities <- apply(result[, c(smaFields, 'km2')], 1, function(x){
              km2 <- as.numeric(x[length(x)])
              y <- x[-length(x)]
              cbind(cbind(smaFields[which(!is.na(y))], y[which(!is.na(y))]), km2)
            })
            
            if (class(entities) == 'list'){
              result2 <- data.frame(do.call(rbind, entities), stringsAsFactors = FALSE)
            } else if (class(entities) == 'matrix'){
              result2 <- data.frame(entities, stringsAsFactors = FALSE)
              if (nrow(entities) == 3){
                result2 <- data.frame(t(entities), stringsAsFactors = FALSE)
              } 
            }
            
            colnames(result2) <- c('category', 'management', 'km2')
            result2$km2 <- as.numeric(result2$km2)
            result2 <- with(result2, aggregate(km2, list(category, management), function(x) sum(x, na.rm = TRUE)))
            colnames(result2) <- c('category', 'management', 'km2')
            
            result <- list(result1 = result1, result2 = result2)
          }
          
        } else {
          result <- 0
        }
        ######### Raster post processing end
        
      } else { ## Calc areas based on vector
        
        
        ## Calculate areas
        
        # This approach calculates areas for all polygons
        # if(metric %in% c('hum', 'ecouicn')){
        #   areakm2 <- ogrinfo(datasource_name = paste0(dataPath, '/singleLayers/', metric,'.shp'), dialect = 'SQLite', 
        #                      sql = paste0("SELECT ", paste0(layerFields, collapse = ', '),", ST_Area(intersection)/1000000 as km2 FROM (SELECT ", paste0(layerFields, collapse = ', '),
        #                                   ", ST_Intersection(GEOMETRY, ST_GeomFromText('", 
        #                                   writeWKT(wkt_pcs, byid = F), "')) AS intersection FROM ", metric, ")") )
        # } else {
        
        # This approach first filter intersecting polygons, and then calculates areas
        areakm2 <- ogrinfo(datasource_name = paste0(dataPath, '/singleLayers/', metric,'.shp'), dialect = 'SQLite', 
                           sql = paste0("SELECT ", paste0(layerFields, collapse = ', '),", ST_Area(intersection)/1000000 as km2 FROM (SELECT ", 
                                        paste0(layerFields, collapse = ', '), ", ST_Intersection(GEOMETRY, ST_GeomFromText('", 
                                        writeWKT(wkt_pcs, byid = F), "')) AS intersection FROM ", metric, " WHERE ST_Intersects(GEOMETRY, ST_GeomFromText('", 
                                        writeWKT(wkt_pcs, byid = F),"')))") )
        # }
        
        ## Create a string buffer 
        areakm2 <- c(areakm2, rep('OGR', 10))
        
        ## Consider GEOS error in topology. Catch GDAL warnings inside the query
        chunks <- sapply(grep('OGRFeature', areakm2, value = FALSE), function(x){
          y <- areakm2[x:(x + length(layerFields)+10)]
          k <- y[ 1: grep('OGR', y)[2]-1 ]
          w <- gsub('^ |^  ', '', paste0( gsub('GEOS error.+|', '', k) , collapse = ''))
          z <- gsub('.+\\= ', '', strsplit(w, paste0(c(layerFields, 'km2'), collapse = ' |'))[[1]])
          gsub('^ |^  | $|  $', '', z[z != ''][-1])
        })
        
        
        ## Process table only if some results are intersected
        if (length(chunks) != 0){
          result <- data.frame(matrix(chunks, ncol = length(layerFields)+1, byrow = TRUE), stringsAsFactors = FALSE)
          colnames(result) <- c(layerFields, 'km2')
          colnames(result) <- gsub('name', metric, colnames(result))
          result$km2 <- as.numeric(as.character(result$km2))
          result$id <- as.numeric(as.character(result$id))
          result <- result[!is.na(result$km2), ]
          result[result == '(null)'] <- NA
          
          
          ## Aggregate areas or post processing
          
          if(metric == 'hum'){
            
            result <- with(result, aggregate(km2, list(id, hum), function(x) sum(x, na.rm = TRUE)))
            colnames(result) <- c(layerFields, 'km2')
            
          } else if (metric == 'faccomp'){
            
            result <- with(result, aggregate(km2, list(faccomp, factor), sum))
            colnames(result) <- c('faccomp', 'factor', 'km2')
            result$faccomp <- iconv(result$faccomp, from ='utf8')
            result$factor <- as.numeric(result$factor)
            
          } else if(metric == 'ecouicn'){
            
            result$Bioma <- iconv(result$Bioma, from ='utf8')
            result$Paisaje <- iconv(result$Paisaje, from ='utf8')
            
          } else if(metric == 'bioticreg'){
            
            result$bioticreg <- iconv(result$bioticreg, from ='utf8')
            
          } else if(metric == 'tropdryforest'){
            
            result <- with(result, aggregate(km2, list(id, tropdryforest), function(x) sum(x, na.rm = TRUE)))
            colnames(result) <- c(layerFields, 'km2')
            
          } else if(metric == 'sma'){
            
            smaFields <- subset(layerFields, layerFields != 'id')
            
            categs <- apply(result[, smaFields], 1, function(x){
              paste(smaFields[which(!is.na(x))], collapse = ', ')
            })
            
            result1 <- data.frame(xtabs(result$km2 ~ categs))
            colnames(result1) <- c('categs', 'km2')
            
            entities <- apply(result[, c(smaFields, 'km2')], 1, function(x){
              km2 <- as.numeric(x[length(x)])
              y <- x[-length(x)]
              cbind(cbind(smaFields[which(!is.na(y))], y[which(!is.na(y))]), km2)
            })
            
            if (class(entities) == 'list'){
              result2 <- data.frame(do.call(rbind, entities), stringsAsFactors = FALSE)
            } else if (class(entities) == 'matrix'){
              result2 <- data.frame(entities, stringsAsFactors = FALSE)
              if (nrow(entities) == 3){
                result2 <- data.frame(t(entities), stringsAsFactors = FALSE)
              } 
            }
            
            colnames(result2) <- c('category', 'management', 'km2')
            result2$km2 <- as.numeric(result2$km2)
            result2 <- with(result2, aggregate(km2, list(category, management), function(x) sum(x, na.rm = TRUE)))
            colnames(result2) <- c('category', 'management', 'km2')
            
            result <- list(result1 = result1, result2 = result2)
            
          }
          
        } else {
          result <- 0
        }
        
      }
    }
    
    
    
    ## Get into the crop-like functions for temporal layers: CLC ---------
    if (metric %in% c('clc')){
      if (is.null(clclevel) | is.na(clclevel)){
        return(paste0('ERROR: clclevel required (1, 2 or 3)'))
        stop()
      }
      
      if (! clclevel %in% c('1', '2', '3' )){
        return(paste0('ERROR: clclevel "', clclevel, '" not 1, 2 or 3'))
        stop()
      }
      
      ##########
      
      print(paste0('Calculating results from -', metric, '- metric'))
      ## CLC areas based on raster
      if (rasterLayer == TRUE){      
        
        ## Default columns into each shapefile
        layerFields <- c('idrast', 'clc')
        
        ## Use the GDAL histogram, limited to 256 values [0, 255]
        n256 <-  TRUE
        
        layerFieldsRast <- gsub('^id$', 'idrast', layerFields)
        
        timeMark <- gsub('[[:punct:]]| ', '', format(as.POSIXct(Sys.time(), tz="CET"), tz="America/Bogota",usetz=TRUE))
        tempRastDir <- paste0(dataPath, '/tempR/', timeMark, 
                              '_r1', round(runif(1, 0, 100)), 
                              '_r2', round(runif(1, 0, 100)), 
                              '_r3', basename(tempfile()))
        dir.create(tempRastDir, recursive = TRUE)
        print(paste0('Temp dir: ', tempRastDir))
        rasterOptions(tmpdir = tempRastDir, format = 'GTiff')
        rastTemp02 <- paste0(tempRastDir, '/', basename(tempfile()), 'rast.tif')
        rastTemp09 <- paste0(tempRastDir, '/', basename(tempfile()), 'rast.tif')
        rastTemp12 <- paste0(tempRastDir, '/', basename(tempfile()), 'rast.tif')
        
        
        print(paste0('Calculating results from -', metric, '- raster metric'))
        
        ## Cut Colombia Hansen layers with WKT geometry
        suppressWarnings(rast02 <- gdalUtilities::gdalwarp(srcfile = paste0(dataPath, '/rasterLayers/N', clclevel, '_2000_2002.tif'),
                                          dstfile = rastTemp02,
                                          csql = paste0("select ST_GeomFromText('", writeWKT(wkt_pcs, byid = F), "')"),
                                          cutline = paste0(dataPath,'/singleLayers/tempSQlite.sqlite'),
                                          dstnodata = 999, crop_to_cutline = TRUE, overwrite = TRUE))
        suppressWarnings(rast09 <- gdalUtilities::gdalwarp(srcfile = paste0(dataPath, '/rasterLayers/N', clclevel, '_2005_2009.tif'),
                                          dstfile = rastTemp09,
                                          csql = paste0("select ST_GeomFromText('", writeWKT(wkt_pcs, byid = F), "')"),
                                          cutline = paste0(dataPath,'/singleLayers/tempSQlite.sqlite'),
                                          dstnodata = 999, crop_to_cutline = TRUE, overwrite = TRUE))
        suppressWarnings(rast12 <- gdalUtilities::gdalwarp(srcfile = paste0(dataPath, '/rasterLayers/N', clclevel, '_2010_2012.tif'),
                                          dstfile = rastTemp12,
                                          csql = paste0("select ST_GeomFromText('", writeWKT(wkt_pcs, byid = F), "')"),
                                          cutline = paste0(dataPath,'/singleLayers/tempSQlite.sqlite'),
                                          dstnodata = 999, crop_to_cutline = TRUE, overwrite = TRUE))
        
        print(paste0('  Crop done!'))
        
        rastC02 <- raster_count(rast02, n256 = n256)
        rastC02 <- rastC02[which(rastC02$count > 0), ]
        rastC09 <- raster_count(rast09, n256 = n256)
        rastC09 <- rastC09[which(rastC09$count > 0), ]
        rastC12 <- raster_count(rast12, n256 = n256)
        rastC12 <- rastC12[which(rastC12$count > 0), ]
        
        print(paste0('  Pixel count done!'))
        
        
        suppressWarnings(rastInfo <- capture.output(gdalinfo(datasetname = rast02)))
        pixAreaKm2 <- prod(abs(as.numeric(gsub(" |[[:alpha:]]|([.-])|[[:punct:]]", "\\1",
                                               strsplit(grep('Pixel Size ', rastInfo, value = TRUE)
                                                        , ',')[[1]]))))/1000000
        dbf02 <- read.dbf(paste0(dataPath, '/rasterLayers/N', clclevel, '_2000_2002.tif.vat.dbf'), as.is = TRUE)
        dbf09 <- read.dbf(paste0(dataPath, '/rasterLayers/N', clclevel, '_2005_2009.tif.vat.dbf'), as.is = TRUE)
        dbf12 <- read.dbf(paste0(dataPath, '/rasterLayers/N', clclevel, '_2010_2012.tif.vat.dbf'), as.is = TRUE)
        
        
        rastCM02 <- merge(rastC02, unique(dbf02[, layerFieldsRast]), by.x = 'id', by.y = 'idrast',
                          all.y = FALSE, stringsAsFactors = TRUE)
        rastCM02$km2 <- rastCM02$count * pixAreaKm2
        rastCM09 <- merge(rastC09, unique(dbf09[, layerFieldsRast]), by.x = 'id', by.y = 'idrast',
                          all.y = FALSE, stringsAsFactors = TRUE)
        rastCM09$km2 <- rastCM09$count * pixAreaKm2
        rastCM12 <- merge(rastC12, unique(dbf12[, layerFieldsRast]), by.x = 'id', by.y = 'idrast',
                          all.y = FALSE, stringsAsFactors = TRUE)
        rastCM12$km2 <- rastCM12$count * pixAreaKm2
        
        print(paste0('  Data merge done!'))
        
        
        allCLC <- rbind(cbind(yy = 2002, na.omit(rastCM02)), cbind(yy = 2009, na.omit(rastCM09)), cbind(yy = 2012, na.omit(rastCM12)))
        allCLC$cod <- allCLC$clc
        
        file.remove(rastTemp02, rastTemp09, rastTemp12)
        unlink(tempRastDir, recursive = TRUE, force = TRUE)
        
        
      } else {
        
        print(paste0('Calculating results from -', metric, '- vector metric '))
        
        ## Calculate areas
        clc2002 <- ogrinfo(datasource_name = paste0(dataPath, '/clc/N', clclevel, '_2000_2002.shp'), dialect = 'SQLite', 
                           sql = paste0("SELECT clc, ST_Area(intersection)/1000000 as km2 FROM (SELECT clc, ST_Intersection(GEOMETRY, ST_GeomFromText('",
                                        writeWKT(wkt_pcs, byid = F), "')) AS intersection FROM N", clclevel, "_2000_2002 WHERE ST_Intersects(GEOMETRY, ST_GeomFromText('", 
                                        writeWKT(wkt_pcs, byid = F),"')))")) 
        
        clc2009 <- ogrinfo(datasource_name = paste0(dataPath, '/clc/N', clclevel, '_2005_2009.shp'), dialect = 'SQLite', 
                           sql = paste0("SELECT clc, ST_Area(intersection)/1000000 as km2 FROM (SELECT clc, ST_Intersection(GEOMETRY, ST_GeomFromText('", 
                                        writeWKT(wkt_pcs, byid = F), "')) AS intersection FROM N", clclevel,
                                        "_2005_2009 WHERE ST_Intersects(GEOMETRY, ST_GeomFromText('", 
                                        writeWKT(wkt_pcs, byid = F),"')))"))
        
        clc2012 <- ogrinfo(datasource_name = paste0(dataPath, '/clc/N', clclevel, '_2010_2012.shp'), dialect = 'SQLite', 
                           sql = paste0("SELECT clc, ST_Area(intersection)/1000000 as km2 FROM (SELECT clc, ST_Intersection(GEOMETRY, ST_GeomFromText('", 
                                        writeWKT(wkt_pcs, byid = F), "')) AS intersection FROM N", clclevel, "_2010_2012 WHERE ST_Intersects(GEOMETRY, ST_GeomFromText('", 
                                        writeWKT(wkt_pcs, byid = F),"')))"))
        
        print(paste0('  Crop done!'))
        
        clc2002 <- c(clc2002, rep('OGR', 3))
        clc2009 <- c(clc2009, rep('OGR', 3))
        clc2012 <- c(clc2012, rep('OGR', 3))
        
        ## Consider GEOS error in topology. Catch GDAL warnings inside the query
        vals02 <- sapply(grep('OGRFeature', clc2002, value = FALSE), function(x){ # x = 8403; x = 11; x = 2099   
          y <- c(clc2002[(x+1):(x + 3)], "")
          w <- gsub('^ |^  ', '', paste0( gsub('GEOS error.+|', '',  y[1:(which(y == '')[1]-1)] ) , collapse = ''))
          z <- gsub('.+\\= ', '', strsplit(w, 'x2002 |km2')[[1]])
          c(gsub('^ | $', '', z[z != '']), NA)[1:2]
        })
        
        vals09 <- sapply(grep('OGRFeature', clc2009, value = FALSE), function(x){
          y <- c(clc2009[(x+1):(x + 3)], "")
          w <- gsub('^ |^  ', '', paste0( gsub('GEOS error.+|', '',  y[1:(which(y == '')[1]-1)] ) , collapse = ''))
          z <- gsub('.+\\= ', '', strsplit(w, 'x2009 |km2')[[1]])
          c(gsub('^ | $', '', z[z != '']), NA)[1:2]
        })
        
        vals12 <- sapply(grep('OGRFeature', clc2012, value = FALSE), function(x){
          y <- c(clc2012[(x+1):(x+3)], "")
          w <- gsub('^ |^  ', '', paste0( gsub('GEOS error.+|', '',  y[1:(which(y == '')[1]-1)] ) , collapse = ''))
          z <- gsub('.+\\= ', '', strsplit(w, 'x2012 |km2')[[1]])
          c(gsub('^ | $', '', z[z != '']), NA)[1:2]
        })
        
        print(paste0('  Area calculation done!'))
        
        
        ## Convert in table
        result02 <- as.data.frame(matrix(as.numeric(gsub('^ .+\\= ', '', unlist(vals02))), ncol = 2, byrow = TRUE))
        result09 <- as.data.frame(matrix(as.numeric(gsub('^ .+\\= ', '', unlist(vals09))), ncol = 2, byrow = TRUE))
        result12 <- as.data.frame(matrix(as.numeric(gsub('^ .+\\= ', '', unlist(vals12))), ncol = 2, byrow = TRUE))
        
        allCLC <- rbind(cbind(yy = 2002, na.omit(result02)), cbind(yy = 2009, na.omit(result09)), cbind(yy = 2012, na.omit(result12)))
        colnames(allCLC) <- c('yy', 'cod', 'km2')
      }
      
      
      resultCLC <- as.data.frame.matrix(xtabs(km2 ~ cod + yy, data = allCLC))
      resultCLC$cod <- rownames(resultCLC)
      
      load(paste0(dataPath, '/clc/clcLegN', clclevel, '.RData'))
      result <- merge(resultCLC, clcLeg[ , c('legend', 'cod')], by = 'cod', all.x = TRUE,
                      all.y = FALSE, sort = TRUE)
      
      
    }
    
    
    ## Get into the forest metrics ------
    if (metric %in% 'forest') { 
      
      if (is.null(ebvstat) | is.null(sour)){
        return(paste0("ERROR: forest metric requiere 'ebvstat' and 'sour' arguments"))
        stop()
      }
      
      ## Validate parameters
      if (! ebvstat %in% c('area', 'lsm_l_tafc', landscapemetrics::list_lsm(level = 'landscape')$function_name)){
        return(paste0('ERROR: EBVstat "', ebvstat, '" not "area" or "landscape-level metrics'))
        stop()
      }
      
      if (! sour %in% c('hansen', 'ideam')){
        return(paste0('ERROR: Source "', sour, '" not "ideam" or "hansen" for forest source'))
        stop()
      }
      
      if ((is.na(ebvyear) & is.null(ebvyear))){
        return(paste0('ERROR: ebvyear must be provided'))
        stop()
      }
      
      if ((is.na(ebvporcrange) & is.null(ebvporcrange))){
        return(paste0('ERROR: ebvporcrangenum must be provided'))
        stop()
      }
      
      ebvyearnum <- as.numeric(strsplit(ebvyear, ':')[[1]])
      
      if (sour == 'hansen'){
        if( ! all(ebvyearnum %in% 2000:2018)){
          return(paste0('ERROR: ebvyear "', ebvyear, '" not in 2000:2018 for "hansen" source'))
          stop()
        }
      } else if (sour == 'ideam'){
        if(! all(ebvyearnum %in% 1990:2016)){
          return(paste0('ERROR: ebvyear "', ebvyear, '" not in 1990:2016 for "ideam" source'))
          stop()
        }
      }
      
      ebvporcrangenum <- as.numeric(strsplit(ebvporcrange, ':')[[1]])
      
      if( !all( ebvporcrangenum >= 0 & ebvporcrangenum <= 100 ) ){
        return(paste0('ERROR: ebvporcrange "', ebvporcrange, '" not in 0:100'))
        stop()
      }
      
      print(' > Validating source and stats...')
      
      
      ## Assign temporal paths to rasters for forest extent and forest loss
      
      timeMark <- gsub('[[:punct:]]| ', '', format(as.POSIXct(Sys.time(), tz="CET"), tz="America/Bogota",usetz=TRUE))
      tempRastDir <- paste0(dataPath, '/tempR/', timeMark, 
                            '_r1', round(runif(1, 0, 100)), 
                            '_r2', round(runif(1, 0, 100)), 
                            '_r3', basename(tempfile()))
      dir.create(tempRastDir, recursive = TRUE)
      print(paste0('Temp dir: ', tempRastDir))
      
      rasterOptions(tmpdir = tempRastDir, format = 'GTiff')
      treeTemp <- paste0(tempRastDir, '/', basename(tempfile()), 'tree.tif')
      lossTemp <- paste0(tempRastDir, '/', basename(tempfile()), 'loss.tif')
      
      print(paste0('Calculating results from -', metric, '- metric'))
      
      ## Cut Colombia Hansen layers with WKT geometry
      
      suppressWarnings(gdalUtilities::gdalwarp(srcfile = paste0(dataPath, '/forest/tree_', sour ,'_pcs.tif'),
                              dstfile = treeTemp,
                              csql = paste0("select ST_GeomFromText('", writeWKT(wkt_pcs, byid = F), "')"),
                              cutline = paste0(dataPath,'/singleLayers/tempSQlite.sqlite'),
                              dstnodata = 999, crop_to_cutline = TRUE,
                              overwrite = TRUE))
      
      print(' > Cutting tree cover ...')
      
      suppressWarnings(gdalUtilities::gdalwarp(srcfile = paste0(dataPath, '/forest/loss_', sour ,'_pcs.tif'), 
                              dstfile = lossTemp, 
                              csql = paste0("select ST_GeomFromText('", writeWKT(wkt_pcs, byid = F), "')"),
                              cutline = paste0(dataPath,'/singleLayers/tempSQlite.sqlite'),
                              overwrite = TRUE, dstnodata = 999, crop_to_cutline = TRUE))
      
      
      print(' > Cutting forest loss ...')
      
      # file1ad189416loss
      maskTemp <- gsub(x = lossTemp, '\\.tif', '\\_mask.tif')
      gdal_cmd <- paste0('gdal_calc.py -A ',treeTemp,' -B ',lossTemp,' --outfile=', maskTemp,' --calc="B*logical_and(A>=',ebvporcrangenum[1],
                         ',A<=',ebvporcrangenum[2],')" --NoDataValue=0 --quiet --overwrite') # --type Int16 
      suppressWarnings(system(gdal_cmd))
      print(' > Masking forest loss ...')
      
      ## Apply EBVmetric
      
      del10 <- ifelse(sour == 'ideam', 10, 0)
      
      ## Calculate metric
      if (ebvstat %in% c('area')){
        
        ## Only using pixel counts
        suppressWarnings(rastInfo <- capture.output(gdalinfo(datasetname = treeTemp)))
        pixelAreas <- prod(abs(as.numeric(gsub(" |[[:alpha:]]|([.-])|[[:punct:]]", "\\1",
                                               strsplit(grep('Pixel Size ', rastInfo, value = TRUE)
                                                        , ',')[[1]]))))/1000000
        
        countForestTime0 <- raster_count(treeTemp, n256 = TRUE)
        countForestTime0 <- sum(countForestTime0$count[
          which(countForestTime0$id >= ebvporcrangenum[1] 
                & countForestTime0$id<= ebvporcrangenum[2]) ]) * pixelAreas
        
        countYearLoss <- raster_count(maskTemp, n256 = TRUE)
        countYearLoss <- subset(countYearLoss, count != 0)
        countYearLoss$count <- countYearLoss$count * pixelAreas
        
        result <- data.frame(year = c(0, countYearLoss$id) + 2000, 
                             area = countForestTime0 - (c(0, cumsum(countYearLoss$count)) ))
        
      } else {
        
        ## Use ForestChange package
        stk <- stack(treeTemp, maskTemp) 
        names(stk) <- c("treecover2000", "lossyear")
        
        fcmask <- forestChange::FCMask(pol = stk, year = (ebvyearnum[1]:ebvyearnum[2]) + del10, 
                                       perc = eval(parse(text = ebvporcrange)), pr.utm = FALSE)
        fcmetric <- forestChange::EBVmetric(fcmask, what = ebvstat)
        fcmetricSubset <- subset(fcmetric , layer %in% ( (ebvyearnum[1]:ebvyearnum[2]) + del10 - 2000) )
        result <- data.frame(year = (ebvyearnum[1]:ebvyearnum[2]) + del10, metric = fcmetricSubset$value, row.names = fcmetricSubset$layer)
        colnames(result)[2] <- ebvstat
        
      }
      
      rownames(result) <- result$year <- result$year - del10
      result <- subset(result, year %in% ebvyearnum[1]:ebvyearnum[2])
      
      file.remove(treeTemp, lossTemp)
      unlink(tempRastDir, recursive = TRUE, force = TRUE)
    } 
    
    
    ## Get into the species metrics
    if (metric %in% c('species', 'threatspecies', 'evalspecies', 'knownspecies')){
      
      # Validate parameters
      if (! sour %in% c('biomod', 'uicn', 'records') & metric %in% 'species'){
        return(paste0('ERROR: Source "', sour, '" not "biomod", "uicn" or "records" for species metric'))
        stop()
      }
      
      if(sour == 'records'){
        
        if (! spFormat %in% c('list', 'count') | is.null(spFormat)){
          return(paste0('ERROR: spFormat "', spFormat, '" not "list" or "count" for species metric'))
          stop()
        }
        
        ## Generate MongoDB connection and query based on spatial extent (square)
        db <<- mongo(db = "biotablero", collection = "species", url ="mongodb://biotablero:admin@biotablerodb/biotablero", verbose = FALSE)
        wkt_ext <- raster::extent(wkt)
        mongoQuery <- paste0('{"decimalLongitude": {"$gte": ', wkt_ext@xmin, ', "$lte": ', wkt_ext@xmax, '}, "decimalLatitude": {"$gte": ', wkt_ext@ymin, ', "$lte": ', wkt_ext@ymax, '}}')
        mongoResult <- db$find(mongoQuery)
        
        ## Closing connection 
        rm(db); gc()
        
        ## Select records inside the geometry
        spatialQuery <- raster::extract(wkt, mongoResult[, c("decimalLongitude", "decimalLatitude")])
        mongoResult <- mongoResult[!is.na(spatialQuery$poly.ID), ]
        
        
        ## Return the records
        if (spFormat %in% c('list') ){
          result <- mongoResult
        }
        
        if (spFormat %in% c('count') ){
          if (! spRecordsTabulate %in% c('class', 'uicn_glob', 'endemic', 'invasive', 'cites') ){
            return(paste0('ERROR: spRecordsTabulate "', spRecordsTabulate, '" not "class", "uicn_glob", "endemic", "invasive" or "cites" for species metric'))
            stop()
          }
          
          result <- as.data.frame.matrix(table(unique(mongoResult[, c('class', spRecordsTabulate)]), useNA = 'ifany'))
        }
        
      }
      
      
      if(sour == 'biomod'){
        
        if (! spFormat %in% c('list', 'area') ){
          return(paste0('ERROR: spFormat "', spFormat, '" not "list" or "area" for species metric'))
          stop()
        }
        
        print(paste0('Calculating results from -', metric, '- metric'))
        
        ## Create temporal tif from ID Raster
        tempIDrast <- paste0(tempfile(), '.tif')
        timeMark <- gsub('[[:punct:]]| ', '', format(as.POSIXct(Sys.time(), tz="CET"), tz="America/Bogota",usetz=TRUE))
        
        ## Wait and avoid 1sec if other query is running on mongo
        
        tempPath <- sort(Sys.getenv(c("TEMP", 'TMP')), decreasing=T)[1]
        temp_file <- paste0(tempPath, '/', timeMark, '_Rtmp')
        availTemps <- list.files(path = tempPath, pattern = '_Rtmp$')
        
        while (TRUE){
          if(temp_file %in% availTemps){
            Sys.sleep(1)
            timeMark <- gsub('[[:punct:]]| ', '', format(as.POSIXct(Sys.time(), tz="CET"), tz="America/Bogota",usetz=TRUE))
            availTemps <- list.files(path = tempPath, pattern = '_Rtmp$')
            temp_file <- paste0(tempPath, '/', timeMark, '_Rtmp')
            print(timeMark)
          } else {
            
            break
          }
        } 
        
        
        
        
        
        ## Get the pixels ID in the region
        suppressWarnings(rastID <- gdalUtils::gdalwarp(srcfile = paste0(dataPath, '/species/biomod/idRast.tif'),
                                      dstfile = tempIDrast,
                                      cutline = paste0(dataPath, '/singleLayers/tempSQlite.sqlite'),
                                      csql = paste0("select ST_GeomFromText('", writeWKT(wkt, byid = F), "')"),
                                      crop_to_cutline = TRUE,
                                      overwrite = TRUE,
                                      output_Raster = TRUE))
        
        cellID <- as.numeric(unique(na.omit(rastID[])))
        
        ## Load species lists from those pixels. Species are numbers here
        spNumbers <- sapply(cellID, function(x){
          ld <- tryCatch(load(paste0(dataPath, '/species/biomod/spByCell/', x,'.RData')),
                         error = function(e) NULL)
          if(!is.null(ld)){
            return(sp)
          }
        })
        
        spNumbers <- unlist(spNumbers)
        
        ## Load species list 
        load(paste0(dataPath, '/species/biomod/spNames.RData'))
        load(paste0(dataPath, '/species/biomod/taxIssuesBm.RData'))
        
        ## Get the species numbers 
        if (spFormat %in% c('list') ){
          spNumbers <- unique(spNumbers)
          spNames. <- spNames[spNumbers]
          result <- taxonomyBm[taxonomyBm$speciesBlank  %in% gsub('[[:blank:]]', '', spNames.), ]
        }
        
        if (spFormat %in% c('area') ){
          spNumbers <- data.frame(table(spNumbers), stringsAsFactors = FALSE)
          spNames. <- spNames[as.numeric(as.character(spNumbers$spNumbers))]
          result <- taxonomyBm[taxonomyBm$speciesBlank  %in% gsub('[[:blank:]]', '', spNames.), ]
          result$km2 <- NA
          pos <- match(result$speciesBlank, gsub('[[:blank:]]', '', spNames.))
          result$km2[which(!is.na(pos))] <- spNumbers$Freq[na.omit(pos)]
          result
        }
      }
      
      if(sour == 'uicn'){
        print(paste0('Calculating results from -', metric, '- metric'))
        
        ## Check if is present in the area
        if (spFormat %in% c('list') ){
          uicnSpp <- ogrinfo(datasource_name = paste0(dataPath,'/species/uicn/sp_UICN.shp'),
                             dialect = 'sqlite',
                             sql = paste0("SELECT binomial FROM sp_UICN WHERE ST_Intersects(GEOMETRY, ST_GeomFromText('",
                                          writeWKT(wkt, byid = F),"'))"))
          binomSpp <- grep(' binomial \\(String\\) ', uicnSpp, value = TRUE)
          result <- data.frame(species = unique(gsub('^ | binomial | km2 |\\(.+\\= ', '', binomSpp)), stringsAsFactors = FALSE)
        }
        
        ## Calculate area
        if (spFormat %in% c('area') ){
          
          uicnSpp <- ogrinfo(datasource_name = paste0(dataPath,'/species/uicn/sp_UICN_pcs.shp'), dialect = 'sqlite',
                             sql = paste0("SELECT binomial, ST_AREA(intersection)/1000000 as km2 FROM (SELECT binomial, ST_Intersection(GEOMETRY, ST_GeomFromText('",
                                          writeWKT(wkt_pcs, byid = F), "')) AS intersection FROM sp_UICN_pcs WHERE ST_Intersects(GEOMETRY, ST_GeomFromText('", 
                                          writeWKT(wkt_pcs, byid = F),"')))"))
          
          
          # Consider GEOS error in topology
          chunks <- sapply(grep('OGRFeature', uicnSpp, value = FALSE), function(x){
            y <- uicnSpp[x:(x+8)]
            y <- paste0( gsub('GEOS error.+|', '',  y[1:(which(y == '')[1]-1)] ) , collapse = '') 
            z <- gsub('^ |OGRFeature| binomial | km2 |\\(.+\\= | $|  $|   $', '', strsplit(y, ' km2')[[1]])
          })
          
          result <- data.frame(matrix(chunks, ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
          colnames(result) <- c('species', 'km2')
          result$km2 <- as.numeric(as.character(result$km2))
          
          sumAreas <- tapply(result$km2, result$species, sum)
          result <- data.frame(species = rownames(sumAreas), km2 = unname(sumAreas), stringsAsFactors = FALSE)
          result <- result[!is.na(result$km2), ]
        }
        
        load(paste0(dataPath,'/species/uicn/sp_UICN_taxonomy.RData')) # dbfUICN
        result <- merge(result, dbfUICN, by.x = 'species', by.y = 'binomial', all.x = TRUE, all.y = FALSE)
        result
      }
      
    }
    
    
    ## Get into the surface metric
    if (metric %in% 'surface') {
      
      # Validate parameters
      if (! cellSize %in% c(1, 2, 5, 10, 20, 50, 100, 200, 500) ){
        return(paste0('ERROR: CellSize "', cellSize, '" not in the available sizes 1, 2, 5, 10, 20, 50, 100, 200, 500'))
        stop()
      }
      
      print(paste0('Calculating results from -', metric, '- metric'))
      
      suppressWarnings(surf <- raster(paste0(dataPath, '/surface/RecCoun-', cellSize, '.tif')))
      suppressWarnings(msk <- raster::crop(surf, wkt))
      if(ncell(msk) > 4){
        suppressWarnings(msk <- raster::mask(msk, wkt))
      }
      
      result <- list(percentage = mean(Which(msk > 0)[], na.rm = TRUE) * 100)
      
    }
  }
  
  ## Load pre-calculated values. A 'metrics' variable is ------
  ## stored in each RData from forest output/forest/...RData
  if (is.null(pol) & (!is.null(lay) & !is.null(polID) ) ){
    precalculateResults <- tryCatch(load(paste0(dataPath, '/output/', metric, '/', lay, '/', polID,'.RData')),
                                    error = function(e) NULL)
    if (is.null(precalculateResults)){
      return("ERROR: Not previous results for this query")
      stop()
    }
  }
  
  print(paste0('Exporting results from -', metric, '- metric'))
  timeElapsed <- Sys.time() - tStart
  
  # Return result and parameters 
  
  return(
    list(result =  result,
         #list(result =  jsonlite::toJSON(as.list(result), auto_unbox = TRUE),
         params = data.frame(params = c(metric = metric, lay = lay, polID = polID,
                                        pol = pol, spFormat = spFormat,
                                        ebvstat = ebvstat, sour = sour,
                                        clclevel = clclevel, ebvyear = ebvyear,
                                        ebvporc = ebvporcrange, dots,
                                        time = paste(timeElapsed, attr(timeElapsed, 'units')) ) ) ))
}