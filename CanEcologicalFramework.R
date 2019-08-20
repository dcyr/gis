################################################################################
################################################################################
### Fetch, clean up, plot, and store tidy maps of Canadian National
### Ecological Framework
### Dominic Cyr
#############
rm(list = ls())
home <- path.expand("~")
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/GIS", sep ="/"))
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)
require(raster)
require(sp)
require(rgdal)
require(ggplot2)
require(broom)
require(rgeos)

source("../ggScripts/ggBaseMapCanada.R")
workingCRS <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


# ################################################################################
# #### Fetching and saving (only needed once, or if source data needs to be  updated,
# #### else, comment out this block cause it's long to execute)
# 
# urlList <- c(ecozone = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
#              ecoprovince = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/province/ecoprovince_shp.zip",
#              ecoregion = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
#              ecodistrict = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip")
# 
# terrestrialCrop <- TRUE ### must be true to
# lvls <- c("ecozone",
#           "ecoprovince",
#           "ecodistrict",
#           "ecoregion")
# 
# if (terrestrialCrop) {
#     can <- getData('GADM', country="CAN", level=1)
# }
# 
# require(parallel)
# require(doSNOW)
# n <- min(detectCores(), length(lvls))
# 
# ####### loop to execute all hierarchical levels at once
# cl = makeCluster(n, outfile = "") ##
# registerDoSNOW(cl)
# 
# foreach(i = lvls)  %dopar% {
#     require(rgdal)
#     require(raster)
# 
#     url <- urlList[i]
#     ###
#     tmpFile <- tempfile()
#     tmpDir <- tempdir()
#     download.file(url, tmpFile)
#     ### unzipping
#     fileNames <- unzip(tmpFile, list=TRUE)[,1]
#     unzip(zipfile = tmpFile,  files = fileNames, exdir=tmpDir)
#     ### reading shapefile
#     fileIndex <- grep("\\.", basename(fileNames))
#     zipFolder <- basename(fileNames[-fileIndex])
#     shapefileName <- basename(fileNames)[fileIndex]
#     shapefileName <- unique(as.character(lapply(strsplit(shapefileName, "\\."), function(x) x[[1]])))
#     ### loading shapefile
#     sf <- readOGR(dsn = paste(tmpDir, zipFolder, sep = "/"),
#                         layer = shapefileName)
#     sf <- spTransform(sf, workingCRS) ### projection
# 
#     ### intersection (takes 6 minutes for entire Canada)
#     if (terrestrialCrop) {
#         sf <- intersect(sf, can)
#     }
# 
#     ### renaming and saving
#     sfName <- paste0(i,"s")
#     assign(sfName, sf)
#     save(list = sfName, file = paste0(sfName, ".RData"))
# }
# rm(can)
# #####
# ################################################################################

get(load("../CanEcologicalFramework/ecozones.RData"))
#get(load("../CanEcologicalFramework/ecoprovinces.RData"))
get(load("../CanEcologicalFramework/ecoregions.RData"))
#get(load("../CanEcologicalFramework/ecodistricts.RData"))



nameEcozone <- list(en = c("MixedWood Plain" = "MixedWood Plains",
                           "Atlantic Maritime" = "Atlantic Maritime",
                           "Boreal Shield" = "Boreal Shield",
                           "Prairie" =  "Prairies",
                           "Boreal PLain" = "Boreal Plains",
                           "Montane Cordillera" = "Montane Cordillera",
                           "Pacific Maritime" = "Pacific Maritime",
                           "Boreal Cordillera" = "Boreal Cordillera",
                           "Taiga Shield" = "Taiga Shield",
                           "Taiga Plain" = "Taiga Plains",
                           "Taiga Cordillera" = "Taiga Cordillera",
                           "Hudson Plain" = "Hudson Plains" ,
                           "Southern Arctic"= "Southern Arctic",
                           "Northern Arctic" = "Northern Arctic",
                           "Arctic Cordillera" ="Arctic Cordillera"),
                    fr = c("MixedWood Plain" = "Plaines à forêts mixtes",
                           "Atlantic Maritime" = "Maritime de l'Atlantique",
                           "Boreal Shield" = "Bouclier boréal",
                           "Prairie" =  "Prairies",
                           "Boreal PLain" = "Plaines boréales",
                           "Montane Cordillera" = "Cordillère montagnarde",
                           "Pacific Maritime" = "Maritime du Pacifique",
                           "Boreal Cordillera" = "Cordillère boréale",
                           "Taiga Shield" = "Taïga du Bouclier",
                           "Taiga Plain" = "Taïga des plaines",
                           "Taiga Cordillera" = "Cordillère de la Taïga",
                           "Hudson Plain" = "Plaines hudsonniennes" ,
                           "Southern Arctic"= "Bas-Arctique",
                           "Northern Arctic" = "Haut-Arctique",
                           "Arctic Cordillera" ="Cordillère arctique"))

################################################################################
#### rasterizing ecozones and ecoregions
r <- raster(ext = extent(ecozones),
            crs = crs(ecozones),
            res = 0.1)

################################
#### Ecozones
ecozonesR <- rasterize(ecozones, r, field = "ECOZONE")
ecozones_AT <- distinct(as.data.frame(ecozones)[,c("ECOZONE", "ZONE_NAME", "ZONE_NOM")])
ecozones_AT[,"ZONE_NAME_ORIG"] <- ecozones_AT$ZONE_NAME
    
    
# correcting errors in ecozone names
ecozones_AT$ZONE_NOM <- nameEcozone[["fr"]][match(ecozones_AT$ZONE_NAME,
                                                  names(nameEcozone[["en"]]))]
ecozones_AT$ZONE_NAME <- nameEcozone[["en"]][match(ecozones_AT$ZONE_NAME,
                                           names(nameEcozone[["en"]])) ]

# reordering AT
ecozones_AT <- ecozones_AT[order(ecozones_AT$ECOZONE),]
# saving
writeRaster(ecozonesR, file = "ecozones.tif", overwrite = T)
write.csv(ecozones_AT, file = "ecozones_AT.csv", row.names = F)

################################
#### Ecoregions
ecoregionsR <- rasterize(ecoregions, r, field = "ECOREGION")
ecoregions_AT <- distinct(as.data.frame(ecoregions)[,c("ECOREGION", "REGION_NAM", "REGION_NOM")])
# reordering AT
ecoregions_AT <- ecoregions_AT[order(ecoregions_AT$ECOREGION),]
# saving
writeRaster(ecoregionsR, file = "ecoregions.tif", overwrite = T)
write.csv(ecoregions_AT, file = "ecoregions_AT.csv", row.names = F)



