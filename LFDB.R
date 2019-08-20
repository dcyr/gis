#### a script to plot fires from the Large Fire Database
rm(list = ls())
setwd("~/Travail/GIS")
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

#########################################
### National fire database - fetching and preparing data
dataDir <- "~/Travail/GIS/feux/LFDB/shapefile_2017/"
#########################################
### reading NFDB shapefile (large file! )
NFDB_poly <- readOGR(dsn = path.expand(dataDir),
                     layer = "NFDB_poly_20170928")
### subsetting
years <- c(1981, min(as.numeric(format(Sys.Date(), "%Y")), 2015))#max(NFDB_poly$YEAR)))
minSize <- 10000 # in ha
#
index <- which(NFDB_poly$YEAR >= min(years) &
                 NFDB_poly$YEAR <= max(years) &
                 NFDB_poly$CFS_HA >= minSize)
NFDB_subset <- NFDB_poly[index,]
## transform into the same CRS than base map
NFDB_subset <- spTransform(NFDB_subset, workingCRS) ### projection
NFDB_subset <- tidy(NFDB_subset)
#########################################


#########################################
### Color palette, dark and light base map
#########################################
colors <- list(col = c(dark = "black", light = "black"),
               baseFill = c(dark = "grey10", light = "grey90"),  ## canada base fill
               baseCol =  c(dark = "grey", light = "grey35"), ## canada base color (lines)
               waterFill =  c(dark = "grey35", light = "grey75"),
               textCol = c(dark = "black", light = "black"),
               gridCol = c(dark = "black", light = "black"),
               background = c(dark = "grey25", light = "white"),
               titleCol = c(dark = "white", light = "black"),
               captionCol = c(dark = "grey", light = "grey25"),
               legendCol = c(dark = "grey25", light = "white"))

#########################################
### Actual plotting - en & fr
#########################################
title <- c(en = "Burned areas in Canada",
           fr = "Aires brûlées au Canada")
subtitle <- c(en = "National Fire Database",
              fr = "Base nationale de donnÃ©es sur les feux de forêt")
caption <- c(en = "Source: http://cwfis.cfs.nrcan.gc.ca/ha/nfdb\nAuthor: Dominic Cyr - Environment and Climate change Canada",
             fr = "Source: http://cwfis.cfs.nrcan.gc.ca/ha/nfdb\nAuteur: Dominic Cyr - Environnement et Changement climatique Canada")


for (shade in c("dark", "light")) {
  
  cols <- lapply(colors, function(x) as.character(x[shade]))
  ### base map
  base <- ggBaseCanada(shade = shade)
  
  for (lang in c("fr", "en")) {
    png(filename = paste0("NFDB_",  min(years), "-", max(years), "_", lang, "_", shade, ".png"),
        width = 6, height = 4.5, units = "in", res = 300, pointsize = 10, bg = "grey25")
    
    
    print(base$base + geom_polygon(data = NFDB_subset, aes(x = long, y = lat, group = group),
                                   colour = NA,
                                   fill = "red1") +
            geom_polygon(data = base$can, aes(x = long, y = lat, group = group),
                         colour = cols$baseCol,
                         fill = NA,
                         size = 0.1) +
            labs(title = paste0(title[lang]),
                 subtitle = paste0(subtitle[lang]," (", min(years), "-", max(years), ")"),
                 caption = caption[lang])
    )
    dev.off()  
  }
}





