### Dominic Cyr
rm(list = ls())
# home <- path.expand("~")
# home <- gsub("/Documents", "", home) # necessary on my Windows machine
# setwd(paste(home, "Sync/Travail/ECCC/GIS", sep ="/"))
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


get(load("../CanEcologicalFramework/ecozones.RData"))
ecozones_AT <- read.csv("../CanEcologicalFramework/ecozones_AT.csv")
ecozonesF <- tidy(ecozones, region = "ECOZONE")

#########################################
### Actual plotting - en & fr
source("../ggScripts/ggBaseMapCanada.R")
#########################################
title <- c(en = "Ecological Framework of Canada",
           fr = "Cadre écologique du Canada")
subtitle <- c(en = "Ecozones",
              fr = "Écozones")
caption <- c(en = "Source: http://sis.agr.gc.ca/\nAuthor: Dominic Cyr - Environment and Climate change Canada",
             fr = "Source: http://sis.agr.gc.ca/\nAuteur: Dominic Cyr - Environnement et Changement climatique Canada")


colEcozones <- c("MixedWood Plains" ="palegreen",
                 "Atlantic Maritime" = "seagreen3",
                 "Boreal Shield" = "palegreen4",
                 "Prairies" = "navajowhite1",
                 "Boreal Plains" = "darkseagreen2",
                 "Montane Cordillera" = "darkkhaki",
                 "Pacific Maritime" = "darkolivegreen4",
                 "Boreal Cordillera" = "burlywood4",
                 "Taiga Shield" = "bisque3",
                 "Taiga Plains" ="burlywood3",
                 "Taiga Cordillera" = "darkslategray4",
                 "Hudson Plains" ="lemonchiffon4",
                 "Southern Arctic" = "lightblue1",
                 "Northern Arctic" = "lightcyan1",
                 "Arctic Cordillera" ="lightblue3")

ecozonesF$id <- factor(ecozones_AT[match(ecozonesF$id, ecozones_AT$ECOZONE), "ZONE_NAME"],
                                   levels = names(colEcozones))

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
### Ecozones - actual plotting
#########################################
for (shade in c("dark", "light")) {
    cols <- lapply(colors, function(x) as.character(x[shade]))
    
    #########################################
    ### Fetching and preparing base map
    base <- ggBaseCanada(shade = shade)
    
    
    for (lang in c("fr", "en" )) {
        
        if(lang == "fr") {
            lab <- as.character(ecozones_AT$ZONE_NOM)
            names(lab) <- as.character(ecozones_AT$ZONE_NAME)
        }
        if(lang == "en") {
            lab <- as.character(ecozones_AT$ZONE_NAME)
            names(lab) <- as.character(ecozones_AT$ZONE_NAME)
        }
        
        
        png(filename =  paste0("ecozones_", lang, "_", shade, ".png"),
            width = 7.5, height = 4.5, units = "in", res = 300, pointsize = 10, bg = "grey25")
        
        print(base$base +
                  geom_polygon(data = ecozonesF,
                               aes(x = long, y = lat, group = group, fill=id),
                               colour = NA) +
                  scale_fill_manual(name = "",
                                    values = colEcozones,
                                    labels = lab,
                                    guide = guide_legend(reverse = TRUE)) +
                  geom_polygon(data = base$can, aes(x = long, y = lat, group = group),
                               colour = cols$baseCol,
                               fill = NA,
                               size = 0.1) +
                  labs(title = paste0(title[lang]),
                       subtitle = paste0(subtitle[lang]),
                       caption = caption[lang]))
        dev.off()
    }
}




