################################################################################
################################################################################
### Produce some maps for the NIR
### Declaration zones, reconciliation units, and spatial units (to be completed)
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
require(dplyr)
require(maptools)

workingCRS <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


# ###############################################################################
# ###############################################################################
# ### Reporting zones
# ### (creating & formatting data sets)
# get(load("../CanEcologicalFramework/ecozones.RData"))
# #get(load("../CanEcologicalFramework/ecoprovinces.RData"))
# get(load("../CanEcologicalFramework/ecoregions.RData"))
# #get(load("../CanEcologicalFramework/ecodistricts.RData"))
# ecozonesR <- raster("../CanEcologicalFramework/ecozones.tif")
# 
# ecozoneTable <- distinct(as.data.frame(ecozones)[,c("ECOZONE", "ZONE_NAME", "ZONE_NOM")])
# ecozoneTable$ZONE_NAME <- as.character(ecozoneTable$ZONE_NAME)
# ecozoneTable$ZONE_NOM <- as.character(ecozoneTable$ZONE_NOM)
# ### a few mistakes to correct
# ecozoneTable[which(ecozoneTable$ZONE_NOM == "Plaines à foréts mixtes"), "ZONE_NOM"] <- "Plaines à forÃªts mixtes"
# ecozoneTable[which(ecozoneTable$ZONE_NOM == "Cordillère borèale"), "ZONE_NOM"] <- "Cordillère boréale"
# ecozoneTable[which(ecozoneTable$ZONE_NOM == "Maritime de l'Atlantique "), "ZONE_NOM"] <- "Maritime de l'Atlantique"
# ecozoneTable[which(ecozoneTable$ZONE_NOM == "Plaines hudsonniennes"), "ZONE_NOM"] <- "Plaines hudsoniennes"
# ecozoneTable[which(ecozoneTable$ZONE_NAME == "Boreal PLain"), "ZONE_NAME"] <- "Boreal Plain"
# reportZones <- ecoregions %>%
#     merge(ecozoneTable)
# 
# df <- as.data.frame(reportZones)
# df[, c("reportName", "reportNom")] <- df[, c("ZONE_NAME", "ZONE_NOM")]
# #############################
# ### naming reporting zones
# 
# ### Boreal Bhield
# df[which(df$reportName == "Boreal Shield" &
#              as.numeric(as.character(df$ECOREGION)) >= 96), "reportName"] <- "Boreal Shield East"
# df[which(df$reportName == "Boreal Shield East"), "reportNom"] <- "Bouclier boréal est"
# df[which(df$reportName == "Boreal Shield"), "reportName"] <- "Boreal Shield West"
# df[which(df$reportName == "Boreal Shield West"), "reportNom"] <- "Bouclier boréal ouest"
# 
# ### Taiga shield
# df[which(df$reportName == "Taiga Shield" &
#              as.numeric(as.character(df$ECOREGION)) >= 72) , "reportName"] <- "Taiga Shield East"
# df[which(df$reportName == "Taiga Shield East"), "reportNom"] <- "Taïga du Bouclier est"
# df[which(df$reportName == "Taiga Shield"), "reportName"] <- "Taiga Shield West"
# df[which(df$reportName == "Taiga Shield West"), "reportNom"] <- "Taïga du Bouclier ouest"
# 
# ### Prairies
# df[which(df$reportName == "Prairie" &
#              as.numeric(as.character(df$ECOREGION)) %in% c(157, 159, 160)), "reportName"] <- "Semi-arid Prairies"
# df[which(df$reportName == "Semi-arid Prairies"), "reportNom"] <- "Prairies semi-arides"
# df[which(df$reportName == "Prairie"), "reportName"] <- "Subhumid Prairies"
# df[which(df$reportName == "Subhumid Prairies"), "reportNom"] <- "Prairies subhumides"
# 
# ### reordering levels
# rzLvls <- list(en = c("MixedWood Plain" = "MixedWood Plain",
#                       "Atlantic Maritime" = "Atlantic Maritime",
#                       "Boreal Shield East" = "Boreal Shield East",
#                       "Boreal Shield West" = "Boreal Shield West",
#                       "Semi-arid Prairies" = "Semi-arid Prairies",
#                       "Subhumid Prairies" = "Subhumid Prairies",
#                       "Boreal Plain" = "Boreal Plain",
#                       "Montane Cordillera" = "Montane Cordillera",
#                       "Pacific Maritime" = "Pacific Maritime",
#                       "Boreal Cordillera" = "Boreal Cordillera",
#                       "Hudson Plain" = "Hudson Plain",
#                       "Taiga Shield East" = "Taiga Shield East",
#                       "Taiga Shield West" = "Taiga Shield West",
#                       "Taiga Plain" = "Taiga Plain",
#                       "Taiga Cordillera" = "Taiga Cordillera",
#                       "Southern Arctic" = "Southern Arctic",
#                       "Northern Arctic" = "Northern Arctic",
#                       "Arctic Cordillera" = "Arctic Cordillera"),
# 
#                fr = c("MixedWood Plain" = "Plaines à forêts mixtes",
#                       "Atlantic Maritime" = "Maritime de l'Atlantique",
#                       "Boreal Shield East" = "Bouclier boréal est",
#                       "Boreal Shield West" = "Bouclier boréal ouest",
#                       "Semi-arid Prairies" = "Prairies semi-arides",
#                       "Subhumid Prairies" = "Prairies subhumides",
#                       "Boreal Plain" = "Plaines boréales",
#                       "Montane Cordillera" = "Cordillère montagnarde",
#                       "Pacific Maritime" = "Maritime du Pacifique",
#                       "Boreal Cordillera" = "Cordillère boréale",
#                       "Hudson Plain" = "Plaines hudsoniennes" ,
#                       "Taiga Shield East" = "Taïga du Bouclier est",
#                       "Taiga Shield West" = "Taïga du Bouclier ouest",
#                       "Taiga Plain" = "Taïga des plaines",
#                       "Taiga Cordillera" = "Taïga de la Cordillère",
#                       "Southern Arctic" = "Bas-Arctique",
#                       "Northern Arctic" = "Haut-Arctique",
#                       "Arctic Cordillera" = "Cordillère arctique"))
# 
# df$reportName <- factor(df$reportName, levels = rzLvls[["en"]])
# df$reportNom <- factor(df$reportNom, levels = rzLvls[["fr"]])
# reportZones_NIR <- merge(reportZones, df)
# ## rasterizing reporting zones
# reportZonesR <- rasterize(reportZones_NIR, ecozonesR, field = "reportName")
# reportZones_AT <- distinct(as.data.frame(reportZones_NIR[,c("reportName", "reportNom")]))
# reportZones_AT <- reportZones_AT[match(names(rzLvls[["en"]]),reportZones_AT$reportName),]
# reportZones_AT <- data.frame(id = 1:nrow(reportZones_AT), reportZones_AT)
# 
# ## saving
# save(reportZones_NIR, file = "reportZones_NIR.RData")
# writeRaster(reportZonesR, file = "reportZones.tif", overwrite = T)
# write.csv(reportZones_AT, file = "reportZones_AT.csv", row.names = F)


# ###############################################################################
# ###############################################################################
# ### Managed / Unmanaged
# reportZones <- raster("../NIR/reportZones.tif")
# ### (creating & formatting data sets)
# originalShapeFile <- "PSPUS_2017_MFUF" ### MFUF : Managed forest / Unmanaged forest
# ###
# mfuf <- readOGR(dsn = "../NIR/shapefiles", layer = originalShapeFile)
# mfuf <- spTransform(mfuf, workingCRS)
# ###
# mfufR <- rasterize(mfuf, reportZones)
# mfuf_AT <- data.frame(levels(mfufR))
# # saving files
# save(mfuf, file = "mfuf.RData")
# writeRaster(mfufR, file = "mfufR.tif", overwrite = T)
# write.csv(mfuf_AT[,c("ID", "ManagedFor")], file = "mfuf_AT.csv", row.names = F)
# 
# 
# ###############################################################################
# ###############################################################################
# ### Reconciliation units, spatial units, etc.
# reportZones <- raster("../NIR/reportZones.tif")
# ### (creating & formatting data sets)
# originalShapeFile <- "PSPUS_2017_newBC" ### Spatial units and Reconciliation units
# ###
# spu <- readOGR(dsn = "../NIR/shapefiles", layer = originalShapeFile)
# spu <- spTransform(spu, workingCRS)
# ###
# spuR <- rasterize(spu, reportZones)
# spuR_AT <- data.frame(levels(spuR))
# # saving files
# save(spu, file = "spu.RData")
# writeRaster(spuR, file = "spuR.tif", overwrite = T)
# write.csv(spuR_AT, file = "spuR_AT.csv", row.names = F)

################################################################################
################################################################################
#### Plotting reporting zones
get(load("../NIR/reportZones_NIR.RData"))
reportZones_AT <- read.csv("../NIR/reportZones_AT.csv")
df <- tidy(reportZones_NIR, region = "reportName")
df$id <- factor(df$id, levels = reportZones_AT$reportName)      
### managed forests, if necessary 
get(load("../NIR/mfuf.RData"))
mf <- mfuf[mfuf$OBJECTID == 1,]
mf_df <- tidy(mf, region = "OBJECTID")


#########################################
### Actual plotting - en & fr
source("../ggScripts/ggBaseMapCanada.R")
#########################################
title <- c(en = "Canada's GHG National Inventory",
           fr = "Inventaire national canadien des GES")
subtitle <- c(en = "Reporting Zones",#- Managed lands outlined in red",
              fr = "Zones de déclaration")# - Terres aménagées indiquées en rouge ")

caption <- c(en = "Source: http://https://www.canada.ca/en/environment-climate-change/services/climate-change/greenhouse-gas-emissions/inventory.html/\nAuthor: Dominic Cyr - Environment and Climate change Canada",
             fr = "Source: https://www.canada.ca/fr/environnement-changement-climatique/services/changements-climatiques/emissions-gaz-effet-serre/inventaire.html/\nAuteur: Dominic Cyr - Environnement et Changement climatique Canada")

colRz <- c("MixedWood Plain" ="palegreen",
           "Atlantic Maritime" = "seagreen3",
           "Boreal Shield East" = "palegreen4",
           "Boreal Shield West" = "palegreen3",
           "Semi-arid Prairies" = "navajowhite2",
           "Subhumid Prairies" = "navajowhite1",
           "Boreal Plain" = "darkseagreen2",
           "Montane Cordillera" = "darkkhaki",
           "Pacific Maritime" = "darkolivegreen4",
           "Boreal Cordillera" = "burlywood4",
           "Hudson Plain" ="lemonchiffon4",
           "Taiga Shield East" = "bisque2",
           "Taiga Shield West" = "bisque3",
           "Taiga Plain" ="burlywood3",
           "Taiga Cordillera" = "darkslategray4",
           "Southern Arctic" = "lightblue1",
           "Northern Arctic" = "lightcyan1",
           "Arctic Cordillera" ="lightblue3")


#########################################
### Reporting zones - actual plotting
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



for (shade in c("dark", "light")) {
    cols <- lapply(colors, function(x) as.character(x[shade]))
    ### base map
    base <- ggBaseCanada(shade = shade)
    #########################################
    ### Fetching and preparing base map
    
    
    for (lang in c("fr", "en" )) {
        
        if(lang == "fr") {
            lab <- as.character(reportZones_AT$reportNom)
            names(lab) <- as.character(reportZones_AT$reportName)
        }
        if(lang == "en") {
            lab <- as.character(reportZones_AT$reportName)
            names(lab) <- as.character(reportZones_AT$reportName)
        }
        png(filename =  paste0("reportingZones_", lang, "_", shade, ".png"),
            width = 7.5, height = 4.5, units = "in", res = 300, pointsize = 10, bg = "grey25")
        
        print(base$base +
                  geom_polygon(data = df,
                               aes(x = long, y = lat, group = group, fill=id),
                               colour = NA) +
                  scale_fill_manual(name = "",
                                    values = colRz,
                                    labels = lab,
                                    guide = guide_legend(reverse = TRUE)) +
                  geom_polygon(data = base$can, aes(x = long, y = lat, group = group),
                               colour = cols$baseCol,
                               fill = NA,
                               size = 0.1) +
                  # geom_polygon(data = mf_df, aes(x = long, y = lat, group = group),
                  #              colour = "firebrick4",
                  #              fill = NA,
                  #              size = 0.2) +
                  labs(title = paste0(title[lang]),
                       subtitle = paste0(subtitle[lang]),
                       caption = caption[lang]))
        dev.off()
    }
}

