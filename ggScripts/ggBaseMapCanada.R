#### a function that generate base map for Canada (with language and shade)

ggBaseCanada <- function(shade,
                         waterbodies = TRUE,
                         waterbodiesMinSizeHa = 100000,
                         grid = TRUE) {
  
  require(maps)
  require(ggplot2)
  require(dplyr)
  require(raster)
  require(rgdal)
  require(broom)
  
  
  # world <- map_data("world")
  ### can select regions
  
  
  ######################################################################
  ### get map from "Global Administrative Areas" GADM database
  ######################################################################
  can <- getData('GADM', country="CAN", level=1)  ## level 1: provinces, level 2:
  crsCan = crs(can)
  # convert to a format appropriate for ggplot2
  can <- tidy(can)
  
  
  ######################################################################
  ### various map elements (called from external scripts)
  ######################################################################
  
  source("../ggScripts/ggGrid.R")
  grid <- ggGrid(can)
  
  if(waterbodies) {
    source("../ggScripts/ggWaterbodies.R")
    wb <- ggWaterbodies(minSizeHa = waterbodiesMinSizeHa)
  }
  
  
  
  ### building base map
  base <- ggplot() +
    geom_line(data = grid$latLongGrid, aes(x = long, y = lat, group = group),
              col = cols$col, size = 0.1) +
    geom_polygon(data = can, aes(x = long, y = lat, group = group),
                 colour = NA,
                 fill = cols$baseFill,
                 size = 0.1) 
  if(waterbodies) {
    base <- base +
      geom_polygon(data = wb, aes(x = long, y = lat, group = group),
                   colour = NA,
                   fill = cols$waterFill) 
  }
  
  base <- base +
    scale_x_continuous(limits = grid$longLim) +
    scale_y_continuous(limits = grid$latLim) +
    geom_text(data = grid$latLab,
              aes(x = long, y = lat, label = labels),
              hjust = 1, vjust = 0.5,
              colour = cols$textCol, size = 2) +
    geom_text(data = grid$longLab,
              aes(x = long, y = lat, label = labels),
              hjust = 0.5, vjust = 1,
              colour = cols$textCol, size = 2) +
    coord_map("azequidistant") +
    theme(panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = cols$background),
          plot.background = element_rect(fill = cols$background, colour = cols$background),
          text = element_text(colour = cols$titleCol),
          plot.caption = element_text(size = rel(0.5), colour = cols$captionCol), ####
          #legend.box = element_rect(colour = "black"),
          legend.background = element_rect(fill = cols$background),
          legend.text = element_text(size = rel(.65)), 
          legend.key = element_blank())  +
    geom_polygon(data = can, aes(x = long, y = lat, group = group),
                 colour = cols$baseCol,
                 fill = NA,
                 size = 0.1)
  return(list(base = base,
              can = can,
              crs = crsCan)
             )
  
}



