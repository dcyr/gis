### a small function that creates a grid for ggplot2 based on a tidy dataframe
### now for Canada, generalize later

ggGrid <- function(x) {
  areaBase <- x
  latLim <- c(floor(min(areaBase$lat)),
              ceiling(max(areaBase$lat)))
  longLim <- c(floor(min(areaBase$long)/2.5)*2.5,
               ceiling(max(areaBase$long)/2.5)*2.5)
  
  latInt <- 10
  latMin <- ceiling(min(latLim)/latInt)*latInt
  latMax <- floor(max(latLim)/latInt)*latInt
  
  longInt <- 20
  longMin <- ceiling((min(longLim)+10)/longInt)*longInt
  longMax <- floor(max(longLim)/longInt)*longInt
  
  
  latSeq <- seq(latMin, latMax, latInt)
  longSeq <- seq(longMin, longMax, longInt)
  
  latLongGrid <- rbind(data.frame(long = c(rep(min(longLim), times = length(latSeq)),
                                           rep(max(longLim), times = length(latSeq))),
                                  lat = latSeq,
                                  group = seq_along(latSeq),
                                  labels = paste0(seq(latMin, latMax, latInt), "°N")),
                       data.frame(long = longSeq,
                                  lat = c(rep(min(latLim), times = length(longSeq)),
                                          rep(max(latLim), times = length(longSeq))),
                                  group = seq_along(longSeq) + length(latSeq),
                                  labels = paste0(abs(seq(longMin, longMax, longInt)), "°W")))
  
  labSpace <- 0.5 ## in degrees 
  
  latLab <- latLongGrid[latLongGrid$long == min(latLongGrid$long),]
  latLab$long <- latLab$long-labSpace
  longLab <- latLongGrid[latLongGrid$lat == min(latLongGrid$lat),]
  longLab$lat <- longLab$lat-labSpace
  # updating xlim and ylim
  longLim[which.min(longLim)] <- min(longLim)-labSpace
  latLim[which.min(latLim)] <- min(latLim)-labSpace
  
  return(list(latLongGrid = latLongGrid,
              latLab = latLab,
              longLab = longLab,
              latLim = latLim,
              longLim = longLim))
  
}
