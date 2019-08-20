### a function that fetch waterbodies' polygons, and prepare data for ggplot2

ggWaterbodies <- function(dsn = path.expand("../hydro"), layer = "canadlake_p",
                          minSizeHa = 100000) {
  ### large waterbodies
  lakes <- readOGR(dsn = dsn, layer = layer)
  minSize <- minSizeHa*1000 # convert in sq-meters
  #minimum size for displaing lakes (in ha)
  lakes$area_ha <- area(lakes)
  index <- which(lakes$area_ha >= minSize)
  lakes <- lakes[index,]
  lakes <- lakes[which(complete.cases(as.data.frame(lakes))),]
  ### could make a merge with original attribute to get names, etc.
  x <- tidy(lakes)
  rm(lakes)
  return(x)
}