library(plyr)
library(sp)
library(uncertainty)

#set local workspace
setwd("/mnt/lfs2/erichs/git/farm_vision_apples/data")

#load list of gala files
temp = list.files(pattern="b-gala")

#separate even and odd files
even_gala <- temp[c(1,3,5,7,9)]
odd_gala <- temp[c(2,4,6,8,10)]

#load list of honeycrisp files
temp = list.files(pattern="b-honeycrisp")

#separate even and odd files
odd_honeycrisp <- temp[c(1,3,5,7)]
even_honeycrisp <- temp[c(2,4,6,8)]


#BEGIN Spatial mapping loops

for (i in even_gala) {
  ii <- gsub('.{11}$', '', i)
  row <- read.csv(i, header=TRUE, strip.white = TRUE)
  row_ontree <- row[row$height.off.ground > 0,]
  row_ontree_true <- subset(row_ontree, in.calibration.sample == "True")
  odd_row_sp <- SpatialPointsDataFrame(cbind.data.frame(row_ontree$lat, row_ontree$lon),  row_ontree)
  odd_row_sp_true <- SpatialPointsDataFrame(cbind.data.frame(row_ontree_true$lat, row_ontree_true$lon),  row_ontree_true)
  plot(odd_row_sp, add = TRUE, pch=19, cex=.2, col="green")
  plot(odd_row_sp_true, add = TRUE,  pch=19, cex=.2, col="red")
  
}

for (i in even_honeycrisp) {
  ii <- gsub('.{11}$', '', i)
  row <- read.csv(i, header=TRUE, strip.white = TRUE)
  row_ontree <- row[row$height.off.ground > 0,]
  row_ontree_true <- subset(row_ontree, in.calibration.sample == "True")
  odd_row_sp <- SpatialPointsDataFrame(cbind.data.frame(row_ontree$lat, row_ontree$lon),  row_ontree)
  odd_row_sp_true <- SpatialPointsDataFrame(cbind.data.frame(row_ontree_true$lat, row_ontree_true$lon),  row_ontree_true)
  plot(odd_row_sp, add = TRUE, pch=19, cex=.2, col="blue")
  plot(odd_row_sp_true, add = TRUE,  pch=19, cex=.2, col="red")
}

for (i in odd_gala) {
  ii <- gsub('.{11}$', '', i)
  row <- read.csv(i, header=TRUE, strip.white = TRUE)
  row_ontree <- row[row$height.off.ground > 0,]
  row_ontree_true <- subset(row_ontree, in.calibration.sample == "True")
  odd_row_sp <- SpatialPointsDataFrame(cbind.data.frame(row_ontree$lat, row_ontree$lon),  row_ontree)
  #odd_row_sp_true <- SpatialPointsDataFrame(cbind.data.frame(row_ontree_true$lat, row_ontree_true$lon),  row_ontree_true)
  plot(odd_row_sp, add = TRUE, pch=19, cex=.2, col="green")
  #plot(odd_row_sp_true, add = TRUE,  pch=19, cex=.2, col="red")
}

for (i in odd_honeycrisp) {
  ii <- gsub('.{11}$', '', i)
  row <- read.csv(i, header=TRUE, strip.white = TRUE)
  row_ontree <- row[row$height.off.ground > 0,]
  row_ontree_true <- subset(row_ontree, in.calibration.sample == "True")
  odd_row_sp <- SpatialPointsDataFrame(cbind.data.frame(row_ontree$lat, row_ontree$lon),  row_ontree)
  #odd_row_sp_true <- SpatialPointsDataFrame(cbind.data.frame(row_ontree_true$lat, row_ontree_true$lon),  row_ontree_true)
  plot(odd_row_sp, add = TRUE, pch=19, cex=.2, col="blue")
  #plot(odd_row_sp_true, add = TRUE,  pch=19, cex=.2, col="red")
}

#END - Spatial Mapping loops


#BEGIN Spatial mapping loops EW

for (i in even_gala) {
  ii <- gsub('.{11}$', '', i)
  row <- read.csv(i, header=TRUE, strip.white = TRUE)
  row_ontree <- row[row$height.off.ground > 0,]
  row_ontree_true <- subset(row_ontree, in.calibration.sample == "True")
  
  row_ontree_E <- row_ontree[row_ontree$side == "E",]
  row_ontree_W <- row_ontree[row_ontree$side == "W",]
  
  odd_row_sp <- SpatialPointsDataFrame(cbind.data.frame(row_ontree$lat, row_ontree$lon),  row_ontree)
  odd_row_sp_true <- SpatialPointsDataFrame(cbind.data.frame(row_ontree_true$lat, row_ontree_true$lon),  row_ontree_true)
  odd_row_sp_E <- SpatialPointsDataFrame(cbind.data.frame(row_ontree_E$lat, row_ontree_E$lon),  row_ontree_E)
  odd_row_sp_W <- SpatialPointsDataFrame(cbind.data.frame(row_ontree_W$lat, row_ontree_W$lon),  row_ontree_W)
  
  plot(odd_row_sp_W, add = TRUE, pch=19, cex=.2, col="green")
  plot(odd_row_sp_E, add = TRUE, pch=19, cex=.2, col="blue")
  plot(odd_row_sp_true, add = TRUE,  pch=19, cex=.2, col="red")
  
}

for (i in even_honeycrisp) {
  ii <- gsub('.{11}$', '', i)
  row <- read.csv(i, header=TRUE, strip.white = TRUE)
  row_ontree <- row[row$height.off.ground > 0,]
  row_ontree_true <- subset(row_ontree, in.calibration.sample == "True")
  
  row_ontree_E <- row_ontree[row_ontree$side == "E",]
  row_ontree_W <- row_ontree[row_ontree$side == "W",]
  
  odd_row_sp <- SpatialPointsDataFrame(cbind.data.frame(row_ontree$lat, row_ontree$lon),  row_ontree)
  odd_row_sp_true <- SpatialPointsDataFrame(cbind.data.frame(row_ontree_true$lat, row_ontree_true$lon),  row_ontree_true)
  odd_row_sp_E <- SpatialPointsDataFrame(cbind.data.frame(row_ontree_E$lat, row_ontree_E$lon),  row_ontree_E)
  odd_row_sp_W <- SpatialPointsDataFrame(cbind.data.frame(row_ontree_W$lat, row_ontree_W$lon),  row_ontree_W)
  
  plot(odd_row_sp_W, add = TRUE, pch=19, cex=.2, col="green")
  plot(odd_row_sp_E, add = TRUE, pch=19, cex=.2, col="blue")
  plot(odd_row_sp_true, add = TRUE,  pch=19, cex=.2, col="red")
}

for (i in odd_gala) {
  ii <- gsub('.{11}$', '', i)
  row <- read.csv(i, header=TRUE, strip.white = TRUE)
  row_ontree <- row[row$height.off.ground > 0,]
  row_ontree_true <- subset(row_ontree, in.calibration.sample == "True")
  
  row_ontree_E <- row_ontree[row_ontree$side == "E",]
  row_ontree_W <- row_ontree[row_ontree$side == "W",]
  
  odd_row_sp <- SpatialPointsDataFrame(cbind.data.frame(row_ontree$lat, row_ontree$lon),  row_ontree)
  #odd_row_sp_true <- SpatialPointsDataFrame(cbind.data.frame(row_ontree_true$lat, row_ontree_true$lon),  row_ontree_true)
  odd_row_sp_E <- SpatialPointsDataFrame(cbind.data.frame(row_ontree_E$lat, row_ontree_E$lon),  row_ontree_E)
  odd_row_sp_W <- SpatialPointsDataFrame(cbind.data.frame(row_ontree_W$lat, row_ontree_W$lon),  row_ontree_W)
  
  plot(odd_row_sp_W, add = TRUE, pch=19, cex=.2, col="green")
  plot(odd_row_sp_E, add = TRUE, pch=19, cex=.2, col="blue")
  #plot(odd_row_sp_true, add = TRUE,  pch=19, cex=.2, col="red")
}

for (i in odd_honeycrisp) {
  ii <- gsub('.{11}$', '', i)
  row <- read.csv(i, header=TRUE, strip.white = TRUE)
  row_ontree <- row[row$height.off.ground > 0,]
  row_ontree_true <- subset(row_ontree, in.calibration.sample == "True")
  
  row_ontree_E <- row_ontree[row_ontree$side == "E",]
  row_ontree_W <- row_ontree[row_ontree$side == "W",]
  
  odd_row_sp <- SpatialPointsDataFrame(cbind.data.frame(row_ontree$lat, row_ontree$lon),  row_ontree)
  #odd_row_sp_true <- SpatialPointsDataFrame(cbind.data.frame(row_ontree_true$lat, row_ontree_true$lon),  row_ontree_true)
  odd_row_sp_E <- SpatialPointsDataFrame(cbind.data.frame(row_ontree_E$lat, row_ontree_E$lon),  row_ontree_E)
  odd_row_sp_W <- SpatialPointsDataFrame(cbind.data.frame(row_ontree_W$lat, row_ontree_W$lon),  row_ontree_W)
  
  plot(odd_row_sp_W, add = TRUE, pch=19, cex=.2, col="green")
  plot(odd_row_sp_E, add = TRUE, pch=19, cex=.2, col="blue")
  #plot(odd_row_sp_true, add = TRUE,  pch=19, cex=.2, col="red")
}
