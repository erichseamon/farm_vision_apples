library(plyr)


setwd("/mnt/lfs2/erichs/git/farm_vision_apples/data")
temp = list.files(pattern="b-gala")

even_gala <- temp[c(1,3,5,7,9)]
odd_gala <- temp[c(2,4,6,8,10)]

temp = list.files(pattern="b-honeycrisp")
odd_honeycrisp <- temp[c(1,3,5,7)]
even_honeycrisp <- temp[c(2,4,6,8)]

calibration <- read.csv("hand-counts-and-sizes.csv", header=TRUE)

#oblate ellipsoid calibration calculation

calibration$west_A_volume <- (4/3 *pi * calibration$west_width_A^2 * calibration$west_height_A)

for (i in temp_even_gala) {
  ii <- gsub('.{11}$', '', i)
row <- read.csv(i, header=TRUE)
  row_ontree <- row[row$height.off.ground > 0,]
calibration_row <- calibration[calibration$name == ii,]
  
    
}



for (i in temp_even_honeycrisp) {
  ii <- gsub('.{11}$', '', i)
  row <- read.csv(i, header=TRUE)
  row_ontree <- row[row$height.off.ground > 0,]
  calibration_row <- calibration[calibration$name == ii,]
  
  
}






myfiles = lapply(temp, read.csv)
df <- ldply(myfiles, data.frame)
