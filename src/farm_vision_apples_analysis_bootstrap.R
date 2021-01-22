#farm_vision_apples_bootstrap.R


library(pdqr)
library(magrittr)
set.seed(101)


#bootstrapping the ratio of H to W

galaboot <- c(1,2,5,6,9)
ratio_df <- matrix(NA, nrow=9, ncol=5)
for (i in galaboot) {
# Using ordinary bootstrap
set.seed(101)
d_mpg_dis_mean <- as.numeric(calibration[i,c(18,19,20,21)]) %>% 
  new_d(type = "continuous") %>% 
  form_estimate(stat = mean, sample_size = calibration[i,3])
# Spread of this bootstrap distribution describes the precision of estimation:
# bigger values indicate lower precision
ratio_df[i,1] <- summ_sd(d_mpg_dis_mean)
ratio_df[i,2] <- summ_range(d_mpg_dis_mean)
ratio_df[i,3] <- summ_mean(d_mpg_dis_mean)
ratio_df[i,4:5] <- summ_quantile(d_mpg_dis_mean,probs = c(.05,.95))

## [1] 1.04067
# This discrete distribution has the following d-function
plot(d_mpg_dis_mean, main = paste("Ordinary bootstrap distribution of HW Ratio: GALA row ", i, sep=""), xlab = paste("HW Ratio: n = ", calibration[i,3], sep=""))
#plot(as_p(d_mpg_dis_mean))
}

honeycrispboot <- c(3,4,7,8)
for (i in honeycrispboot) {
  # Using ordinary bootstrap
  d_mpg_dis_mean <- as.numeric(calibration[i,c(18,19,20,21)]) %>% 
    new_r(type = "continuous") %>% 
    form_estimate(stat = mean, sample_size = calibration[i,3])
  # Spread of this bootstrap distribution describes the precision of estimation:
  # bigger values indicate lower precision
  ratio_df[i,1] <- summ_sd(d_mpg_dis_mean)
  ratio_df[i,2] <- summ_range(d_mpg_dis_mean)
  ratio_df[i,3] <- summ_mean(d_mpg_dis_mean)
  ratio_df[i,4:5] <- summ_quantile(d_mpg_dis_mean,probs = c(.05,.95))
  
  ## [1] 1.04067
  # This discrete distribution has the following d-function
  plot(d_mpg_dis_mean, main = paste("Ordinary bootstrap distribution of HW Ratio: HONEYCRISP row ", i, sep=""))
  #plot(as_p(d_mpg_dis_mean))
  
}


#ratio plotting 


colnames(ratio_df) <- c("sd", "range", "mean", "CI05", "CI95")

par(mar=c(11, 4.5, 2, 1))

plot(ratio_df[,3],  ylim=c(.50, 1), main="GALA and HONEYCRISP Calibrated HW ratio", xlab = "", ylab = "H/W Ratio (%)", axes=FALSE)
axis(side = 1, c(1:9), c(as.character(calibration[,1])), las=2)
axis(side = 2, at = c(.5, .6, .7, .8, .9, 1))
box()

lines(ratio_df[,3], col="black")
points(ratio_df[,4], col="red")
points(ratio_df[,5], col="red")
lines(ratio_df[,4], col="red")
lines(ratio_df[,5], col="red")



plot(height_df[,2],  ylim=c(0,5))
lines(height_df[,2], col="black")
points(width_df[,2], col="red")
lines(width_df[,2], col="red")



#HW plotting


plot(height_df[,1],  ylim=c(0,.6), main="SD GALA and HONEYCRISP Calibrated Height & Width: black = H, red = W", xlab = "", ylab = "mm", axes=FALSE)
axis(side = 1, c(1:9), c(as.character(calibration[,1])), las=2)
axis(side = 2, at = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6))
box()
lines(height_df[,1], col="black")
points(width_df[,1], col="red")
lines(width_df[,1], col="red")


#gala

plot(height_df[c(1,2,5,6,9),3],  ylim=c(50,90), main="GALA Calibrated Height & Width: black = H, red = W", xlab = "Rows", ylab = "mm")
lines(height_df[c(1,2,5,6,9),3], col="black")
points(width_df[c(1,2,5,6,9),3], col="red")
lines(width_df[c(1,2,5,6,9),3], col="red")

plot(height_df[c(1,2,5,6,9),2],  ylim=c(0,5), main="GALA Calibrated Height & Width: black = H, red = W", xlab = "Rows", ylab = "mm")
lines(height_df[c(1,2,5,6,9),2], col="black")
points(width_df[c(1,2,5,6,9),2], col="red")
lines(width_df[c(1,2,5,6,9),2], col="red")

plot(height_df[c(1,2,5,6,9),1],  ylim=c(0,.6), main="GALA Calibrated Height & Width: black = H, red = W", xlab = "Rows", ylab = "mm")
lines(height_df[c(1,2,5,6,9),1], col="black")
points(width_df[c(1,2,5,6,9),1], col="red")
lines(width_df[c(1,2,5,6,9),1], col="red")





galaboot <- c(1,2,5,6,9)
height_df <- matrix(NA, nrow=9, ncol=5)
for (i in galaboot) {
  # Using ordinary bootstrap
  d_mpg_dis_mean <- as.numeric(calibration[i,c(4,6,8,10)]) %>% 
    new_d(type = "continuous") %>% 
    form_estimate(stat = mean, sample_size = calibration[i,3])
  # Spread of this bootstrap distribution describes the precision of estimation:
  # bigger values indicate lower precision
  height_df[i,1] <- summ_sd(d_mpg_dis_mean)
  height_df[i,2] <- summ_range(d_mpg_dis_mean)
  height_df[i,3] <- summ_mean(d_mpg_dis_mean)
  height_df[i,4:5] <- summ_quantile(d_mpg_dis_mean,probs = c(.20,.80))
  
  ## [1] 1.04067
  # This discrete distribution has the following d-function
  #plot(d_mpg_dis_mean, main = "Ordinary bootstrap distribution of heights")
  
}

honeycrispboot <- c(3,4,7,8)
for (i in honeycrispboot) {
  # Using ordinary bootstrap
  d_mpg_dis_mean <- as.numeric(calibration[i,c(4,6,8,10)]) %>% 
    new_d(type = "continuous") %>% 
    form_estimate(stat = mean, sample_size = calibration[i,3])
  # Spread of this bootstrap distribution describes the precision of estimation:
  # bigger values indicate lower precision
  height_df[i,1] <- summ_sd(d_mpg_dis_mean)
  height_df[i,2] <- summ_range(d_mpg_dis_mean)
  height_df[i,3] <- summ_mean(d_mpg_dis_mean)
  height_df[i,4:5] <- summ_quantile(d_mpg_dis_mean,probs = c(.20,.80))
  
  ## [1] 1.04067
  # This discrete distribution has the following d-function
  #plot(d_mpg_dis_mean, main = "Ordinary bootstrap distribution of heights")
  
}


galaboot <- c(1,2,5,6,9)
width_df <- matrix(NA, nrow=9, ncol=5)
for (i in galaboot) {
  # Using ordinary bootstrap
  d_mpg_dis_mean <- as.numeric(calibration[i,c(5,7,9,11)]) %>% 
    new_d(type = "continuous") %>% 
    form_estimate(stat = mean, sample_size = calibration[i,3])
  # Spread of this bootstrap distribution describes the precision of estimation:
  # bigger values indicate lower precision
  width_df[i,1] <- summ_sd(d_mpg_dis_mean)
  width_df[i,2] <- summ_range(d_mpg_dis_mean)
  width_df[i,3] <- summ_mean(d_mpg_dis_mean)
  width_df[i,4:5] <- summ_quantile(d_mpg_dis_mean,probs = c(.20,.80))
  
  ## [1] 1.04067
  # This discrete distribution has the following d-function
  #plot(d_mpg_dis_mean, main = "Ordinary bootstrap distribution of heights")
  
}

honeycrispboot <- c(3,4,7,8)
for (i in honeycrispboot) {
  # Using ordinary bootstrap
  d_mpg_dis_mean <- as.numeric(calibration[i,c(5,7,9,11)]) %>% 
    new_d(type = "continuous") %>% 
    form_estimate(stat = mean, sample_size = calibration[i,3])
  # Spread of this bootstrap distribution describes the precision of estimation:
  # bigger values indicate lower precision
  width_df[i,1] <- summ_sd(d_mpg_dis_mean)
  width_df[i,2] <- summ_range(d_mpg_dis_mean)
  width_df[i,3] <- summ_mean(d_mpg_dis_mean)
  width_df[i,4:5] <- summ_quantile(d_mpg_dis_mean,probs = c(.20,.80))
  
  ## [1] 1.04067
  # This discrete distribution has the following d-function
  #plot(d_mpg_dis_mean, main = "Ordinary bootstrap distribution of heights")
  
}

par(mar=c(11, 4.5, 2, 1))

plot(height_df[,3],  ylim=c(50,90), main="Mean GALA and HONEYCRISP Calibrated Height & Width: black = H, red = W", xlab = "", ylab = "mm", axes=FALSE)
axis(side = 1, c(1:9), c(as.character(calibration[,1])), las=2)


lines(height_df[,3], col="black")
points(width_df[,3], col="red")
lines(width_df[,3], col="red")


plot(height_df[,1],  ylim=c(0,.6), main="SD GALA and HONEYCRISP Calibrated Height & Width: black = H, red = W", xlab = "", ylab = "mm", axes=FALSE)
axis(side = 1, c(1:9), c(as.character(calibration[,1])), las=2)
axis(side = 2, at = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6))
box()
lines(height_df[,1], col="black")
points(width_df[,1], col="red")
lines(width_df[,1], col="red")


#gala

plot(height_df[c(1,2,5,6,9),3],  ylim=c(50,90), main="Mean GALA Calibrated Height & Width: black = H, red = W", xlab = "Rows", ylab = "mm")
lines(height_df[c(1,2,5,6,9),3], col="black")
points(width_df[c(1,2,5,6,9),3], col="red")
lines(width_df[c(1,2,5,6,9),3], col="red")

plot(height_df[c(1,2,5,6,9),2],  ylim=c(0,5), main="Range GALA Calibrated Height & Width: black = H, red = W", xlab = "Rows", ylab = "mm")
lines(height_df[c(1,2,5,6,9),2], col="black")
points(width_df[c(1,2,5,6,9),2], col="red")
lines(width_df[c(1,2,5,6,9),2], col="red")

plot(height_df[c(1,2,5,6,9),1],  ylim=c(0,.6), main="SD GALA Calibrated Height & Width: black = H, red = W", xlab = "Rows", ylab = "mm")
lines(height_df[c(1,2,5,6,9),1], col="black")
points(width_df[c(1,2,5,6,9),1], col="red")
lines(width_df[c(1,2,5,6,9),1], col="red")

#honeycrisp

plot(height_df[c(3,4,7,8),3],  ylim=c(50,90), main="Mean HONEYCRISP Calibrated Height & Width: black = H, red = W", xlab = "Rows", ylab = "mm")
lines(height_df[c(3,4,7,8),3], col="black")
points(width_df[c(3,4,7,8),3], col="red")
lines(width_df[c(3,4,7,8),3], col="red")

plot(height_df[c(3,4,7,8),2],  ylim=c(0,5), main="Range HONEYCRISP Calibrated Height & Width: black = H, red = W", xlab = "Rows", ylab = "mm")
lines(height_df[c(3,4,7,8),2], col="black")
points(width_df[c(3,4,7,8),2], col="red")
lines(width_df[c(3,4,7,8),2], col="red")

plot(height_df[c(3,4,7,8),1],  ylim=c(0,.6), main="SD HONEYCRISP Calibrated Height & Width: black = H, red = W", xlab = "Rows", ylab = "mm")
lines(height_df[c(3,4,7,8),1], col="black")
points(width_df[c(3,4,7,8),1], col="red")
lines(width_df[c(3,4,7,8),1], col="red")








require(ggplot2)


h_g <- ggplot(height_df[c(1,2,5,6,9),], aes(x = as.numeric(1:nrow(height_df[c(1,2,5,6,9),])), y = V3)) +
  geom_point(size = 4) + geom_errorbar(aes(ymax = V4, ymin = V5)) + xlim(as.character(calibration[c(1,2,5,6,9),1])) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("GALA HEIGHT calibrated") +
  xlab("Rows") + ylab("Height")

h_h <- ggplot(height_df[c(3,4,7,8),], aes(x = as.numeric(1:nrow(height_df[c(3,4,7,8),])), y = V3)) +
  geom_point(size = 4) + geom_errorbar(aes(ymax = V4, ymin = V5)) + xlim(as.character(calibration[c(3,4,7,8),1])) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("HONEYCRISP  HEIGHT calibrated") +
  xlab("Rows") + ylab("Height")
  
library("gridExtra")
grid.arrange(h_g, h_h, ncol = 2, nrow = 1, 
             layout_matrix = t(c(2,1)))


w_g <- ggplot(width_df[c(1,2,5,6,9),], aes(x = as.numeric(1:nrow(width_df[c(1,2,5,6,9),])), y = V3)) +
  geom_point(size = 4) + geom_errorbar(aes(ymax = V4, ymin = V5)) + xlim(as.character(calibration[c(1,2,5,6,9),1])) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("GALA WIDTH calibrated") +
  xlab("Rows") + ylab("Width")

w_h <- ggplot(width_df[c(3,4,7,8),], aes(x = as.numeric(1:nrow(width_df[c(3,4,7,8),])), y = V3)) +
  geom_point(size = 4) + geom_errorbar(aes(ymax = V4, ymin = V5)) + xlim(as.character(calibration[c(3,4,7,8),1])) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("HONEYCRISP WIDTH calibrated") +
  xlab("Rows") + ylab("Width")

library("gridExtra")
grid.arrange(w_g, w_h, ncol = 2, nrow = 1, 
             layout_matrix = t(c(2,1)))

colnames(height_df) <- c("SD", "range", "mean", "CI_20", "CI_80")
colnames(width_df) <- c("SD", "range", "mean", "CI_20", "CI_80")



