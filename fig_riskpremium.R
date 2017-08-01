library(GGally)
library(ellipse)
library(hexbin)
library(ggplot2)
library(tidyverse)
library(foreach)
library(MASS)
<<<<<<< HEAD

=======
>>>>>>> master

#データの準備----

result.lasso <- readRDS("data/result_lasso.RDS")

riskPremium <- foreach(i = 1:26, .combine = rbind) %do% {
  replicate <- diag(5) %*% ginv(as.matrix(result.lasso[[i]][,-1]))
  riskPremium.tmp <- t(replicate) * result.lasso[[i]][,1]
  apply(riskPremium.tmp, 2, sum)
}
rm(i)
rm(replicate)
rm(riskPremium.tmp)


riskPremium.tmp <- ts(riskPremium, start=c(2015,4), end=c(2017,5), frequency = 12)
riskPremium.tmp2 <- data.frame(time = time(riskPremium.tmp), as.data.frame(riskPremium))
colnames(riskPremium.tmp2) <- c("time", "TOPIX", "VIX", "Value", "Size", "JPY_USD")
riskPremium_tidy <- riskPremium.tmp2[,-3] %>% 
  gather(type, value, -time)
rm(riskPremium.tmp)
rm(riskPremium.tmp2)

#グラフの作成----




date_vec <- seq(as.Date("2015-03-01"), as.Date("2017-04-01"), by = "1 month")

riskPremium_tidy$time <- date_vec



date_vec <- seq(as.Date("2015-03-01"), as.Date("2017-04-01"), by = "1 month")
riskPremium_tidy$time <- date_vec

ggplot(riskPremium_tidy, aes(x=time, y=value))+
#<<<<<<< HEAD
  geom_line(aes(group=type, colour=type), size=0.7)+
  theme_bw() +
  scale_colour_hue(name = "factor") +
  scale_x_date(date_breaks = "3 months", date_labels = "%y-%m") +
  labs(x = "Date", y = "risk premium")
# 上の図は15-03から17-04まで
# "15-03"は15-01から15-03までの3ヶ月分のデータを用いて
=======
  
  geom_line(aes(group=type, colour=type), size=1, lineend="round")+
  
  theme_bw() +
  
  scale_colour_hue(name = "factor") +
  
  scale_x_date(date_breaks = "3 months", date_labels = "%y-%m") +
  
  labs(x = "Date", y = "risk premium")

# 上の図は15-03から17-04まで

# "15-03"は15-01から15-03までの3ヶ月分のデータを用いて

>>>>>>> master
# 算出したrisk premiumであることに注意