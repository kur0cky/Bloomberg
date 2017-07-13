library(GGally)
library(ellipse)
library(hexbin)
library(ggplot2)
library(tidyverse)


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
riskPremium_tidy <- riskPremium.tmp2 %>% 
  gather(type, value, -time)
rm(riskPremium.tmp)
rm(riskPremium.tmp2)
#グラフの作成----

riskPremium_tidy <- riskPremium.tmp2 %>% 
  gather(type, value, -time)

ggplot(riskPremium_tidy, aes(x=time, y=value))+
  geom_line(aes(group=type, colour=type))+
  theme_bw()
