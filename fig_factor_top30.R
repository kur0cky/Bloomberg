library(glmnet)
library(tidyverse)
library(lubridate)
library(xts)
library(MASS)
library(foreach)
library(GGally)
library(ellipse)
library(hexbin)

#データの準備----
result.lasso <- readRDS("data/result_lasso.RDS")

riskPremium <- foreach(i = 1:26, .combine = rbind) %do% {
  replicate <- diag(5) %*% ginv(as.matrix(result.lasso[[i]][,-1]))
  riskPremium.tmp <- t(replicate) * result.lasso[[i]][,1]
  apply(riskPremium.tmp, 2, sum)
}

top30.names.list <- list()

for(i in 1:length(result.lasso)) {
  top30.names.list[[i]] <- matrix(ncol=ncol(result.lasso[[1]]), nrow=30)  
  for(j in 1:ncol(result.lasso[[1]])) {
    top30.names.list[[i]][,j] <- result.lasso[[i]][,j] %>% 
      sort(decreasing = T) %>% 
      head(n=30) %>% 
      names() %>% as.vector()
  }
}
rm(i)
rm(j)

top30.test.df <- matrix(nrow=length(result.lasso), ncol=ncol(result.lasso[[1]]))

for(i in 1:length(result.lasso)){
  for(j in 1:ncol((result.lasso[[1]]))) {
    top30.test.df[i,j] <- dfs[[i+3]][,top30.names.list[[i]][,j]] %>% 
      apply(2, function(x) exp(sum(x))) %>% mean()
  }
}
rm(i)
rm(j)
colnames(top30.test.df) <- colnames(result.lasso[[1]])

#ベンチマーク----
benchmark.tmp <- read.csv("data/topix_index.csv")
benchmark.tmp %>% head()

benchmark.tmp2 <- xts(x=diff(log(benchmark.tmp$PX_LAST)), order.by =as.Date(benchmark.tmp$Date[-1]))
benchmark.tmp2 %>% plot()


benchmark <- apply.monthly(benchmark.tmp2, function(x) exp(sum(x)))
benchmark %>% plot()

benchmark.ret <- cumprod(benchmark[-c(1,2,3)])
benchmark.ret %>% plot(gpars=list(xlab="time", ylab="return(TOPIX)"))

date_vec <- seq(as.Date("2015-04-01"), as.Date("2017-06-01"), by = "1 month")


#グラフの作成

top30.test.cum <- apply(top30.test.df, 2, function(x) cumprod(x))

ret.top30.tmp <- ts(top30.test.cum, start=c(2015,4), end=c(2017,5), frequency = 12)
ret.top30.tmp2 <- data.frame(time = time(ret.top30.tmp), as.data.frame(top30.test.cum))
colnames(ret.top30.tmp2) <- c("time", "alpha", "TOPIX", "VIX", "Value", "Size", "JPY_USD")
ret.top30.tmp2$benchmark <- as.numeric(benchmark.ret[-(length(benchmark.ret))])
ret.top30.tidy <- ret.top30.tmp2 %>% 
  gather(type, value, -time)
rm(ret.top30.tmp)
rm(ret.top30.tmp2)
date_vec <- seq(as.Date("2015-04-01"), as.Date("2017-05-01"), by = "1 month")
ret.top30.tidy$time <- date_vec


ggplot(data=ret.top30.tidy, aes(x=time, y=value))+
  geom_line(aes(group=type, colour=type), size=0.8)+
  theme_bw()+
  scale_colour_hue(name = "factor") +
  scale_x_date(date_breaks = "3 months", date_labels = "%y-%m") +
  labs(x = "Date", y = "return")
