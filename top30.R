library(tidyverse)
library(foreach)
library(MASS)
library(xts)
result.lasso <- readRDS("data/result_lasso.RDS")

riskPremium <- foreach(i = 1:26, .combine = rbind) %do% {
  replicate <- diag(5) %*% ginv(as.matrix(result.lasso[[i]][,-1]))
  
  riskPremium.tmp <- t(replicate) * result.lasso[[i]][,1]
  apply(riskPremium.tmp, 2, sum)
}
rm(i)
rm(replicate)
rm(riskPremium.tmp)

top100.names <- list()
top30.names <- list()
for(i in 1:26){
  top100.names[[i]] <- result.lasso[[i]][,-1] %>% .[,rank(riskPremium[i,])==5] %>% 
    sort(decreasing=T) %>% head(n=100) %>% names()
  
  top30.names[[i]] <- result.lasso[[i]][top100.names[[i]],1] %>% sort(decreasing=T) %>%
    head(n=30) %>% 
    names()
}
top30.names
top30.return <- numeric()
for(i in 1:26){
  top30.return[i] <- dfs[[i+3]][,top30.names[[i]]] %>% 
    apply(2, function(x) exp(sum(x))) %>% mean()
}
top30 <- cumprod(top30.return)
top30 %>% plot()

data <- data.frame(time=time(benchmark[-c(1,2,3,30)]),
                  benchmark=benchmark[-c(1,2,3,30)] %>% cumprod(),
                  portfolio = top30)

data.tidy <- data %>% 
  gather(type, value, -time)
data.tidy
ggplot(data=data.tidy, aes(x=time, y=value))+
  geom_line(aes(group=type, colour=type), size=0.8)+
  theme_bw()+
  scale_colour_hue(name = "factor") +
  scale_y
  scale_x_date(date_breaks = "3 months", date_labels = "%y-%m") +
  labs(x = "Date", y = "return")

#graph2
data2 <- c(benchmark=top30-cumprod(benchmark[-c(1,2,3,27)]))
plot(data2, main="", ylab="return", xlab="Date")
###ggplotで書く？？？
###塗りつぶしありで
###スペースの問題???