# load packages
library(glmnet)
library(tidyverse)
library(lubridate)
library(xts)
library(MASS)
library(foreach)
library(GGally)

#データの整理と概観----

##資産価格データ----
#NAの入ってないcsvを用意する
asset.tmp <- read.csv("data/assets.csv")
asset.tmp[1:5,1:5]

asset.tmp2 <- data.frame(apply(
  asset.tmp[,-1] ,
  2,
  function(x) diff(log(x))
))
rownames(asset.tmp2) <- asset.tmp$Date[-1]
asset.tmp2[1:5,1:5]

asset <- data.frame(Date = rownames(asset.tmp2) %>% as.Date(),
                    asset.tmp2)
asset[1:5,1:5]
rm(asset.tmp)
rm(asset.tmp2)

##ファクターデータ----
#NAの入っていない、正規化したデータを用意する
factor.tmp <- read.csv("data/factors.csv")
head(factor.tmp)

factor.tmp2 <- data.frame(factor.tmp[,-c(1,2)])
rownames(factor.tmp2) <- factor.tmp[,2]
head(factor.tmp2)
plot(ts(factor.tmp2))

factor <- data.frame(Date = as.Date(rownames(factor.tmp2)),
                     factor.tmp2)
factor[1:5,1:5]
GGally::ggpairs(factor[,-1])

#0.4を弱い相関と言っていいのかわからない。
#アヒル本のggplotを参考にする。
rm(factor.tmp)
rm(factor.tmp2)

##資産とファクターの結合----
d <- inner_join(factor, asset, by="Date")
d[1:5,1:10]
dim(d)

##月ごとのリストに分ける
d$year <- d$Date %>% year()
d$month <- d$Date %>% month()

dfs <- split(d, list(d$month, d$year))
#dataframesの略
length(dfs)

for(i in 1:length(dfs)){
  print(dim(dfs[[i]]))
}
rm(i)

for(i in length(dfs):1) {
  if(dim(dfs[[i]])[1] == 0) dfs[[i]] <- NULL
}
rm(i)

for(i in 1:length(dfs)){
  print(dim(dfs[[i]]))
}
rm(i)
length(dfs)
#29カ月分のデータ

#ファクターモデルの推定----

#result.lasso <- for(i in 1:(length(dfs)-3)){
#  train <- rbind(dfs[[i]], dfs[[i+1]], dfs[[i+2]])
#  result.lasso[[i]] <- foreach(j = 2:ncol(d), .combine = rbind) %do% {
#    cv.glmnet(x = as.matrix(train[,1951:1955]), 
#              y = as.matrix(train[,j]),
#              standardize=TRUE) %>% #訓練の三ヶ月間の中で自動的に正規化してくれる
#      coef(s="lambda.min") %>% 
#      .[,1]
#  }
#}

result.lasso <- readRDS("data/result_lasso.RDS")
length(result.lasso)
dim(result.lasso[[1]])

# NOTE ------------------------------------------------------------------
# asset : 収益率と日付のデータ
asset[1:5,1:5]
# factor : 各Factorの収益率と日付のデータ
# ggpairsで相関係数も出せる
factor[1:5,1:5]
GGally::ggpairs(factor[,-1])
# d : assetとfactorを日付列でmergeしたもの
# 562 days
d[1:5,1:10]
dim(d)
# dfs : dを月ごとに分けたもの
# 29ヶ月分ある（from 2015-01 to 2017-05）
dfs[[1]][1:10,1:7]
length(dfs)
# result.lasso : 各資産に関して、factor modelのbeta値（各月）
# 3ヶ月分を用いて推定してるため、dfsのはじめ3ヶ月分は推定のみに使われる
# そのため26ヶ月分ある（from 2015-04 to 2017-05）
result.lasso[[1]][1:5,]
length(result.lasso)
# ------------------------------------------------------------------------

#バックテスト(top30)----
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
top30.test.df %>%
#  apply(2,cumprod) %>%
#　累積リターンにするならコメントはずす
    ts.plot(col=1:6)
legend("topleft",c("Alpha", "TOPIX", "VIX", "value", "size", "JPY_USD"),
       col=1:6,
       lwd=2, cex=0.7)


#リスク・プレミアム----


riskPremium <- foreach(i = 1:26, .combine = rbind) %do% {
  replicate <- diag(5) %*% ginv(as.matrix(result.lasso[[i]][,-1]))
  
  riskPremium.tmp <- t(replicate) * result.lasso[[i]][,1]
  apply(riskPremium.tmp, 2, sum)
}
rm(i)
rm(replicate)
rm(riskPremium.tmp)

ts.plot(riskPremium, col=c("black", "red", "yellow", "blue", "green"))
legend("topleft",c("TOPIX", "VIX", "value_growth", "size", "JPY_USD"))

ts.plot(riskPremium[,-2], col=c("black", "red", "blue", "green"))
legend("topleft",c("TOPIX", "value_growth", "size", "JPY_USD"),
       col=c("black", "red", "blue", "green"),
       lwd=2, cex=0.7)
