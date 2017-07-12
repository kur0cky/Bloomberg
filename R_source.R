library(glmnet)
library(tidyverse)
library(lubridate)
library(xts)
library(MASS)
library(foreach)

#データの整理と概観----

##資産価格データ----
#NAの入ってないcsvを用意する
asset.tmp <- read.csv("data/topix2.csv")
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
pairs(factor[,-1])
cor(factor[,-1])
#0.4を弱い相関と言っていいのかわからない。
#アヒル本のggplotを参考にする。





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

for(i in length(dfs):1) {
  if(dim(dfs[[i]])[1] == 0) dfs[[i]] <- NULL
}

length(dfs)
#29カ月分のデータ

#ファクターモデルの推定

result.lasso <- for(i in 1:(length(dfs)-3)){
  train <- rbind(dfs[[i]], dfs[[i+1]], dfs[[i+2]])
  result.lasso[[i]] <- foreach(j = 2:ncol(d), .combine = rbind) %do% {
    cv.glmnet(x = as.matrix(train[,1951:1955]), 
              y = as.matrix(train[,j]),
              standardize=TRUE) %>% #訓練の三ヶ月間の中で自動的に正規化してくれる
      coef(s="lambda.min") %>% 
      .[,1]
  }
}
