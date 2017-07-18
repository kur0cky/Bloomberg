library(tidyverse)



premium.top30 <- list()
for(i in 1:length(result.lasso)) {
assetpremium.tmp <- result.lasso[[i]][,-1] %>% apply(1, function(x) sum(x*riskPremium[i,])) 
assetpremium.tmp2 <- sort(assetpremium.tmp, decreasing=T) %>% head(n=30)
premium.top30[[i]] <- assetpremium.tmp2 %>% names()
rm(assetpremium.tmp)
rm(assetpremium.tmp2)
}

premium.top30



for(i in 1:length(result.lasso)){
  ret.premium.top30[i] <-  dfs[[i+3]][,premium.top30[[i]]] %>% 
    apply(2, function(x) exp(sum(x))) %>% mean()
}
rm(i)
ret.premium.top30 %>% ts.plot()


ret.premium.top30 %>%
  cumprod()

