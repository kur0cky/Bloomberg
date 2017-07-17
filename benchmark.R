library(glmnet)
library(tidyverse)
library(lubridate)
library(xts)
library(MASS)
library(foreach)
library(GGally)
library(ellipse)
library(hexbin)


benchmark.tmp <- read.csv("data/topix_index.csv")
benchmark.tmp %>% head()

benchmark.tmp2 <- xts(x=diff(log(benchmark.tmp$PX_LAST)), order.by =as.Date(benchmark.tmp$Date[-1]))
benchmark.tmp2 %>% plot()


benchmark <- apply.monthly(benchmark.tmp2, function(x) exp(sum(x)))
benchmark %>% plot()

benchmark.ret <- cumprod(benchmark[-c(1,2,3)])
benchmark.ret %>% plot(gpars=list(xlab="time", ylab="return(TOPIX)"))

date_vec <- seq(as.Date("2015-04-01"), as.Date("2017-06-01"), by = "1 month")

