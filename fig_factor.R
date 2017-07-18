
library(GGally)

library(ellipse)

library(hexbin)

library(ggplot2)

library(tidyverse)





#データの用意----

factor.tmp <- read.csv("data/factors.csv")

head(factor.tmp)



factor.tmp2 <- data.frame(factor.tmp[,-c(1,2)])

rownames(factor.tmp2) <- factor.tmp[,2]

head(factor.tmp2)

plot(ts(factor.tmp2))



factor <- data.frame(Date = as.Date(rownames(factor.tmp2)),
                     
                     factor.tmp2)

factor[1:5,1:5]



rm(factor.tmp)

rm(factor.tmp2)





#グラフ----

factor_tidy <- factor %>% 
  
  gather(type, value, -Date)



factor.tmp <- factor[,-1]

ggplot(factor_tidy, aes(x=Date, y=value))+
  
  geom_line()+
  
  facet_grid(type~. ,scales="free_y")+
<<<<<<< HEAD
  theme_bw()+
=======
  
  theme_bw()+
  
>>>>>>> master
  scale_x_date(date_breaks = "4 months", date_labels = "%Y-%m")
