setwd("C:/Users/SHIO-160412-1/Desktop")

d.tmp <- read.csv("result_bloomberg.csv")
str(d.tmp)
d.tmp$Date <- as.Date(d.tmp$Date)


library(tidyverse)

d <- data.frame(time= d.tmp$Date,
                portfoio = cumprod((d.tmp$Total.Return..P. * 0.01) + 1),
                benchmark = cumprod((d.tmp$Total.Return..B. * 0.01) + 1))

d.long <- d %>% gather(type, return, -time)
str(d.long)

ggplot(d.long, aes(x=time, y=return), fill=type)+
  geom_line(aes(group=type, colour=type), size=0.75)+
  theme_bw()
