library(GGally)
library(ellipse)
library(hexbin)
library(ggplot2)
library(dplyr)


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



#グラフの作成----
factor.tmp <- factor[,-1]
N_col <- ncol(factor.tmp)
ggp <- ggpairs(factor.tmp, upper='blank', diag='blank', lower='blank')

label_list <- list(TOPIX='ToPIX', VIX='VIX', Value='Value', Size='Size', JPY_USD="JPY_USD")
for(i in 1:N_col) {
  x <- factor.tmp[,i]
  bw <- (max(x)-min(x))/10
  p <- ggplot(data.frame(x), aes(x))
  p <- p + theme_bw(base_size=14)
  p <- p + theme(axis.text.x=element_text(angle=60, vjust=1, hjust=1))
  p <- p + geom_histogram(binwidth=bw, fill='white', color='grey5')
  p <- p + geom_line(eval(bquote(aes(y=..count..*.(bw)))), stat='density')
  p <- p + geom_label(data=data.frame(x=-Inf, y=Inf, label=label_list[[colnames(factor.tmp)[i]]]), aes(x=x, y=y, label=label), hjust=0, vjust=1)
  ggp <- putPlot(ggp, p, i, i)
}

zcolat <- seq(-1, 1, length=81)
zcolre <- c(zcolat[1:40]+1, rev(zcolat[41:81]))

for(i in 1:(N_col-1)) {
  for(j in (i+1):N_col) {
    x <- as.numeric(factor.tmp[,i])
    y <- as.numeric(factor.tmp[,j])
    r <- cor(x, y)
    zcol <- lattice::level.colors(r, at=zcolat, col.regions=grey(zcolre))
    textcol <- ifelse(abs(r) < 0.4, 'grey20', 'white')
    ell <- ellipse::ellipse(r, level=0.95, type='l', npoints=50, scale=c(.2, .2), centre=c(.5, .5))
    p <- ggplot(data.frame(ell), aes(x=x, y=y))
    p <- p + theme_bw() + theme(
      plot.background=element_blank(),
      panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
      panel.border=element_blank(), axis.ticks=element_blank()
    )
    p <- p + geom_polygon(fill=zcol, color=zcol)
    p <- p + geom_text(data=NULL, x=.5, y=.5, label=100*round(r, 2), size=6, col=textcol)
    ggp <- putPlot(ggp, p, i, j)
  }
}

for(j in 1:(N_col-1)) {
  for(i in (j+1):N_col) {
    x <- factor.tmp[,j]
    y <- factor.tmp[,i]
    p <- ggplot(data.frame(x, y), aes(x=x, y=y))
    p <- p + theme_bw(base_size=14)
    p <- p + theme(axis.text.x=element_text(angle=60, vjust=1, hjust=1))
    p <- p + geom_hex()
    p <- p + scale_fill_gradientn(colours=gray.colors(7, start=0.1, end=0.9))
    ggp <- putPlot(ggp, p, i, j)
  }
}


print(ggp, left=0.6, bottom=0.6)
dev.off()
