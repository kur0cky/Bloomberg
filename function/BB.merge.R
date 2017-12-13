
# read Bloomberg data
BB.merge <- function(filename, xts=TRUE){
  if (is.na(pmatch("package:xts", search()))){
    stop("\nYou must load 'xts' package.")
  }
  if (!is.character(filename)){
    stop("\nfilename isn't character.")
  }
  
  x         = read.csv(filename, stringsAsFactors=FALSE)
  name      = colnames(x)
  na.info   = which(is.na(x[1,]))
  param.num = na.info[1]-2
  start     = na.info-(param.num+1)
  start     = c(start, start[length(start)]+na.info[1])
  end       = na.info-1
  end       = c(end, end[length(end)]+na.info[1])
  data.num  = length(start)
  x.onlynum = x[-1,]
  my.as.xts <- function(df) {
    index = which(as.character(df[,1])!="")
    date  = as.character(df[index,1])
    df    = apply(df[index,],1,function(x) as.numeric(x[-1]))
    df    = as.matrix(df)
    rownames(df) = date
    as.xts(df)
  } 
  
  result = lapply(seq(param.num), function(pn){
    cat(sprintf("############ %s/%s parameter ############\n",pn,param.num))
    cat(sprintf("%s/%s times process\n",1,data.num-1))
    kekka = merge(my.as.xts(x.onlynum[ c(start[1],(start[1]+pn)) ]),my.as.xts(x.onlynum[ c(start[2],(start[2]+pn)) ]),join="outer")
    
    for(i in seq(3,data.num)){
      cat(sprintf("%s/%s times process\n",i-1,data.num-1))
      kekka = merge(kekka,my.as.xts(x.onlynum[ c(start[i],(start[i]+pn)) ]))
    }
    gc();gc();
    colnames(kekka) = name[start]
    if(!xts) kekka = data.frame(kekka)
    kekka
  })
  
  result
}
