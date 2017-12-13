BB.merge2 <- function(filename) {
  
  if(is.na(pmatch(x = "package:data.table", search()))) {
    stop("\nYou must load 'data.table' package.")
  }
  if(is.na(pmatch(x = "package:pforeach", search()))) {
    stop("\nYou must load 'pforeach' package.")
  }
  if(is.na(pmatch(x = "package:dplyr", search()))) {
    stop("\nYou must load 'dplyr' package.")
  }
  if (!is.character(filename)){
    stop("\nfilename isn't character.")
  }
  
  data <- fread(filename, stringsAsFactors=FALSE) %>% as.data.frame()
  data[data == ""] <- NA
  info <- seq(1, ncol(data), by = 3) %>>% 
    ( ~ colnames(data)[.] -> asset_name )
  for(i in info) {
    if(is.na(data[nrow(data), i]) == FALSE){
      date.info <- data[-c(1), i] %>% as.Date %>% data.frame(Date = .)
    }
  }
  each.d <- pforeach::pforeach(i = info, .inorder = TRUE, 
                               .combine = cbind) ({
    d <- data.frame(Date = data[-1,i] %>% as.Date,
                    a = data[-1,i+1] %>% as.numeric)
    colnames(d) <- c("Date", colnames(data)[i])
    if(nrow(d) == length(date.info)){
      d[,2]
    } else {
      dplyr::left_join(date.info, d, by = "Date")[,2]
    }
  })
  res <- data.table(Date = date.info, each.d)
  names(res) <- c("Date", asset_name)
  return(res)
}
