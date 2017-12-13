#datacleaning
datacleaning <- function(data_xts, dterm, col_names){
  d <- na.locf(data_xts)[dterm]
  d <- as.data.frame(d)
  Date <- as.Date(rownames(d))
  colnames(d) <- col_names
  d <- cbind(Date, d)
  print(dim(d))
  d
}