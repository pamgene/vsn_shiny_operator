library(vsn)
library(reshape2)
library(data.table)

vsn0 = function(df, normalization = TRUE) {
  X    <- acast(df, .ri ~ .ci, value.var = ".y")
  res  <- vsn2(X, calib = ifelse(normalization, "affine", "none"))
  data.table(vsn = list(res))
}

vsnr = function(df, normalization = TRUE){
  X = acast(df, rowSeq ~ colSeq)
  R = acast(df %>% filter(RefFactor == levels(RefFactor)[1]), rowSeq ~ colSeq)
  
  if(dim(X)[1] != dim(R)[1]){
    stop("Number of rows in data and reference do not match")
  }
  
  if(all(rownames(X) == rownames(R))){
    aRef = vsn2(R, calib = ifelse(normalization, "affine", "none"))
    aVsn = vsn2(X, reference = aRef, calib = ifelse(normalization, "affine", "none"))
    result = data.table(vsn = list(aVsn))
  } else {
    stop("IDs of data and reference do not match")
  }
  return(result)
}

vsnh = function(dt) {
  H           <- attr(dt$vsn[[1]], "hx")
  rownames(H) <- 0:(dim(H)[1] - 1)
  result      <- reshape2::melt(H)
  colnames(result) <- c(".ri", ".ci", "Hvsn")
  result
}