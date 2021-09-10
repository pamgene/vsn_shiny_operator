library(vsn)
library(reshape2)
library(data.table)

vsn0 = function(df, normalization = TRUE) {
  X    <- acast(df, .ri ~ .ci, value.var = ".y")
  res  <- vsn2(X, calib = ifelse(normalization, "affine", "none"))
  data.table(vsn = list(res))
}

vsnr = function(df, normalization = TRUE) {
  result <- data.table()
  X      <- acast(df, .ri ~ .ci, value.var = ".y")
  df_ref <- df %>% filter(RefFactor == levels(RefFactor)[1])
  
  if (nrow(df_ref) > 0) {
    R <- acast(df_ref, .ri ~ .ci, value.var = ".y")
    
    if (dim(X)[1] != dim(R)[1]) {
      stop("Number of rows in data and reference do not match")
    }
    
    if (ncol(R) < 2) {
      stop("The filtered data needs to span at least two columns.")
    }
    
    if (all(rownames(X) == rownames(R))) {
      aRef   <- vsn2(R, calib = ifelse(normalization, "affine", "none"))
      aVsn   <- vsn2(X, reference = aRef, calib = ifelse(normalization, "affine", "none"))
      result <- data.table(vsn = list(aVsn))
    } else {
      stop("IDs of data and reference do not match")
    }
  }
  result
}

vsnh = function(dt) {
  H           <- attr(dt$vsn[[1]], "hx")
  rownames(H) <- 0:(dim(H)[1] - 1)
  result      <- reshape2::melt(H)
  colnames(result) <- c(".ri", ".ci", "Hvsn")
  result
}