autolentele <- function(){
  mat <- matrix(nrow = 8, ncol = 3)
  mat[,1] <- c("ABC123", "DEF456", "GHI789","JKL123","MNO456","PRS789","TUV123","ZZZ456")
  mat[,2] <- c(100, 100, 100, 100, 100, 100, 100, 100)
  mat[,3] <- c(100, 100, 100, 100, 100, 100, 100, 100)
 
  
  colnames(mat) <- c("auto nr", "svoris min", "svoris max")
  return(mat)
}
