#'
#'
#' This function
#' @param veid A matrix
#' @param z A number
#' @param n A number
#' @keywords veidr
#' @export 1
#' @examples 1
#' @export

veidr <- function(veid,z,n) {

  veid1 <- matrix(paste(veid[,1],veid[,2], sep = ";"),ncol = 1)

  names <- as.numeric(z)
  items <- as.numeric(NROW(names))
  xmat <- matrix(rep(0,length(z)*length(z)),nrow = length(z), ncol = length(z),dimnames=list(names,names))


  for (i in 1:length(z)) {
    j <- 1:length(z)
    xmat[i,j] <- veid[match(paste(names[i],names[j], sep = ";"),veid1[,1]),3]
  }

  xmat[is.na(xmat)] <- 0
  xmat[lower.tri(xmat)] <- t(xmat)[lower.tri(xmat)]

  #######pridedame baze#######

  return(xmat)

}
