#'
#'
#' This function
#' @param n A number
#' @param p A number
#' @param mydata1 A list
#' @param mydata2 A list
#' @keywords autoskaicius1
#' @export 1
#' @examples 1
#' @export

autoskaicius1 <- function(n,p,mydata1,mydata2) {

  n1 <- sum(as.numeric(mydata2[2:(n+1),4]))
  p1 <- matrix(ncol = 2, nrow = p)

  for (i in 1:p) {
    p1[i,1] <- sum(as.numeric(mydata1[1:i,2]))
    p1[i,2] <- sum(as.numeric(mydata1[1:i,3]))
    if ((p1[i,1]>=n1)&&(p1[i,2]<=n1)) {z <- i}
  }

  z <- min(which(n1/p1[,1] < 1))
  return(z)
}
