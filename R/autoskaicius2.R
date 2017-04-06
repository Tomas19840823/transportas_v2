#'
#'
#' This function
#' @param masina A matrix
#' @param klientaisort A matrix
#' @keywords autoskaicius2
#' @export 1
#' @examples 1
#' @export


autoskaicius2 <- function(masina,klientaisort) {

  #  n1 <- sum(as.numeric(mydata2[2:(n+1),4]))
  # p1 <- matrix(ncol = 2, nrow = p)
  n1 <- 0
  z <- 0
  i <- 1

  while (z == 0) {
    n1 <- sum(as.numeric(klientaisort[(1:i),4]))
    a <- ((n1/as.numeric(masina[2])) > 1)
    b <- (i == length(klientaisort[,1]))
    if(a) {z <- i - 1} else {
      if(b) z <- i}
    i <- i + 1
  }

  return(z)
}
