#'
#'
#' This function
#' @param n A number
#' @param p A number
#' @param mydata1 A matrix
#' @param mydata2 A matrix
#' @keywords rek_auto_sk
#' @export
#' @examples 1
#' @export

rek_auto_sk <- function(n,p,mydata1,mydata2){
  pran <- autoskaicius1(n,p,mydata1,mydata2)
#  pran <- paste("Skaiciuojamas auto skaicius: ", pran, sep = " ")
  return(pran)
}
