#'
#'
#' This function
#' @param data A matrix
#' @keywords summarauto
#' @export 1
#' @examples 1
#' @export

summarauto <- function(data) {

  data <- as.matrix(data)
  data <- length(data[,1])
  data <- paste("Auto skaicius: ", data, sep = " ")
  return(data)

}
