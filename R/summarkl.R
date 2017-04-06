#'
#'
#' This function
#' @param data A matrix
#' @keywords summarkl
#' @export 1
#' @examples 1
#' @export

summarkl <- function(data) {

  data <- as.matrix(data)
  data <- length(data[,1])-1
  data <- paste("Klientu skaicius: ", data, sep = " ")
  return(data)

}
