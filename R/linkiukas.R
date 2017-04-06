#'
#'
#' This function
#' @param x A list
#' @keywords linkiukas
#' @export 1
#' @examples 1
#' @export

linkiukas <- function(x) {
  rez <- matrix(nrow = 5, ncol = 2)

  for (i in 1:5)
  {
    if (i <= length(x)) {
      rez[i,1] <-  x[[i]][[4]]
      rez[i,2] <- paste("google link: ", x[[i]][[3]]) } else {
        rez[i,1] <-  ""
        rez[i,2] <- "" }
  }

  rez <- list(rez[1,],rez[2,],rez[3,],rez[4,],rez[5,])
  return(rez)
}
