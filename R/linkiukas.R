#'
#'
#' This function
#' @param x A list
#' @keywords linkiukas
#' @export 1
#' @examples 1
#' @export

linkiukas <- function(x) {
  rez <- matrix(nrow = 50, ncol = 2)

  for (i in 1:50)
  {
    if (i <= length(x)) {
      rez[i,1] <-  x[[i]][[4]]
      rez[i,2] <- paste("google link: ", x[[i]][[3]],"| atstumas", x[[i]][[2]]$distance.travelled/1000, "km") } else {
        rez[i,1] <-  ""
        rez[i,2] <- "" }
  }

  rez <- list(rez[1,],rez[2,],rez[3,],rez[4,],rez[5,],rez[6,],rez[7,],rez[8,],rez[9,],rez[10,],rez[11,],rez[12,],rez[13,],rez[14,],rez[15,],rez[16,],rez[17,],rez[18,],rez[19,],rez[20,],rez[21,],rez[22,],rez[23,],rez[24,],rez[25,],rez[26,],rez[27,],rez[28,],rez[29,],rez[30,],rez[31,],rez[32,],rez[33,],rez[34,],rez[35,],rez[36,],rez[37,],rez[38,],rez[39,],rez[40,],rez[41,],rez[42,],rez[43,],rez[44,],rez[45,],rez[46,],rez[47,],rez[48,],rez[49,],rez[50,])
  return(rez)
}
