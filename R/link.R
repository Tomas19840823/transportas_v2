#'
#'
#' This function
#' @param marsrutas A matrix
#' @param klientai A matrix
#' @param mydata2 A matrix
#' @keywords link
#' @export 1
#' @examples 1
#' @export


link <- function(marsrutas,klientai,mydata2){

  a <- as.numeric(marsrutas$route)
  klientai1 <- rbind(mydata2[1,],mydata2[a[2:length(a)],],mydata2[1,])

  linkas <- paste("https://www.google.lt/maps/dir")


  for (i in 1:(length(klientai1[,1]))){

    geo <- paste(klientai1[i,2],klientai1[i,3],sep = ",")
    linkas <- paste(linkas,geo,sep = "/")

  }

  remove(geo)
  return(linkas)


}
