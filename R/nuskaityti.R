#'
#'
#' This function
#' @param mydata2 A matrix
#' @keywords nuskaityti
#' @export 1
#' @examples 1
#' @export

nuskaityti <- function(mydata2) {


  data <- mydata2


  #https://developers.google.com/places/android-api/signup
  x <- cbind(permutations(length(data[,1]), 2),matrix(nrow=length(permutations(length(data[,1]), 2)[,1])),matrix(nrow=length(permutations(length(data[,1]), 2)[,1])),matrix(nrow=length(permutations(length(data[,1]), 2)[,1])))

  for (i in 1 : length(x[,1])) {

    k <- google_distance(origins = list(c(as.numeric(data[as.numeric(x[i,1]),2]),as.numeric(data[as.numeric(x[i,1]),3]))), destinations = list(c(as.numeric(data[as.numeric(x[i,2]),2]),as.numeric(data[as.numeric(x[i,2]),3]))), departure_time = Sys.time()+200, key = "AIzaSyBwavZyPq5VKws4avjqK6lwfVH4csd8TUc")
    x[i,3] <- as.numeric(unlist(k)[4])
  }

  for (i in 1: length(x[,1])) {

    x[i,4] <-  mydata2[as.numeric(x[i,1]),1]
    x[i,5] <-  mydata2[as.numeric(x[i,2]),1]
  }

#  x <- cbind(x,c(as.numeric(x[,1]),as.numeric(x[,2])))
  veid <- x
  x1 <- cbind(paste(x[,1],"v",x[,2]),x)
  xkoord <- x1

  s <- 0
  for (i in 1:length(data[,1])) {
    s1 <- rep(mydata2[i,1],length((1+(i-1)*(length(data[,1])-1)):(i*(length(data[,1])-1))))
    s <- c(s,s1)
  }
  s <- s[-1]
  rownames(veid) <- s
  veid <- cbind(veid,s)

  return(veid)

}
