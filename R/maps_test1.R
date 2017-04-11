#'
#'
#' This function
#' @param p A number
#' @param n A number
#' @param mydata1 A matrix
#' @param mydata2 A matrix
#' @param veid A matrix
#' @keywords maps_test1
#' @export 1
#' @examples 1
#' @export

maps_test1 <- function(p,n,mydata1,mydata2,veid) {


  #  n <- length(mydata2[,1])-1
  p <- autoskaicius1(n,p,mydata1,mydata2)



  x <- paskirstymas(p,n,mydata1,mydata2,veid)
  k <- length(x)
  spalva <- c("#ffcc00","#0033ff","#ff4c00","#00ffcc","#ff00b3","#b3ff00","#ffa100","#ff0000","#f3ff00","#a5ff00","#00ff23","#00ffd9","#00faff","#0080ff","#7b00ff","#ffcc00","#0033ff","#ff4c00","#00ffcc","#ff00b3","#b3ff00","#ff4c00","#ff4c00","#f3ff00","#a5ff00","#00ff23","#00ffd9","#00faff","#0080ff","#7b00ff","#ffcc00","#0033ff","#ff4c00","#00ffcc","#ff00b3","#b3ff00","#ff4c00","#ff4c00","#f3ff00","#a5ff00","#00ff23","#00ffd9","#00faff","#0080ff","#7b00ff","#ffcc00","#0033ff","#ff4c00","#00ffcc","#ff00b3","#b3ff00","#ff4c00","#ff4c00","#f3ff00","#a5ff00","#00ff23","#00ffd9","#00faff","#0080ff","#7b00ff","#ffcc00","#0033ff","#ff4c00","#00ffcc","#ff00b3","#b3ff00","#ff4c00","#ff4c00","#f3ff00","#a5ff00","#00ff23","#00ffd9","#00faff","#0080ff","#7b00ff","#ffcc00","#0033ff","#ff4c00","#00ffcc","#ff00b3","#b3ff00","#ff4c00","#ff4c00","#f3ff00","#a5ff00","#00ff23","#00ffd9","#00faff","#0080ff","#7b00ff","#ffcc00","#0033ff","#ff4c00","#00ffcc","#ff00b3","#b3ff00","#ff4c00","#ff4c00","#f3ff00","#a5ff00")

  autopav <- matrix(rep(0,k),nrow = 1)
  for (j in 1:k) {
    autopav[j] <- x[[j]][[3]]
  }

  autopav1 <- autopav
  autopav <- paste("ta\u0161kai:", autopav)
  autopav1 <- paste("eilė:", autopav1)

  autolegend <- matrix(rep(0,k),nrow = 1)

  for (j in 1:k) {
    autolegend[j] <- paste(x[[j]][[3]], sum(as.numeric(matrix(x[[j]][[1]],ncol=4)[,4])),sep = " | ")
    autolegend[j] <- paste(autolegend[j], mydata1[j,2],sep = "/")
  }



  lng <- as.list(seq(1:k))
  lat <- as.list(seq(1:k))


  for (i in 1:k) {

    lng[[i]] = list(as.numeric(mydata2[as.numeric(x[[i]][[2]]$route),3]))
    lat[[i]] = list(as.numeric(mydata2[as.numeric(x[[i]][[2]]$route),2]))
  }


  c1 <- as.list(seq(1:k))

  for (i in 1:k) {

    a <- as.numeric(x[[i]][[2]]$route)
    a <- a[1:length(a)]
    a1 <- 0:(length(a)-1)
    c1[[i]] = list(paste(a1,mydata2[a,1],sep = "  | "))

  }
  
  
  

  z <-  mapai[[k]](lng,lat,c1,autopav1,spalva,autolegend,autopav)

  return(z)
}
