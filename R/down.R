down <- function(x)  {
  
  m <- matrix(nrow = 1, ncol = 6)
  
  for (i in 1:length(x)) {
    
    matr <- matrix(x[[i]][[1]], ncol = 4)
    eile <- match(unlist(x[[i]][[2]][3]),as.matrix(x[[i]][[1]])[,1])
    dist <- rep(as.numeric(unlist(x[[i]][[2]][2])),length(eile))
    auto <- rep(x[[i]][[3]],length(eile))
    m1 <- cbind(matrix(matr[eile,],ncol = 4),as.matrix(auto),as.matrix(dist))
    m <- rbind(m,m1)
  }
  m <- m[-1,]
  
  pav <- c("Klientas","lat", "lon", "svoris", "auto nr", "marsruto km")
  colnames(m) <- pav
  
  return(m)
}
