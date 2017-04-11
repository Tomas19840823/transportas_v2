klientulentele <- function(){
  mat <- matrix(nrow = 8, ncol = 4)
  mat[,1] <- c("Baze", "Klientas1", "Klientas2","Klientas3","Klientas4","Klientas5","Klientas6","Klientas7")
  mat[,2] <- c(54.6872, 54.7017, 54.6500, 54.6842, 54.6814, 54.8061, 54.7587, 54.4390)
  mat[,3] <- c(25.2797, 25.2547, 25.2200, 25.2779, 25.2851, 25.2401, 25.3899, 25.3132)
  mat[,4] <- c(0, 52, 32, 96, 45, 28, 10, 100)
  
  colnames(mat) <- c("Kliento pav", "lat", "lon", "svoris")
  return(mat)
  }
