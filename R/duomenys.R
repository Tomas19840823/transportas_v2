#'
#'
#' This function
#' @keywords duomenys
#' @export 1
#' @examples 1
#' @export


duomenys <- function() {

##########input#############################################

setwd("C:/Users/tomusi/Desktop/R")
mydata1 = as.matrix(read.csv("apribojimai_v1.csv",sep = ";"))
auto <- as.numeric(mydata1[,2])
mydata2 = as.matrix(read.csv("klientai.csv",sep = ";"))
ksvoriai <- as.numeric(mydata2[,4])
########apribojimai##########################################

mydata2 = as.matrix(read.csv("klientai.csv",sep = ";"))
data <- mydata2[1:51,]
mydata2 <- data

#https://developers.google.com/places/android-api/signup

x <- cbind(permutations(length(data[,1]), 2),matrix(nrow=length(permutations(length(data[,1]), 2)[,1])),matrix(nrow=length(permutations(length(data[,1]), 2)[,1])),matrix(nrow=length(permutations(length(data[,1]), 2)[,1])))

for (i in 1: length(x[,1])) {

  k <- google_distance(origins = list(c(as.numeric(data[as.numeric(x[i,1]),2]),as.numeric(data[as.numeric(x[i,1]),3]))), destinations = list(c(as.numeric(data[as.numeric(x[i,2]),2]),as.numeric(data[as.numeric(x[i,2]),3]))), departure_time = Sys.time()+100, key = "AIzaSyC8e6LPqONuW2KzSJ6PaJMN4_u197ml9eU")
  x[i,3] <- as.numeric(unlist(k)[4])
}

for (i in 1: length(x[,1])) {

x[i,4] <-  mydata2[as.numeric(x[i,1]),1]
x[i,5] <-  mydata2[as.numeric(x[i,2]),1]

}

#laikas <- cbind(permutations(length(data[,1]), 2),matrix(nrow=length(permutations(length(data[,1]), 2)[,1])))

#for (i in 1: length(laikas[,1])) {

#  laikas[i,3] <- as.numeric(unlist(google_distance(origins = list(c(as.numeric(data[laikas[i,1],2]),as.numeric(data[laikas[i,1],3]))), destinations = list(c(as.numeric(data[laikas[i,2],2]),as.numeric(data[laikas[i,2],3]))), departure_time = Sys.time()+100, key = "AIzaSyCX-yfSTW_whc1fiRdPdLBBXepzCOvmwVQ"))[6])/60

#}



x <- cbind(x,c(x[,1]&x[,2]))
veid <- x
x1 <- cbind(paste(x[,1],"v",x[,2]),x)
xkoord <- x1

s <- 0
for (i in 1:length(data[,1])) {
s1 <- rep(mydata2[i,1],length((1+(i-1)*50):(i*50)))
 s <- c(s,s1)
}
s <- s[-1]
rownames(veid) <- s
veid <- cbind(veid,s)

return(veid)

}







