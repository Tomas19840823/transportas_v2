#'
#'
#' This function
#' @param n A number
#' @param p A number
#' @param mydata1 A list
#' @param mydata2 A list
#' @param veid A matrix
#' @keywords paskirstymas
#' @export 1
#' @examples 1
#' @export

paskirstymas <- function(p,n,mydata1,mydata2,veid) {


mas <- kraunam(p,n,veid,mydata2,mydata1)$z1
autopav <- colnames(mas)
k <- length(colnames(mas)[!is.na(colnames(mas))])
x <- as.list(seq(1:k))


for (i in 1:k) {


mas1 <- mas[,i][!is.na(mas[,i])]
mas1 <- mas1[2:length(mas1)]


klientai <- mydata2[match(mas1,mydata2),]
auto <- matrix(mydata1[match(autopav[i],mydata1),],nrow = 1)

n1 <- length(mas1)+1


z <-c(1,match(mas1,mydata2))
#veid2 <- veidr(veid,z,n)
mas2 <- mas1
######################################
#klientai
marsrutas <- route1(veid,z,n1,mas2)
#auto[1]
#####################################

x[[i]] <- list(klientai,marsrutas,auto[1,1],link(marsrutas,klientai,mydata2))
##########kazka sugalvoti su spalva
j <- i
#if (i<length(spalva)) spal <- spalva[j] else spal <- spalva[1]
#zemelapis(marsrutas,spalva)
}

return(x)

}






































