#'
#'
#' This function
#' @param p A number
#' @param n A number
#' @param veid A matrix
#' @param mydata2 A matrix
#' @param mydata1 A matrix
#' @keywords kraunam
#' @export 1
#' @examples 1
#' @export


kraunam <- function(p,n,veid,mydata2,mydata1) {

  z <- seq(1:(n+1))
  veidr1 <- as.matrix(veidr(veid,z,n))
  colnames(veidr1) <- c(mydata2[,1][1:length(z)])
  rownames(veidr1) <- c(mydata2[,1][1:length(z)])

  klientai <- mydata2[2:(length(veidr1[,1])),]
  rownames(klientai) <- c(mydata2[,1][2:length(z)])
  klientaisort <- as.matrix(data.table(klientai, key="lat"))
  klientaifirst <- as.character(klientaisort[1,1])

  veidr2 <- veidr1[klientaifirst,]
  veidr2 <- veidr2[-1]
  k <- t(matrix(as.numeric(klientai[,-1]),nrow = n))

  veidr2 <- rbind(veidr2,k)
  rownames(veidr2)[2:length(veidr2[,1])] <- colnames(mydata2)[2:length(mydata2[1,])]
  veidr2 <- t(veidr2[,order(as.numeric(veidr2[1,]))])
  veidr1 <- veidr1[-1,]
  veidr1 <- veidr1[,-1]

  klientai <- klientai[,-1]


  z1 <- matrix(ncol = p, nrow = (n+1))
  z <- 0
  z2 <- 0
  i <- 0
  masin <- matrix(ncol = p)

  while(z2 == 0){

    i <- i + 1
    if (i > p) (a <- "truksta automobiliu")
    if (i > p) break
    masina <- mydata1[i,]
    klientaisort <- veidr2
    z <- autoskaicius2(masina,klientaisort)
    z1[1,i] <- z
    z1[2:(z+1),i] <- rownames(klientaisort)[1:z]
    masin[1,i] <- masina[1]


    #####################################################################################

    a <- match(rownames(klientaisort)[1:z],rownames(veidr1))

    pavlast <- rownames(klientaisort)[seq(1:max(a))[-a]]
    klientaisort <- klientaisort[-(1:z),]

    if (is.null(klientaisort)) break
    if ((nrow(klientaisort) == 0)&&(class(klientaisort) == "matrix")) break



    if (class(klientaisort) == "numeric") {
      veidr2 <- matrix(klientaisort,nrow = 1)
      rownames(veidr2) <- pavlast   }

    if (class(klientaisort) != "numeric") {


    veidr1 <- as.matrix(veidr1[-a,])
    veidr1 <- as.matrix(veidr1[,-a])


    #nauja latitude
    m <- which(klientaisort[,2] == min(klientaisort[,2]), arr.ind = TRUE)[1]
    m1 <- rownames(klientaisort)[m]


    veidr2 <- veidr1[m1,]

    klientai <- klientai[-a,]


    veidr2 <- matrix(as.numeric(cbind(veidr2,klientai)),nrow = length(veidr2))
    rownames(veidr2) <- rownames(klientai)
    colnames(veidr2) <- colnames(klientaisort)

    b <- which(rownames(veidr2) == m1, arr.ind = TRUE)
    mat <- matrix(veidr2[b,],nrow = 1,dimnames = list(m1))
    names <- rownames(veidr2)[-b]
    #veidr2 <- veidr2[-b,]   ##########
    veidr2 <- matrix(veidr2[-b,],nrow = length(veidr2[-b,1]), dimnames = list(names))


    if (length(klientaisort[,1])>2) {
      veidr2 <- veidr2[order(veidr2[,1]),] } else {veidr2 <- veidr2}
    veidr2 <- rbind(mat,veidr2)


    ######################################################################################


    if (nrow(klientaisort) == 0) z2 <- 1
    }


  }



  colnames(z1) <- masin[1,]
  rezultatas <- list("z1" = z1, "msg" = a)
  return(rezultatas)

}
