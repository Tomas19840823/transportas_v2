#'
#'
#' This function
#' @param veid A matrix
#' @param z A number
#' @param n A number
#' @param mas2 A matrix
#' @keywords route1
#' @export 1
#' @examples 1
#' @export

route1 <- function(veid,z,n,mas2) {

 if (requireNamespace("TSP", quietly = TRUE)) {

  #  require("TSP")
  places <- veidr(veid,z,n)
  #rownames(places) <- mas2
  #names <- as.numeric(z)
  #items <- as.numeric(NROW(names))
  #city.matrix <- matrix(places,nrow=items, ncol=items, dimnames=list(names,names))
  ####################################
  tsp <- TSP(places)
  ####################################

  methods <- c("nn")

  tours <- lapply(methods, FUN = function(m) solve_TSP(tsp, method = "nn",control=list(start=1)))
  best <- tours[1]
  best.route <- names(best[[1]])
  best.distance <- tour_length(tsp,best[[1]])

  mas2 <- rbind(mas2,rownames(places)[-1])
  rez <- mas2[1,][match(best.route,mas2[2,])][-1]
  output <- list(route = best.route, distance.travelled = best.distance, klientai = rez)

 } else {## do something else not involving TSP
   }

  return(output)

}
