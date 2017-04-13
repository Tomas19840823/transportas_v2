#'
#'
#' This function
#' @param p A number
#' @param n A number
#' @param mydata1 A list
#' @param mydata2 A list
#' @param veid A matrix
#' @keywords maps_start
#' @export 1
#' @examples 1
#' @export


maps_start <- function(p,n,mydata1,mydata2,veid){
  (m <- leaflet()  %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") )
  (z <- m %>% setView(lng = "25.25562", lat = "54.70204", zoom = 10) )
  return(z)
}
