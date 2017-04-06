#'
#'
#' This function
#' @param lon A number
#' @param lat A number
#' @keywords maps_koord
#' @export 1
#' @examples 1
#' @export


maps_koord <- function(lon, lat){
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = lon, lat = lat, zoom = 10) %>%
      addMarkers(m, lng = lon, lat = lat)  )

  return(z)
}
