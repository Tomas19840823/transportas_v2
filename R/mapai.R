#'
#'
#' This function
#' @keywords mapai
#' @export 1
#' @examples 1
#' @export


mapai <- list(
m1 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      
addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:1], labels = as.character(autolegend))%>%
addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
return(z)},

m2 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      
addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%

addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[1], group = autopav[2])%>%

addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:2], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},


m3 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%


      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:3], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},


m4 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:4], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m5 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:5], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m6 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      
      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      
      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:6], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m7 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:7], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m8 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:8], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m9 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:9], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m10 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:10], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m11 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:11], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m12 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:12], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m13 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:13], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m14 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:14], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m15 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:15], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m16 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:16], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m17 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:17], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m18 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:18], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m19 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:19], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m20 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:20], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m21 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:21], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m22 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:22], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m23 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:23], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m24 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:24], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m25 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:25], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m26 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:26], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m27 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:27], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m28 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:28], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m29 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:29], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m30 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:30], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m31 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:31], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m32 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:32], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m33 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%
      addPopups(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]), popup = as.character(unlist(c1[[33]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[33])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%
      addCircleMarkers(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]),radius = 10, color = spalva[33], group = autopav[33])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:33], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m34 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%
      addPopups(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]), popup = as.character(unlist(c1[[33]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[33])%>%
      addPopups(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]), popup = as.character(unlist(c1[[34]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[34])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%
      addCircleMarkers(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]),radius = 10, color = spalva[33], group = autopav[33])%>%
      addCircleMarkers(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]),radius = 10, color = spalva[34], group = autopav[34])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:34], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m35 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%
      addPopups(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]), popup = as.character(unlist(c1[[33]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[33])%>%
      addPopups(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]), popup = as.character(unlist(c1[[34]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[34])%>%
      addPopups(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]), popup = as.character(unlist(c1[[35]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[35])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%
      addCircleMarkers(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]),radius = 10, color = spalva[33], group = autopav[33])%>%
      addCircleMarkers(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]),radius = 10, color = spalva[34], group = autopav[34])%>%
      addCircleMarkers(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]),radius = 10, color = spalva[35], group = autopav[35])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:35], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m36 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%
      addPopups(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]), popup = as.character(unlist(c1[[33]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[33])%>%
      addPopups(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]), popup = as.character(unlist(c1[[34]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[34])%>%
      addPopups(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]), popup = as.character(unlist(c1[[35]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[35])%>%
      addPopups(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]), popup = as.character(unlist(c1[[36]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[36])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%
      addCircleMarkers(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]),radius = 10, color = spalva[33], group = autopav[33])%>%
      addCircleMarkers(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]),radius = 10, color = spalva[34], group = autopav[34])%>%
      addCircleMarkers(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]),radius = 10, color = spalva[35], group = autopav[35])%>%
      addCircleMarkers(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]),radius = 10, color = spalva[36], group = autopav[36])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:36], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m37 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%
      addPopups(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]), popup = as.character(unlist(c1[[33]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[33])%>%
      addPopups(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]), popup = as.character(unlist(c1[[34]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[34])%>%
      addPopups(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]), popup = as.character(unlist(c1[[35]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[35])%>%
      addPopups(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]), popup = as.character(unlist(c1[[36]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[36])%>%
      addPopups(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]), popup = as.character(unlist(c1[[37]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[37])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%
      addCircleMarkers(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]),radius = 10, color = spalva[33], group = autopav[33])%>%
      addCircleMarkers(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]),radius = 10, color = spalva[34], group = autopav[34])%>%
      addCircleMarkers(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]),radius = 10, color = spalva[35], group = autopav[35])%>%
      addCircleMarkers(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]),radius = 10, color = spalva[36], group = autopav[36])%>%
      addCircleMarkers(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]),radius = 10, color = spalva[37], group = autopav[37])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:37], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m38 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%
      addPopups(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]), popup = as.character(unlist(c1[[33]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[33])%>%
      addPopups(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]), popup = as.character(unlist(c1[[34]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[34])%>%
      addPopups(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]), popup = as.character(unlist(c1[[35]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[35])%>%
      addPopups(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]), popup = as.character(unlist(c1[[36]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[36])%>%
      addPopups(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]), popup = as.character(unlist(c1[[37]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[37])%>%
      addPopups(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]), popup = as.character(unlist(c1[[38]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[38])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%
      addCircleMarkers(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]),radius = 10, color = spalva[33], group = autopav[33])%>%
      addCircleMarkers(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]),radius = 10, color = spalva[34], group = autopav[34])%>%
      addCircleMarkers(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]),radius = 10, color = spalva[35], group = autopav[35])%>%
      addCircleMarkers(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]),radius = 10, color = spalva[36], group = autopav[36])%>%
      addCircleMarkers(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]),radius = 10, color = spalva[37], group = autopav[37])%>%
      addCircleMarkers(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]),radius = 10, color = spalva[38], group = autopav[38])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:38], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m39 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%
      addPopups(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]), popup = as.character(unlist(c1[[33]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[33])%>%
      addPopups(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]), popup = as.character(unlist(c1[[34]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[34])%>%
      addPopups(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]), popup = as.character(unlist(c1[[35]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[35])%>%
      addPopups(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]), popup = as.character(unlist(c1[[36]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[36])%>%
      addPopups(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]), popup = as.character(unlist(c1[[37]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[37])%>%
      addPopups(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]), popup = as.character(unlist(c1[[38]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[38])%>%
      addPopups(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]), popup = as.character(unlist(c1[[39]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[39])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%
      addCircleMarkers(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]),radius = 10, color = spalva[33], group = autopav[33])%>%
      addCircleMarkers(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]),radius = 10, color = spalva[34], group = autopav[34])%>%
      addCircleMarkers(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]),radius = 10, color = spalva[35], group = autopav[35])%>%
      addCircleMarkers(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]),radius = 10, color = spalva[36], group = autopav[36])%>%
      addCircleMarkers(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]),radius = 10, color = spalva[37], group = autopav[37])%>%
      addCircleMarkers(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]),radius = 10, color = spalva[38], group = autopav[38])%>%
      addCircleMarkers(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]),radius = 10, color = spalva[39], group = autopav[39])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:39], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m40 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%
      addPopups(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]), popup = as.character(unlist(c1[[33]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[33])%>%
      addPopups(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]), popup = as.character(unlist(c1[[34]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[34])%>%
      addPopups(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]), popup = as.character(unlist(c1[[35]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[35])%>%
      addPopups(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]), popup = as.character(unlist(c1[[36]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[36])%>%
      addPopups(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]), popup = as.character(unlist(c1[[37]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[37])%>%
      addPopups(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]), popup = as.character(unlist(c1[[38]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[38])%>%
      addPopups(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]), popup = as.character(unlist(c1[[39]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[39])%>%
      addPopups(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]), popup = as.character(unlist(c1[[40]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[40])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%
      addCircleMarkers(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]),radius = 10, color = spalva[33], group = autopav[33])%>%
      addCircleMarkers(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]),radius = 10, color = spalva[34], group = autopav[34])%>%
      addCircleMarkers(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]),radius = 10, color = spalva[35], group = autopav[35])%>%
      addCircleMarkers(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]),radius = 10, color = spalva[36], group = autopav[36])%>%
      addCircleMarkers(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]),radius = 10, color = spalva[37], group = autopav[37])%>%
      addCircleMarkers(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]),radius = 10, color = spalva[38], group = autopav[38])%>%
      addCircleMarkers(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]),radius = 10, color = spalva[39], group = autopav[39])%>%
      addCircleMarkers(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]),radius = 10, color = spalva[40], group = autopav[40])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:40], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m41 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%
      addPopups(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]), popup = as.character(unlist(c1[[33]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[33])%>%
      addPopups(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]), popup = as.character(unlist(c1[[34]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[34])%>%
      addPopups(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]), popup = as.character(unlist(c1[[35]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[35])%>%
      addPopups(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]), popup = as.character(unlist(c1[[36]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[36])%>%
      addPopups(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]), popup = as.character(unlist(c1[[37]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[37])%>%
      addPopups(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]), popup = as.character(unlist(c1[[38]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[38])%>%
      addPopups(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]), popup = as.character(unlist(c1[[39]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[39])%>%
      addPopups(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]), popup = as.character(unlist(c1[[40]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[40])%>%
      addPopups(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]), popup = as.character(unlist(c1[[41]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[41])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%
      addCircleMarkers(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]),radius = 10, color = spalva[33], group = autopav[33])%>%
      addCircleMarkers(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]),radius = 10, color = spalva[34], group = autopav[34])%>%
      addCircleMarkers(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]),radius = 10, color = spalva[35], group = autopav[35])%>%
      addCircleMarkers(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]),radius = 10, color = spalva[36], group = autopav[36])%>%
      addCircleMarkers(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]),radius = 10, color = spalva[37], group = autopav[37])%>%
      addCircleMarkers(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]),radius = 10, color = spalva[38], group = autopav[38])%>%
      addCircleMarkers(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]),radius = 10, color = spalva[39], group = autopav[39])%>%
      addCircleMarkers(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]),radius = 10, color = spalva[40], group = autopav[40])%>%
      addCircleMarkers(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]),radius = 10, color = spalva[41], group = autopav[41])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:41], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m42 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%
      addPopups(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]), popup = as.character(unlist(c1[[33]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[33])%>%
      addPopups(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]), popup = as.character(unlist(c1[[34]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[34])%>%
      addPopups(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]), popup = as.character(unlist(c1[[35]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[35])%>%
      addPopups(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]), popup = as.character(unlist(c1[[36]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[36])%>%
      addPopups(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]), popup = as.character(unlist(c1[[37]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[37])%>%
      addPopups(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]), popup = as.character(unlist(c1[[38]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[38])%>%
      addPopups(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]), popup = as.character(unlist(c1[[39]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[39])%>%
      addPopups(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]), popup = as.character(unlist(c1[[40]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[40])%>%
      addPopups(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]), popup = as.character(unlist(c1[[41]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[41])%>%
      addPopups(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]), popup = as.character(unlist(c1[[42]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[42])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%
      addCircleMarkers(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]),radius = 10, color = spalva[33], group = autopav[33])%>%
      addCircleMarkers(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]),radius = 10, color = spalva[34], group = autopav[34])%>%
      addCircleMarkers(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]),radius = 10, color = spalva[35], group = autopav[35])%>%
      addCircleMarkers(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]),radius = 10, color = spalva[36], group = autopav[36])%>%
      addCircleMarkers(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]),radius = 10, color = spalva[37], group = autopav[37])%>%
      addCircleMarkers(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]),radius = 10, color = spalva[38], group = autopav[38])%>%
      addCircleMarkers(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]),radius = 10, color = spalva[39], group = autopav[39])%>%
      addCircleMarkers(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]),radius = 10, color = spalva[40], group = autopav[40])%>%
      addCircleMarkers(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]),radius = 10, color = spalva[41], group = autopav[41])%>%
      addCircleMarkers(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]),radius = 10, color = spalva[42], group = autopav[42])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:42], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m43 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%
      addPopups(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]), popup = as.character(unlist(c1[[33]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[33])%>%
      addPopups(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]), popup = as.character(unlist(c1[[34]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[34])%>%
      addPopups(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]), popup = as.character(unlist(c1[[35]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[35])%>%
      addPopups(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]), popup = as.character(unlist(c1[[36]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[36])%>%
      addPopups(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]), popup = as.character(unlist(c1[[37]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[37])%>%
      addPopups(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]), popup = as.character(unlist(c1[[38]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[38])%>%
      addPopups(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]), popup = as.character(unlist(c1[[39]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[39])%>%
      addPopups(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]), popup = as.character(unlist(c1[[40]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[40])%>%
      addPopups(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]), popup = as.character(unlist(c1[[41]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[41])%>%
      addPopups(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]), popup = as.character(unlist(c1[[42]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[42])%>%
      addPopups(m, lat = unlist(lat[[43]][1]), lng = unlist(lng[[43]][1]), popup = as.character(unlist(c1[[43]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[43])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%
      addCircleMarkers(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]),radius = 10, color = spalva[33], group = autopav[33])%>%
      addCircleMarkers(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]),radius = 10, color = spalva[34], group = autopav[34])%>%
      addCircleMarkers(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]),radius = 10, color = spalva[35], group = autopav[35])%>%
      addCircleMarkers(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]),radius = 10, color = spalva[36], group = autopav[36])%>%
      addCircleMarkers(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]),radius = 10, color = spalva[37], group = autopav[37])%>%
      addCircleMarkers(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]),radius = 10, color = spalva[38], group = autopav[38])%>%
      addCircleMarkers(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]),radius = 10, color = spalva[39], group = autopav[39])%>%
      addCircleMarkers(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]),radius = 10, color = spalva[40], group = autopav[40])%>%
      addCircleMarkers(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]),radius = 10, color = spalva[41], group = autopav[41])%>%
      addCircleMarkers(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]),radius = 10, color = spalva[42], group = autopav[42])%>%
      addCircleMarkers(m, lat = unlist(lat[[43]][1]), lng = unlist(lng[[43]][1]),radius = 10, color = spalva[43], group = autopav[43])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:43], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m44 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%
      addPopups(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]), popup = as.character(unlist(c1[[33]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[33])%>%
      addPopups(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]), popup = as.character(unlist(c1[[34]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[34])%>%
      addPopups(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]), popup = as.character(unlist(c1[[35]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[35])%>%
      addPopups(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]), popup = as.character(unlist(c1[[36]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[36])%>%
      addPopups(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]), popup = as.character(unlist(c1[[37]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[37])%>%
      addPopups(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]), popup = as.character(unlist(c1[[38]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[38])%>%
      addPopups(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]), popup = as.character(unlist(c1[[39]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[39])%>%
      addPopups(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]), popup = as.character(unlist(c1[[40]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[40])%>%
      addPopups(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]), popup = as.character(unlist(c1[[41]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[41])%>%
      addPopups(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]), popup = as.character(unlist(c1[[42]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[42])%>%
      addPopups(m, lat = unlist(lat[[43]][1]), lng = unlist(lng[[43]][1]), popup = as.character(unlist(c1[[43]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[43])%>%
      addPopups(m, lat = unlist(lat[[44]][1]), lng = unlist(lng[[44]][1]), popup = as.character(unlist(c1[[44]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[44])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%
      addCircleMarkers(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]),radius = 10, color = spalva[33], group = autopav[33])%>%
      addCircleMarkers(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]),radius = 10, color = spalva[34], group = autopav[34])%>%
      addCircleMarkers(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]),radius = 10, color = spalva[35], group = autopav[35])%>%
      addCircleMarkers(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]),radius = 10, color = spalva[36], group = autopav[36])%>%
      addCircleMarkers(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]),radius = 10, color = spalva[37], group = autopav[37])%>%
      addCircleMarkers(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]),radius = 10, color = spalva[38], group = autopav[38])%>%
      addCircleMarkers(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]),radius = 10, color = spalva[39], group = autopav[39])%>%
      addCircleMarkers(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]),radius = 10, color = spalva[40], group = autopav[40])%>%
      addCircleMarkers(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]),radius = 10, color = spalva[41], group = autopav[41])%>%
      addCircleMarkers(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]),radius = 10, color = spalva[42], group = autopav[42])%>%
      addCircleMarkers(m, lat = unlist(lat[[43]][1]), lng = unlist(lng[[43]][1]),radius = 10, color = spalva[43], group = autopav[43])%>%
      addCircleMarkers(m, lat = unlist(lat[[44]][1]), lng = unlist(lng[[44]][1]),radius = 10, color = spalva[44], group = autopav[44])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:44], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m45 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%
      addPopups(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]), popup = as.character(unlist(c1[[33]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[33])%>%
      addPopups(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]), popup = as.character(unlist(c1[[34]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[34])%>%
      addPopups(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]), popup = as.character(unlist(c1[[35]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[35])%>%
      addPopups(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]), popup = as.character(unlist(c1[[36]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[36])%>%
      addPopups(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]), popup = as.character(unlist(c1[[37]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[37])%>%
      addPopups(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]), popup = as.character(unlist(c1[[38]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[38])%>%
      addPopups(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]), popup = as.character(unlist(c1[[39]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[39])%>%
      addPopups(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]), popup = as.character(unlist(c1[[40]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[40])%>%
      addPopups(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]), popup = as.character(unlist(c1[[41]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[41])%>%
      addPopups(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]), popup = as.character(unlist(c1[[42]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[42])%>%
      addPopups(m, lat = unlist(lat[[43]][1]), lng = unlist(lng[[43]][1]), popup = as.character(unlist(c1[[43]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[43])%>%
      addPopups(m, lat = unlist(lat[[44]][1]), lng = unlist(lng[[44]][1]), popup = as.character(unlist(c1[[44]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[44])%>%
      addPopups(m, lat = unlist(lat[[45]][1]), lng = unlist(lng[[45]][1]), popup = as.character(unlist(c1[[45]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[45])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%
      addCircleMarkers(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]),radius = 10, color = spalva[33], group = autopav[33])%>%
      addCircleMarkers(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]),radius = 10, color = spalva[34], group = autopav[34])%>%
      addCircleMarkers(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]),radius = 10, color = spalva[35], group = autopav[35])%>%
      addCircleMarkers(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]),radius = 10, color = spalva[36], group = autopav[36])%>%
      addCircleMarkers(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]),radius = 10, color = spalva[37], group = autopav[37])%>%
      addCircleMarkers(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]),radius = 10, color = spalva[38], group = autopav[38])%>%
      addCircleMarkers(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]),radius = 10, color = spalva[39], group = autopav[39])%>%
      addCircleMarkers(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]),radius = 10, color = spalva[40], group = autopav[40])%>%
      addCircleMarkers(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]),radius = 10, color = spalva[41], group = autopav[41])%>%
      addCircleMarkers(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]),radius = 10, color = spalva[42], group = autopav[42])%>%
      addCircleMarkers(m, lat = unlist(lat[[43]][1]), lng = unlist(lng[[43]][1]),radius = 10, color = spalva[43], group = autopav[43])%>%
      addCircleMarkers(m, lat = unlist(lat[[44]][1]), lng = unlist(lng[[44]][1]),radius = 10, color = spalva[44], group = autopav[44])%>%
      addCircleMarkers(m, lat = unlist(lat[[45]][1]), lng = unlist(lng[[45]][1]),radius = 10, color = spalva[45], group = autopav[45])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:45], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m46 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%
      addPopups(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]), popup = as.character(unlist(c1[[33]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[33])%>%
      addPopups(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]), popup = as.character(unlist(c1[[34]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[34])%>%
      addPopups(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]), popup = as.character(unlist(c1[[35]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[35])%>%
      addPopups(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]), popup = as.character(unlist(c1[[36]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[36])%>%
      addPopups(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]), popup = as.character(unlist(c1[[37]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[37])%>%
      addPopups(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]), popup = as.character(unlist(c1[[38]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[38])%>%
      addPopups(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]), popup = as.character(unlist(c1[[39]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[39])%>%
      addPopups(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]), popup = as.character(unlist(c1[[40]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[40])%>%
      addPopups(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]), popup = as.character(unlist(c1[[41]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[41])%>%
      addPopups(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]), popup = as.character(unlist(c1[[42]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[42])%>%
      addPopups(m, lat = unlist(lat[[43]][1]), lng = unlist(lng[[43]][1]), popup = as.character(unlist(c1[[43]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[43])%>%
      addPopups(m, lat = unlist(lat[[44]][1]), lng = unlist(lng[[44]][1]), popup = as.character(unlist(c1[[44]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[44])%>%
      addPopups(m, lat = unlist(lat[[45]][1]), lng = unlist(lng[[45]][1]), popup = as.character(unlist(c1[[45]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[45])%>%
      addPopups(m, lat = unlist(lat[[46]][1]), lng = unlist(lng[[46]][1]), popup = as.character(unlist(c1[[46]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[46])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%
      addCircleMarkers(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]),radius = 10, color = spalva[33], group = autopav[33])%>%
      addCircleMarkers(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]),radius = 10, color = spalva[34], group = autopav[34])%>%
      addCircleMarkers(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]),radius = 10, color = spalva[35], group = autopav[35])%>%
      addCircleMarkers(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]),radius = 10, color = spalva[36], group = autopav[36])%>%
      addCircleMarkers(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]),radius = 10, color = spalva[37], group = autopav[37])%>%
      addCircleMarkers(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]),radius = 10, color = spalva[38], group = autopav[38])%>%
      addCircleMarkers(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]),radius = 10, color = spalva[39], group = autopav[39])%>%
      addCircleMarkers(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]),radius = 10, color = spalva[40], group = autopav[40])%>%
      addCircleMarkers(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]),radius = 10, color = spalva[41], group = autopav[41])%>%
      addCircleMarkers(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]),radius = 10, color = spalva[42], group = autopav[42])%>%
      addCircleMarkers(m, lat = unlist(lat[[43]][1]), lng = unlist(lng[[43]][1]),radius = 10, color = spalva[43], group = autopav[43])%>%
      addCircleMarkers(m, lat = unlist(lat[[44]][1]), lng = unlist(lng[[44]][1]),radius = 10, color = spalva[44], group = autopav[44])%>%
      addCircleMarkers(m, lat = unlist(lat[[45]][1]), lng = unlist(lng[[45]][1]),radius = 10, color = spalva[45], group = autopav[45])%>%
      addCircleMarkers(m, lat = unlist(lat[[46]][1]), lng = unlist(lng[[46]][1]),radius = 10, color = spalva[46], group = autopav[46])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:46], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m47 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%
      addPopups(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]), popup = as.character(unlist(c1[[33]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[33])%>%
      addPopups(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]), popup = as.character(unlist(c1[[34]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[34])%>%
      addPopups(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]), popup = as.character(unlist(c1[[35]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[35])%>%
      addPopups(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]), popup = as.character(unlist(c1[[36]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[36])%>%
      addPopups(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]), popup = as.character(unlist(c1[[37]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[37])%>%
      addPopups(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]), popup = as.character(unlist(c1[[38]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[38])%>%
      addPopups(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]), popup = as.character(unlist(c1[[39]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[39])%>%
      addPopups(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]), popup = as.character(unlist(c1[[40]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[40])%>%
      addPopups(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]), popup = as.character(unlist(c1[[41]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[41])%>%
      addPopups(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]), popup = as.character(unlist(c1[[42]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[42])%>%
      addPopups(m, lat = unlist(lat[[43]][1]), lng = unlist(lng[[43]][1]), popup = as.character(unlist(c1[[43]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[43])%>%
      addPopups(m, lat = unlist(lat[[44]][1]), lng = unlist(lng[[44]][1]), popup = as.character(unlist(c1[[44]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[44])%>%
      addPopups(m, lat = unlist(lat[[45]][1]), lng = unlist(lng[[45]][1]), popup = as.character(unlist(c1[[45]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[45])%>%
      addPopups(m, lat = unlist(lat[[46]][1]), lng = unlist(lng[[46]][1]), popup = as.character(unlist(c1[[46]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[46])%>%
      addPopups(m, lat = unlist(lat[[47]][1]), lng = unlist(lng[[47]][1]), popup = as.character(unlist(c1[[47]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[47])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%
      addCircleMarkers(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]),radius = 10, color = spalva[33], group = autopav[33])%>%
      addCircleMarkers(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]),radius = 10, color = spalva[34], group = autopav[34])%>%
      addCircleMarkers(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]),radius = 10, color = spalva[35], group = autopav[35])%>%
      addCircleMarkers(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]),radius = 10, color = spalva[36], group = autopav[36])%>%
      addCircleMarkers(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]),radius = 10, color = spalva[37], group = autopav[37])%>%
      addCircleMarkers(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]),radius = 10, color = spalva[38], group = autopav[38])%>%
      addCircleMarkers(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]),radius = 10, color = spalva[39], group = autopav[39])%>%
      addCircleMarkers(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]),radius = 10, color = spalva[40], group = autopav[40])%>%
      addCircleMarkers(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]),radius = 10, color = spalva[41], group = autopav[41])%>%
      addCircleMarkers(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]),radius = 10, color = spalva[42], group = autopav[42])%>%
      addCircleMarkers(m, lat = unlist(lat[[43]][1]), lng = unlist(lng[[43]][1]),radius = 10, color = spalva[43], group = autopav[43])%>%
      addCircleMarkers(m, lat = unlist(lat[[44]][1]), lng = unlist(lng[[44]][1]),radius = 10, color = spalva[44], group = autopav[44])%>%
      addCircleMarkers(m, lat = unlist(lat[[45]][1]), lng = unlist(lng[[45]][1]),radius = 10, color = spalva[45], group = autopav[45])%>%
      addCircleMarkers(m, lat = unlist(lat[[46]][1]), lng = unlist(lng[[46]][1]),radius = 10, color = spalva[46], group = autopav[46])%>%
      addCircleMarkers(m, lat = unlist(lat[[47]][1]), lng = unlist(lng[[47]][1]),radius = 10, color = spalva[47], group = autopav[47])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:47], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m48 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%
      addPopups(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]), popup = as.character(unlist(c1[[33]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[33])%>%
      addPopups(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]), popup = as.character(unlist(c1[[34]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[34])%>%
      addPopups(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]), popup = as.character(unlist(c1[[35]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[35])%>%
      addPopups(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]), popup = as.character(unlist(c1[[36]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[36])%>%
      addPopups(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]), popup = as.character(unlist(c1[[37]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[37])%>%
      addPopups(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]), popup = as.character(unlist(c1[[38]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[38])%>%
      addPopups(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]), popup = as.character(unlist(c1[[39]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[39])%>%
      addPopups(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]), popup = as.character(unlist(c1[[40]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[40])%>%
      addPopups(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]), popup = as.character(unlist(c1[[41]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[41])%>%
      addPopups(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]), popup = as.character(unlist(c1[[42]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[42])%>%
      addPopups(m, lat = unlist(lat[[43]][1]), lng = unlist(lng[[43]][1]), popup = as.character(unlist(c1[[43]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[43])%>%
      addPopups(m, lat = unlist(lat[[44]][1]), lng = unlist(lng[[44]][1]), popup = as.character(unlist(c1[[44]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[44])%>%
      addPopups(m, lat = unlist(lat[[45]][1]), lng = unlist(lng[[45]][1]), popup = as.character(unlist(c1[[45]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[45])%>%
      addPopups(m, lat = unlist(lat[[46]][1]), lng = unlist(lng[[46]][1]), popup = as.character(unlist(c1[[46]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[46])%>%
      addPopups(m, lat = unlist(lat[[47]][1]), lng = unlist(lng[[47]][1]), popup = as.character(unlist(c1[[47]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[47])%>%
      addPopups(m, lat = unlist(lat[[48]][1]), lng = unlist(lng[[48]][1]), popup = as.character(unlist(c1[[48]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[48])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%
      addCircleMarkers(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]),radius = 10, color = spalva[33], group = autopav[33])%>%
      addCircleMarkers(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]),radius = 10, color = spalva[34], group = autopav[34])%>%
      addCircleMarkers(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]),radius = 10, color = spalva[35], group = autopav[35])%>%
      addCircleMarkers(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]),radius = 10, color = spalva[36], group = autopav[36])%>%
      addCircleMarkers(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]),radius = 10, color = spalva[37], group = autopav[37])%>%
      addCircleMarkers(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]),radius = 10, color = spalva[38], group = autopav[38])%>%
      addCircleMarkers(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]),radius = 10, color = spalva[39], group = autopav[39])%>%
      addCircleMarkers(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]),radius = 10, color = spalva[40], group = autopav[40])%>%
      addCircleMarkers(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]),radius = 10, color = spalva[41], group = autopav[41])%>%
      addCircleMarkers(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]),radius = 10, color = spalva[42], group = autopav[42])%>%
      addCircleMarkers(m, lat = unlist(lat[[43]][1]), lng = unlist(lng[[43]][1]),radius = 10, color = spalva[43], group = autopav[43])%>%
      addCircleMarkers(m, lat = unlist(lat[[44]][1]), lng = unlist(lng[[44]][1]),radius = 10, color = spalva[44], group = autopav[44])%>%
      addCircleMarkers(m, lat = unlist(lat[[45]][1]), lng = unlist(lng[[45]][1]),radius = 10, color = spalva[45], group = autopav[45])%>%
      addCircleMarkers(m, lat = unlist(lat[[46]][1]), lng = unlist(lng[[46]][1]),radius = 10, color = spalva[46], group = autopav[46])%>%
      addCircleMarkers(m, lat = unlist(lat[[47]][1]), lng = unlist(lng[[47]][1]),radius = 10, color = spalva[47], group = autopav[47])%>%
      addCircleMarkers(m, lat = unlist(lat[[48]][1]), lng = unlist(lng[[48]][1]),radius = 10, color = spalva[48], group = autopav[48])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:48], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m49 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%
      addPopups(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]), popup = as.character(unlist(c1[[33]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[33])%>%
      addPopups(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]), popup = as.character(unlist(c1[[34]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[34])%>%
      addPopups(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]), popup = as.character(unlist(c1[[35]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[35])%>%
      addPopups(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]), popup = as.character(unlist(c1[[36]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[36])%>%
      addPopups(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]), popup = as.character(unlist(c1[[37]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[37])%>%
      addPopups(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]), popup = as.character(unlist(c1[[38]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[38])%>%
      addPopups(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]), popup = as.character(unlist(c1[[39]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[39])%>%
      addPopups(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]), popup = as.character(unlist(c1[[40]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[40])%>%
      addPopups(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]), popup = as.character(unlist(c1[[41]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[41])%>%
      addPopups(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]), popup = as.character(unlist(c1[[42]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[42])%>%
      addPopups(m, lat = unlist(lat[[43]][1]), lng = unlist(lng[[43]][1]), popup = as.character(unlist(c1[[43]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[43])%>%
      addPopups(m, lat = unlist(lat[[44]][1]), lng = unlist(lng[[44]][1]), popup = as.character(unlist(c1[[44]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[44])%>%
      addPopups(m, lat = unlist(lat[[45]][1]), lng = unlist(lng[[45]][1]), popup = as.character(unlist(c1[[45]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[45])%>%
      addPopups(m, lat = unlist(lat[[46]][1]), lng = unlist(lng[[46]][1]), popup = as.character(unlist(c1[[46]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[46])%>%
      addPopups(m, lat = unlist(lat[[47]][1]), lng = unlist(lng[[47]][1]), popup = as.character(unlist(c1[[47]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[47])%>%
      addPopups(m, lat = unlist(lat[[48]][1]), lng = unlist(lng[[48]][1]), popup = as.character(unlist(c1[[48]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[48])%>%
      addPopups(m, lat = unlist(lat[[49]][1]), lng = unlist(lng[[49]][1]), popup = as.character(unlist(c1[[49]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[49])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%
      addCircleMarkers(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]),radius = 10, color = spalva[33], group = autopav[33])%>%
      addCircleMarkers(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]),radius = 10, color = spalva[34], group = autopav[34])%>%
      addCircleMarkers(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]),radius = 10, color = spalva[35], group = autopav[35])%>%
      addCircleMarkers(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]),radius = 10, color = spalva[36], group = autopav[36])%>%
      addCircleMarkers(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]),radius = 10, color = spalva[37], group = autopav[37])%>%
      addCircleMarkers(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]),radius = 10, color = spalva[38], group = autopav[38])%>%
      addCircleMarkers(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]),radius = 10, color = spalva[39], group = autopav[39])%>%
      addCircleMarkers(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]),radius = 10, color = spalva[40], group = autopav[40])%>%
      addCircleMarkers(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]),radius = 10, color = spalva[41], group = autopav[41])%>%
      addCircleMarkers(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]),radius = 10, color = spalva[42], group = autopav[42])%>%
      addCircleMarkers(m, lat = unlist(lat[[43]][1]), lng = unlist(lng[[43]][1]),radius = 10, color = spalva[43], group = autopav[43])%>%
      addCircleMarkers(m, lat = unlist(lat[[44]][1]), lng = unlist(lng[[44]][1]),radius = 10, color = spalva[44], group = autopav[44])%>%
      addCircleMarkers(m, lat = unlist(lat[[45]][1]), lng = unlist(lng[[45]][1]),radius = 10, color = spalva[45], group = autopav[45])%>%
      addCircleMarkers(m, lat = unlist(lat[[46]][1]), lng = unlist(lng[[46]][1]),radius = 10, color = spalva[46], group = autopav[46])%>%
      addCircleMarkers(m, lat = unlist(lat[[47]][1]), lng = unlist(lng[[47]][1]),radius = 10, color = spalva[47], group = autopav[47])%>%
      addCircleMarkers(m, lat = unlist(lat[[48]][1]), lng = unlist(lng[[48]][1]),radius = 10, color = spalva[48], group = autopav[48])%>%
      addCircleMarkers(m, lat = unlist(lat[[49]][1]), lng = unlist(lng[[49]][1]),radius = 10, color = spalva[49], group = autopav[49])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:49], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m50 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%
      addPopups(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]), popup = as.character(unlist(c1[[33]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[33])%>%
      addPopups(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]), popup = as.character(unlist(c1[[34]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[34])%>%
      addPopups(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]), popup = as.character(unlist(c1[[35]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[35])%>%
      addPopups(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]), popup = as.character(unlist(c1[[36]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[36])%>%
      addPopups(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]), popup = as.character(unlist(c1[[37]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[37])%>%
      addPopups(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]), popup = as.character(unlist(c1[[38]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[38])%>%
      addPopups(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]), popup = as.character(unlist(c1[[39]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[39])%>%
      addPopups(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]), popup = as.character(unlist(c1[[40]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[40])%>%
      addPopups(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]), popup = as.character(unlist(c1[[41]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[41])%>%
      addPopups(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]), popup = as.character(unlist(c1[[42]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[42])%>%
      addPopups(m, lat = unlist(lat[[43]][1]), lng = unlist(lng[[43]][1]), popup = as.character(unlist(c1[[43]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[43])%>%
      addPopups(m, lat = unlist(lat[[44]][1]), lng = unlist(lng[[44]][1]), popup = as.character(unlist(c1[[44]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[44])%>%
      addPopups(m, lat = unlist(lat[[45]][1]), lng = unlist(lng[[45]][1]), popup = as.character(unlist(c1[[45]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[45])%>%
      addPopups(m, lat = unlist(lat[[46]][1]), lng = unlist(lng[[46]][1]), popup = as.character(unlist(c1[[46]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[46])%>%
      addPopups(m, lat = unlist(lat[[47]][1]), lng = unlist(lng[[47]][1]), popup = as.character(unlist(c1[[47]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[47])%>%
      addPopups(m, lat = unlist(lat[[48]][1]), lng = unlist(lng[[48]][1]), popup = as.character(unlist(c1[[48]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[48])%>%
      addPopups(m, lat = unlist(lat[[49]][1]), lng = unlist(lng[[49]][1]), popup = as.character(unlist(c1[[49]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[49])%>%
      addPopups(m, lat = unlist(lat[[50]][1]), lng = unlist(lng[[50]][1]), popup = as.character(unlist(c1[[50]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[50])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%
      addCircleMarkers(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]),radius = 10, color = spalva[33], group = autopav[33])%>%
      addCircleMarkers(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]),radius = 10, color = spalva[34], group = autopav[34])%>%
      addCircleMarkers(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]),radius = 10, color = spalva[35], group = autopav[35])%>%
      addCircleMarkers(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]),radius = 10, color = spalva[36], group = autopav[36])%>%
      addCircleMarkers(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]),radius = 10, color = spalva[37], group = autopav[37])%>%
      addCircleMarkers(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]),radius = 10, color = spalva[38], group = autopav[38])%>%
      addCircleMarkers(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]),radius = 10, color = spalva[39], group = autopav[39])%>%
      addCircleMarkers(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]),radius = 10, color = spalva[40], group = autopav[40])%>%
      addCircleMarkers(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]),radius = 10, color = spalva[41], group = autopav[41])%>%
      addCircleMarkers(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]),radius = 10, color = spalva[42], group = autopav[42])%>%
      addCircleMarkers(m, lat = unlist(lat[[43]][1]), lng = unlist(lng[[43]][1]),radius = 10, color = spalva[43], group = autopav[43])%>%
      addCircleMarkers(m, lat = unlist(lat[[44]][1]), lng = unlist(lng[[44]][1]),radius = 10, color = spalva[44], group = autopav[44])%>%
      addCircleMarkers(m, lat = unlist(lat[[45]][1]), lng = unlist(lng[[45]][1]),radius = 10, color = spalva[45], group = autopav[45])%>%
      addCircleMarkers(m, lat = unlist(lat[[46]][1]), lng = unlist(lng[[46]][1]),radius = 10, color = spalva[46], group = autopav[46])%>%
      addCircleMarkers(m, lat = unlist(lat[[47]][1]), lng = unlist(lng[[47]][1]),radius = 10, color = spalva[47], group = autopav[47])%>%
      addCircleMarkers(m, lat = unlist(lat[[48]][1]), lng = unlist(lng[[48]][1]),radius = 10, color = spalva[48], group = autopav[48])%>%
      addCircleMarkers(m, lat = unlist(lat[[49]][1]), lng = unlist(lng[[49]][1]),radius = 10, color = spalva[49], group = autopav[49])%>%
      addCircleMarkers(m, lat = unlist(lat[[50]][1]), lng = unlist(lng[[50]][1]),radius = 10, color = spalva[50], group = autopav[50])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:50], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)},

m51 = function(lng,lat,c1,autopav1,spalva,autolegend,autopav) {
  (m <- leaflet() %>% addTiles())
  (z <- m %>% setView(lng = mean(unlist(lng)), lat = mean(unlist(lat)), zoom = 10) %>% addTiles(group = "OpenStreetMap") %>% addProviderTiles("Stamen.Toner", group = "Balta / Juoda") %>% 
      addPopups(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]), popup = as.character(unlist(c1[[1]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[1])%>%
      addPopups(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]), popup = as.character(unlist(c1[[2]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[2])%>%
      addPopups(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]), popup = as.character(unlist(c1[[3]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[3])%>%
      addPopups(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]), popup = as.character(unlist(c1[[4]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[4])%>%
      addPopups(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]), popup = as.character(unlist(c1[[5]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[5])%>%
      addPopups(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]), popup = as.character(unlist(c1[[6]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[6])%>%
      addPopups(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]), popup = as.character(unlist(c1[[7]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[7])%>%
      addPopups(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]), popup = as.character(unlist(c1[[8]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[8])%>%
      addPopups(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]), popup = as.character(unlist(c1[[9]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[9])%>%
      addPopups(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]), popup = as.character(unlist(c1[[10]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[10])%>%
      addPopups(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]), popup = as.character(unlist(c1[[11]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[11])%>%
      addPopups(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]), popup = as.character(unlist(c1[[12]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[12])%>%
      addPopups(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]), popup = as.character(unlist(c1[[13]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[13])%>%
      addPopups(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]), popup = as.character(unlist(c1[[14]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[14])%>%
      addPopups(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]), popup = as.character(unlist(c1[[15]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[15])%>%
      addPopups(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]), popup = as.character(unlist(c1[[16]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[16])%>%
      addPopups(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]), popup = as.character(unlist(c1[[17]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[17])%>%
      addPopups(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]), popup = as.character(unlist(c1[[18]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[18])%>%
      addPopups(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]), popup = as.character(unlist(c1[[19]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[19])%>%
      addPopups(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]), popup = as.character(unlist(c1[[20]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[20])%>%
      addPopups(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]), popup = as.character(unlist(c1[[21]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[21])%>%
      addPopups(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]), popup = as.character(unlist(c1[[22]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[22])%>%
      addPopups(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]), popup = as.character(unlist(c1[[23]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[23])%>%
      addPopups(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]), popup = as.character(unlist(c1[[24]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[24])%>%
      addPopups(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]), popup = as.character(unlist(c1[[25]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[25])%>%
      addPopups(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]), popup = as.character(unlist(c1[[26]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[26])%>%
      addPopups(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]), popup = as.character(unlist(c1[[27]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[27])%>%
      addPopups(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]), popup = as.character(unlist(c1[[28]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[28])%>%
      addPopups(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]), popup = as.character(unlist(c1[[29]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[29])%>%
      addPopups(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]), popup = as.character(unlist(c1[[30]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[30])%>%
      addPopups(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]), popup = as.character(unlist(c1[[31]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[31])%>%
      addPopups(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]), popup = as.character(unlist(c1[[32]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[32])%>%
      addPopups(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]), popup = as.character(unlist(c1[[33]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[33])%>%
      addPopups(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]), popup = as.character(unlist(c1[[34]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[34])%>%
      addPopups(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]), popup = as.character(unlist(c1[[35]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[35])%>%
      addPopups(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]), popup = as.character(unlist(c1[[36]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[36])%>%
      addPopups(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]), popup = as.character(unlist(c1[[37]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[37])%>%
      addPopups(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]), popup = as.character(unlist(c1[[38]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[38])%>%
      addPopups(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]), popup = as.character(unlist(c1[[39]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[39])%>%
      addPopups(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]), popup = as.character(unlist(c1[[40]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[40])%>%
      addPopups(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]), popup = as.character(unlist(c1[[41]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[41])%>%
      addPopups(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]), popup = as.character(unlist(c1[[42]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[42])%>%
      addPopups(m, lat = unlist(lat[[43]][1]), lng = unlist(lng[[43]][1]), popup = as.character(unlist(c1[[43]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[43])%>%
      addPopups(m, lat = unlist(lat[[44]][1]), lng = unlist(lng[[44]][1]), popup = as.character(unlist(c1[[44]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[44])%>%
      addPopups(m, lat = unlist(lat[[45]][1]), lng = unlist(lng[[45]][1]), popup = as.character(unlist(c1[[45]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[45])%>%
      addPopups(m, lat = unlist(lat[[46]][1]), lng = unlist(lng[[46]][1]), popup = as.character(unlist(c1[[46]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[46])%>%
      addPopups(m, lat = unlist(lat[[47]][1]), lng = unlist(lng[[47]][1]), popup = as.character(unlist(c1[[47]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[47])%>%
      addPopups(m, lat = unlist(lat[[48]][1]), lng = unlist(lng[[48]][1]), popup = as.character(unlist(c1[[48]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[48])%>%
      addPopups(m, lat = unlist(lat[[49]][1]), lng = unlist(lng[[49]][1]), popup = as.character(unlist(c1[[49]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[49])%>%
      addPopups(m, lat = unlist(lat[[50]][1]), lng = unlist(lng[[50]][1]), popup = as.character(unlist(c1[[50]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[50])%>%
      addPopups(m, lat = unlist(lat[[51]][1]), lng = unlist(lng[[51]][1]), popup = as.character(unlist(c1[[51]])),options = popupOptions(closeOnClick = FALSE, closeButton = FALSE,maxHeight = 20), group = autopav1[51])%>%

      addCircleMarkers(m, lat = unlist(lat[[1]][1]), lng = unlist(lng[[1]][1]),radius = 10, color = spalva[1], group = autopav[1])%>%
      addCircleMarkers(m, lat = unlist(lat[[2]][1]), lng = unlist(lng[[2]][1]),radius = 10, color = spalva[2], group = autopav[2])%>%
      addCircleMarkers(m, lat = unlist(lat[[3]][1]), lng = unlist(lng[[3]][1]),radius = 10, color = spalva[3], group = autopav[3])%>%
      addCircleMarkers(m, lat = unlist(lat[[4]][1]), lng = unlist(lng[[4]][1]),radius = 10, color = spalva[4], group = autopav[4])%>%
      addCircleMarkers(m, lat = unlist(lat[[5]][1]), lng = unlist(lng[[5]][1]),radius = 10, color = spalva[5], group = autopav[5])%>%
      addCircleMarkers(m, lat = unlist(lat[[6]][1]), lng = unlist(lng[[6]][1]),radius = 10, color = spalva[6], group = autopav[6])%>%
      addCircleMarkers(m, lat = unlist(lat[[7]][1]), lng = unlist(lng[[7]][1]),radius = 10, color = spalva[7], group = autopav[7])%>%
      addCircleMarkers(m, lat = unlist(lat[[8]][1]), lng = unlist(lng[[8]][1]),radius = 10, color = spalva[8], group = autopav[8])%>%
      addCircleMarkers(m, lat = unlist(lat[[9]][1]), lng = unlist(lng[[9]][1]),radius = 10, color = spalva[9], group = autopav[9])%>%
      addCircleMarkers(m, lat = unlist(lat[[10]][1]), lng = unlist(lng[[10]][1]),radius = 10, color = spalva[10], group = autopav[10])%>%
      addCircleMarkers(m, lat = unlist(lat[[11]][1]), lng = unlist(lng[[11]][1]),radius = 10, color = spalva[11], group = autopav[11])%>%
      addCircleMarkers(m, lat = unlist(lat[[12]][1]), lng = unlist(lng[[12]][1]),radius = 10, color = spalva[12], group = autopav[12])%>%
      addCircleMarkers(m, lat = unlist(lat[[13]][1]), lng = unlist(lng[[13]][1]),radius = 10, color = spalva[13], group = autopav[13])%>%
      addCircleMarkers(m, lat = unlist(lat[[14]][1]), lng = unlist(lng[[14]][1]),radius = 10, color = spalva[14], group = autopav[14])%>%
      addCircleMarkers(m, lat = unlist(lat[[15]][1]), lng = unlist(lng[[15]][1]),radius = 10, color = spalva[15], group = autopav[15])%>%
      addCircleMarkers(m, lat = unlist(lat[[16]][1]), lng = unlist(lng[[16]][1]),radius = 10, color = spalva[16], group = autopav[16])%>%
      addCircleMarkers(m, lat = unlist(lat[[17]][1]), lng = unlist(lng[[17]][1]),radius = 10, color = spalva[17], group = autopav[17])%>%
      addCircleMarkers(m, lat = unlist(lat[[18]][1]), lng = unlist(lng[[18]][1]),radius = 10, color = spalva[18], group = autopav[18])%>%
      addCircleMarkers(m, lat = unlist(lat[[19]][1]), lng = unlist(lng[[19]][1]),radius = 10, color = spalva[19], group = autopav[19])%>%
      addCircleMarkers(m, lat = unlist(lat[[20]][1]), lng = unlist(lng[[20]][1]),radius = 10, color = spalva[20], group = autopav[20])%>%
      addCircleMarkers(m, lat = unlist(lat[[21]][1]), lng = unlist(lng[[21]][1]),radius = 10, color = spalva[21], group = autopav[21])%>%
      addCircleMarkers(m, lat = unlist(lat[[22]][1]), lng = unlist(lng[[22]][1]),radius = 10, color = spalva[22], group = autopav[22])%>%
      addCircleMarkers(m, lat = unlist(lat[[23]][1]), lng = unlist(lng[[23]][1]),radius = 10, color = spalva[23], group = autopav[23])%>%
      addCircleMarkers(m, lat = unlist(lat[[24]][1]), lng = unlist(lng[[24]][1]),radius = 10, color = spalva[24], group = autopav[24])%>%
      addCircleMarkers(m, lat = unlist(lat[[25]][1]), lng = unlist(lng[[25]][1]),radius = 10, color = spalva[25], group = autopav[25])%>%
      addCircleMarkers(m, lat = unlist(lat[[26]][1]), lng = unlist(lng[[26]][1]),radius = 10, color = spalva[26], group = autopav[26])%>%
      addCircleMarkers(m, lat = unlist(lat[[27]][1]), lng = unlist(lng[[27]][1]),radius = 10, color = spalva[27], group = autopav[27])%>%
      addCircleMarkers(m, lat = unlist(lat[[28]][1]), lng = unlist(lng[[28]][1]),radius = 10, color = spalva[28], group = autopav[28])%>%
      addCircleMarkers(m, lat = unlist(lat[[29]][1]), lng = unlist(lng[[29]][1]),radius = 10, color = spalva[29], group = autopav[29])%>%
      addCircleMarkers(m, lat = unlist(lat[[30]][1]), lng = unlist(lng[[30]][1]),radius = 10, color = spalva[30], group = autopav[30])%>%
      addCircleMarkers(m, lat = unlist(lat[[31]][1]), lng = unlist(lng[[31]][1]),radius = 10, color = spalva[31], group = autopav[31])%>%
      addCircleMarkers(m, lat = unlist(lat[[32]][1]), lng = unlist(lng[[32]][1]),radius = 10, color = spalva[32], group = autopav[32])%>%
      addCircleMarkers(m, lat = unlist(lat[[33]][1]), lng = unlist(lng[[33]][1]),radius = 10, color = spalva[33], group = autopav[33])%>%
      addCircleMarkers(m, lat = unlist(lat[[34]][1]), lng = unlist(lng[[34]][1]),radius = 10, color = spalva[34], group = autopav[34])%>%
      addCircleMarkers(m, lat = unlist(lat[[35]][1]), lng = unlist(lng[[35]][1]),radius = 10, color = spalva[35], group = autopav[35])%>%
      addCircleMarkers(m, lat = unlist(lat[[36]][1]), lng = unlist(lng[[36]][1]),radius = 10, color = spalva[36], group = autopav[36])%>%
      addCircleMarkers(m, lat = unlist(lat[[37]][1]), lng = unlist(lng[[37]][1]),radius = 10, color = spalva[37], group = autopav[37])%>%
      addCircleMarkers(m, lat = unlist(lat[[38]][1]), lng = unlist(lng[[38]][1]),radius = 10, color = spalva[38], group = autopav[38])%>%
      addCircleMarkers(m, lat = unlist(lat[[39]][1]), lng = unlist(lng[[39]][1]),radius = 10, color = spalva[39], group = autopav[39])%>%
      addCircleMarkers(m, lat = unlist(lat[[40]][1]), lng = unlist(lng[[40]][1]),radius = 10, color = spalva[40], group = autopav[40])%>%
      addCircleMarkers(m, lat = unlist(lat[[41]][1]), lng = unlist(lng[[41]][1]),radius = 10, color = spalva[41], group = autopav[41])%>%
      addCircleMarkers(m, lat = unlist(lat[[42]][1]), lng = unlist(lng[[42]][1]),radius = 10, color = spalva[42], group = autopav[42])%>%
      addCircleMarkers(m, lat = unlist(lat[[43]][1]), lng = unlist(lng[[43]][1]),radius = 10, color = spalva[43], group = autopav[43])%>%
      addCircleMarkers(m, lat = unlist(lat[[44]][1]), lng = unlist(lng[[44]][1]),radius = 10, color = spalva[44], group = autopav[44])%>%
      addCircleMarkers(m, lat = unlist(lat[[45]][1]), lng = unlist(lng[[45]][1]),radius = 10, color = spalva[45], group = autopav[45])%>%
      addCircleMarkers(m, lat = unlist(lat[[46]][1]), lng = unlist(lng[[46]][1]),radius = 10, color = spalva[46], group = autopav[46])%>%
      addCircleMarkers(m, lat = unlist(lat[[47]][1]), lng = unlist(lng[[47]][1]),radius = 10, color = spalva[47], group = autopav[47])%>%
      addCircleMarkers(m, lat = unlist(lat[[48]][1]), lng = unlist(lng[[48]][1]),radius = 10, color = spalva[48], group = autopav[48])%>%
      addCircleMarkers(m, lat = unlist(lat[[49]][1]), lng = unlist(lng[[49]][1]),radius = 10, color = spalva[49], group = autopav[49])%>%
      addCircleMarkers(m, lat = unlist(lat[[50]][1]), lng = unlist(lng[[50]][1]),radius = 10, color = spalva[50], group = autopav[50])%>%
      addCircleMarkers(m, lat = unlist(lat[[51]][1]), lng = unlist(lng[[51]][1]),radius = 10, color = spalva[51], group = autopav[51])%>%

      addLegend(position = c("bottomleft"),values = as.character(autopav),  colors = spalva[1:51], labels = as.character(autolegend))%>%
      addLayersControl(baseGroups = c("Gatvi\u0173 \u017Eem\u0117lapis","Balta / Juoda"), overlayGroups = c(autopav, autopav1)))
  return(z)}
)
