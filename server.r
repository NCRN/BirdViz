library(shiny)
library(leaflet)
library(NCRNbirds)
NCRN<-importNCRNbirds("./Data/")
shinyServer(function(input,output){

  BMap<-leaflet() %>%
    addTiles("//{s}.tiles.mapbox.com/v4/nps.2yxv8n84,nps.jhd2e8lb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q") %>% addCircles(data=getPoints(object=NCRN))
    
  output$BirdMap<-renderLeaflet(BMap)
  
  
})