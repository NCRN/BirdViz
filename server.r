library(shiny)
library(leaflet)
library(NCRNbirds)
NCRN<-importNCRNbirds("./Data/")
shinyServer(function(input,output){


  MapColors<-reactive({
    colorNumeric(palette=c("cyan","magenta4","orangered3"),domain=circleValues())
  })
  
  ####### Circle Data 
  
  
  ### based on input$mapvalues, get relevant data and add it to map
  
  circleValues<-reactive({
    switch(input$mapValues,
           nothing=c(1:10),
           richness=1
           #get the point names and count species.
           )
    
  })
  
  
  output$BirdMap<-renderLeaflet({leaflet() %>%
      
      addTiles("//{s}.tiles.mapbox.com/v4/nps.2yxv8n84,nps.jhd2e8lb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q") %>% 
      
      addCircles(data=getPoints(object=NCRN), color=MapColors()(circleValues()), fillColor=MapColors()(circleValues())) %>% 
                   
      addLegend(pal=MapColors(), values=circleValues(), title="test legend")
  })
  
  
})