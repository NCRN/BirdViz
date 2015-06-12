library(shiny)
library(leaflet)
library(NCRNbirds)
library(dplyr)
NCRN<-importNCRNbirds("./Data/")
shinyServer(function(input,output,session){


  
  ##### Set up Map #############
  
  output$BirdMap<-renderLeaflet({leaflet() %>%
      #addTiles("//{s}.tiles.mapbox.com/v4/nps.2yxv8n84,nps.jhd2e8lb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q") %>% 
     setView(lng=-77.8,lat=39.03,zoom=9)
  })

  
  ###toggles
  observe({
    toggle( id="SpeciesControls" , condition= (input$MapValues=="individual"))
    toggleState( id='SpeciesValues', condition=( input$MapSpecies!=""))
  })
  
  ### Reactive Map UI Widgets
  
  observe({
    updateSelectizeInput(session,inputId="MapSpecies",label="Species", choices=getChecklist(NCRN, years=input$MapYear), 
                   options = list(placeholder='Choose a species'), server = TRUE
    )
  })
  
  ##### Choose BaseMap
  observe({
    leafletProxy("BirdMap") %>% 
      clearTiles() %>% 
      addTiles(urlTemplate= switch(input$MapBase,
        Map="//{s}.tiles.mapbox.com/v4/nps.2yxv8n84,nps.jhd2e8lb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",
        
        Imagery="//{s}.tiles.mapbox.com/v4/nps.map-n9nxe12m,nps.gdipreks,nps.jhd2e8lb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q"
        )
      )
        
    })

  
  
  ####### Circle Data 
  
  ### based on input$mapvalues, get relevant data and add it to map
  
  circleValues<-reactive({
    switch(input$MapValues,
          nothing=c(1:10),
          richness={
             X<-data.frame(Point_Name=getPoints(NCRN,years=input$MapYear)$Plot_Name) %>%      
               group_by(Point_Name) %>% mutate(Richness=birdRichness(NCRN,points=Point_Name,years=input$MapYear))
             as.vector(X$Richness)
             },
          individual=c(1:5)
           
    )
    
  })
  
  ### Color funciton for circles
  MapColors<-reactive({
    colorNumeric(palette=c("cyan","magenta4","orangered3"),domain=circleValues())
  })  

  ### Update Map Circles and the legend
       observe({
         leafletProxy("BirdMap") %>%   
         clearShapes() %>% 
          addCircles(data=getPoints(object=NCRN,years=input$MapYear), color=MapColors()(circleValues()), 
                fillColor=MapColors()(circleValues()),opacity=0.8, radius=50,fillOpacity = 0.8) %>% 
          addLegend(layerId="CircleLegned",pal=MapColors(), values=circleValues(), title="test legend")
        })
  
})
