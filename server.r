library(shiny)
library(leaflet)
library(NCRNbirds)
library(dplyr)
library(magrittr)
library(tidyr)
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
    updateSelectizeInput(session,inputId="MapSpecies",label="Species", choices=getChecklist(NCRN, years=input$MapYear, band=1), 
                   options = list(placeholder='Choose a species'),server = FALSE
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
  
  circleData<-reactive({
    P<-getPoints(NCRN,years=input$MapYear)
    switch(input$MapValues,
      nothing={
        return(P %>% mutate(Values=row_number() )) 
      },
      
      richness={
        return(P %>% group_by(Plot_Name) %>% mutate(Values=birdRichness(NCRN,points=Plot_Name, years=input$MapYear)))
        },
      
      individual={
        X<-CountXVisit(object=NCRN,years=input$MapYear,AOU=input$MapSpecies)
        switch(input$SpeciesValues,
          "Visit 1"={return(P %>% left_join(X %>% dplyr::select(Plot_Name,Visit1) %>% 
                                              transmute(Plot_Name=Plot_Name,Values=Visit1) ) )},
          "Visit 2" ={return(P %>% left_join(X %>% dplyr::select(Plot_Name,Visit2) %>% 
                                              transmute(Plot_Name=Plot_Name,Values=Visit2) ) )},
          "Maximum Observed"={return(P %>% left_join(X %>% dplyr::select(Plot_Name,Visit1,Visit2) %>% 
                                                       transmute(Plot_Name=Plot_Name,Values=pmax(Visit1,Visit2,na.rm=TRUE) ) ))}
        )
      } 
    )
    
  })
  
  ### Color funciton for circles
  MapColors<-reactive({
    colorNumeric(palette=c("cyan","magenta4","orangered3"),domain=circleData()$Values)
  })  

  ### Update Map Circles and the legend
       observe({
         leafletProxy("BirdMap") %>%   
         clearShapes() %>% 
           addCircles(data=circleData(), color=MapColors()(circleData()$Values),
                     fillColor = MapColors()(circleData()$Values), opacity=0.8, radius=50, fillOpacity = 0.8,
                     popup=paste0(circleData()$Plot_Name," : ",circleData()$Values)) %>% 
          addLegend(layerId="CircleLegned",pal=MapColors(), values=circleData()$Values,na.label="Not Visited", title="test legend")
        })
  
})
