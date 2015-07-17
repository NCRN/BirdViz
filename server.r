library(shiny)
library(leaflet)
library(NCRNbirds)
library(dplyr)
library(magrittr)
library(jsonlite, pos=100)
library(rgdal)

NCRN<-importNCRNbirds("./Data/")
ParkList<-getParkNames(NCRN, name.class="code")
names(ParkList)<-getParkNames(NCRN, name.class="short")

ParkBounds<-read.csv(file="./Data/boundboxes.csv", as.is=TRUE)



shinyServer(function(input,output,session){
  
  output$Test<-renderPrint(input$BirdMap_shape_click$group)
  ##### Set up Map #############
  
  output$BirdMap<-renderLeaflet({leaflet() %>%
     setView(lng=-77.8,lat=39.03,zoom=9)
  })

  
  ###toggles
  observe({
    toggle( id="SpeciesControls" , condition= (input$MapValues=="individual"))
    toggleState( id='SpeciesValues', condition=( input$MapSpecies!=""))
    toggle(id="MapControlPanel", condition=("MapControls" %in% input$MapHide))
    toggle(id="ZoomPanel", condition=("Zoom" %in% input$MapHide))
  })
  

  ### Reactive Map UI Widgets
  
  #### List of species for map
  
  BirdNames<-reactive({
    BN<-getChecklist(object =  NCRN, years=input$MapYear,band=1)
    TempNames<-getBirdNames(object=NCRN[[1]], names=BN, in.style="AOU", out.style=input$MapNames)
    TempNames[is.na(TempNames)]<-"Needs Name"
    names(BN)<-TempNames
    BN [order(TempNames)]
  })
  

  
  observe({
    updateSelectizeInput(session,inputId="MapSpecies",label="Species", choices=c(BirdNames()),
                         options = list(placeholder='Choose a species'),server = FALSE)
  })

#### Make map with Base Layer and Layer Controls
  
  observe({
    leafletProxy("BirdMap") %>% 
      
    clearTiles() %>% 
      
    addTiles(group="Map", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.2yxv8n84,nps.jhd2e8lb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q") %>% 
    addTiles(group="Imagery", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.map-n9nxe12m,nps.gdipreks,nps.jhd2e8lb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q") %>% 
    addTiles(group="Slate", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.68926899,nps.502a840b/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q" ) %>% 
    {if("BaseLayers" %in% input$MapHide) 
      
    addLayersControl(map=., baseGroups=c("Map","Imagery","Slate"),
                     overlayGroups=c("Ecoregions","Circles"),
                     options=layersControlOptions(collapsed=F,autoZIndex = F))}
})

  ### Hide Layers Control
  observe({
    if(!"BaseLayers" %in% input$MapHide ) leafletProxy("BirdMap") %>% removeLayersControl()
  })
  
  
#   #### Zoom Control for Map
  output$ParkZoomControl<-renderUI({
    selectInput(inputId="ParkZoom",label=NULL, choices=c("All Parks"="All", ParkList ) ) 
  })
   
#   ############Zoom the map
   observe({
     input$MapZoom
      isolate({
        BoundsUse<-reactive({ as.numeric(ParkBounds[ParkBounds$ParkCode==input$ParkZoom,2:5]) })
      leafletProxy("BirdMap") %>% fitBounds(lat1=BoundsUse()[1],lng1=BoundsUse()[2],lat2=BoundsUse()[3],lng2=BoundsUse()[4])
      })
      })
  
  
  ###### Circle Data 
  
  ### based on input$mapvalues, get relevant data and add it to map
  
  circleData<-reactive({
    P<-getPoints(NCRN,years=input$MapYear)
    switch(input$MapValues,
      
      richness={withProgress(message="Calculating...  Please Wait",value=1,
                return(P %>% group_by(Point_Name) %>% mutate(Values=birdRichness(NCRN,points=Point_Name, years=input$MapYear)))
        )},
      
      individual={
        X<-CountXVisit(object=NCRN,years=input$MapYear,AOU=input$MapSpecies)
        switch(input$SpeciesValues,
          "Visit 1"={return(P %>% left_join(X %>% dplyr::select(Point_Name,Visit1) %>% 
                                              rename(Values=Visit1) ) )},
          "Visit 2" ={return(P %>% left_join(X %>% dplyr::select(Point_Name,Visit2) %>% 
                                              rename(Values=Visit2) ) )},
          "Maximum Observed"={return(P %>% left_join(X %>% dplyr::select(Point_Name,Visit1,Visit2) %>% 
                                                       transmute(Point_Name=Point_Name,Values=pmax(Visit1,Visit2,na.rm=TRUE) ) ))}
        )
      },
      bci={withProgress(message="Calculating...  Please Wait",value=1,
                        return(P %>% left_join(BCI(object=NCRN, years=input$MapYear,points=P$Point_Name) %>% 
                                                 mutate(Values=factor(BCI_Category, 
                                                   levels=c("Low Integrity","Medium Integrity","High Integrity","Highest Integrity"))) %>% 
                                                 dplyr::select(Point_Name,BCI,Values) )  )
      )}
    )
    
  })
  
  #### Circle Legends
  circleLegend<-reactive({
    switch(input$MapValues,
           richness="# of Species",
           individual="# Observed",
           bci="Bird Community Index")
  })
  
  ### Color funciton for circles
  MapColors<-reactive({
    switch(input$MapValues,
           richness= colorNumeric(palette=c("cyan","magenta4","orangered3"),domain=circleData()$Values),
          individual=, bci=  colorFactor(palette=c("cyan","magenta4","orangered3"), domain=circleData()$Values)
    )
  })  


  ### Add Map Circle
  observe({
    leafletProxy("BirdMap") %>%  
    clearGroup("Circles") %>% 
    addCircles(data=circleData(), layerId=circleData()$Point_Name, group="Circles", color=MapColors()(circleData()$Values),
          fillColor = MapColors()(circleData()$Values), opacity=0.8, radius=50*as.numeric(input$PointSize), fillOpacity = 0.8) 
  })
  
  ### Add Legend
 observe({ 
    leafletProxy("BirdMap") %>% 
     removeControl(layerId="CircleLegend") %>% 
     {if("Legend" %in% input$MapHide) 
        addLegend(map=., layerId="CircleLegend",pal=MapColors(), values= circleData()$Values, 
             na.label="Not Visited", title=circleLegend(), className="panel panel-default info legend")}
  })
  
 ### User Clicks on map not on a shape - popups close

 observeEvent(input$BirdMap_click, {
   leafletProxy("BirdMap") %>% 
     clearPopups()
 })
  
  
  ## Popup for user clicking on a shape
  observeEvent(input$BirdMap_shape_click, {  
    
    ShapeClick<-input$BirdMap_shape_click
    
    leafletProxy("BirdMap") %>% 
      clearPopups() %>% {
      switch(ShapeClick$group,
        Circles= addPopups(map=.,lat=ShapeClick$lat, lng=ShapeClick$lng, 
          popup=switch(input$MapValues,
            
            richness=  paste(collapse="<br/>",
              paste(ShapeClick$id, ':',circleData()[circleData()$Point_Name==ShapeClick$id,]$Values, 
                  "Species","<br/>","<br/>",collapse=" "),
              paste(getBirdNames(object=NCRN[[1]],names=getChecklist(NCRN,points=ShapeClick$id, years=input$MapYear),
                                 in.style="AOU", out.style=input$MapNames),collapse="<br/>") ),
                
            individual=paste(collapse="<br/>", 
                paste(ShapeClick$id,"<br/>"),
                paste(circleData()[circleData()$Point_Name==ShapeClick$id,]$Values,"detected", collapse=" ")),
                
            bci=paste(sep="<br/>", ShapeClick$id, paste0('BCI Value: ',circleData()[circleData()$Point_Name==ShapeClick$id,]$BCI),
                      paste('BCI Category: ', circleData()[circleData()$Point_Name==ShapeClick$id,]$Values) )
            )
        ),
        Ecoregions=addPopups(map=.,lat=ShapeClick$lat, lng=ShapeClick$lng, popup=ShapeClick$id)
      )}
  })
  
  ### Add additional layers
  
  Ecoregion<-readOGR(dsn="T:/I&M/MONITORING/Forest_Birds/BirdViz/Maps/ecoregion.geojson","OGRGeoJSON")
   observe({
     leafletProxy("BirdMap") %>% 
       
       addPolygons(data=Ecoregion, group="Ecoregions",layerId=Ecoregion$Level3_Nam, stroke=FALSE, fillOpacity=.65, 
                   color=colorFactor("RdYlBu", levels=Ecoregion$Level3_Nam)(Ecoregion$Level3_Nam)) %>% 
      addLegend(title="Layer Legend",pal=colorFactor("RdYlBu", levels=Ecoregion$Level3_Nam), values=Ecoregion$Level3_Nam) %>% 
       showGroup(group="Circles")
   })
  
})
