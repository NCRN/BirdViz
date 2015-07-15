library(shiny)
library(leaflet)
library(NCRNbirds)
library(dplyr)
library(magrittr)
library(jsonlite, pos=100)
NCRN<-importNCRNbirds("./Data/")
ParkList<-getParkNames(NCRN, name.class="code")
names(ParkList)<-getParkNames(NCRN, name.class="short")

ParkBounds<-read.csv(file="./Data/boundboxes.csv", as.is=TRUE)



shinyServer(function(input,output,session){
  
  output$Test<-renderPrint(input$MapHide)
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
   # toggle(id="BaseLayerPanel", condition=("BaseLayers" %in% input$MapHide))
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
    addLayersControl(map=., baseGroups=c("Map","Imagery","Slate"),options=layersControlOptions(collapsed=F))}
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
    clearShapes() %>% 
    addCircles(data=circleData(), layerId=circleData()$Point_Name, color=MapColors()(circleData()$Values),
          fillColor = MapColors()(circleData()$Values), opacity=0.8, radius=50*as.numeric(input$PointSize), fillOpacity = 0.8) 
  })
  
  ### Add Legend
 observe({ 
    leafletProxy("BirdMap") %>% 
     clearControls() %>% 
     {if("Legend" %in% input$MapHide) 
        addLegend(map=., layerId="CircleLegend",pal=MapColors(), values= circleData()$Values, 
             na.label="Not Visited", title=circleLegend(), className="panel panel-default info legend")}
  })
  
  
  
  ## Popup for user clicking a circle
  observeEvent(input$BirdMap_shape_click, {  
    cl_Cir<-input$BirdMap_shape_click
    leafletProxy("BirdMap") %>% 
        clearPopups() %>% 
        addPopups(lat=cl_Cir$lat, lng=cl_Cir$lng, 
            popup=switch(input$MapValues,
                
                richness=  paste(collapse="<br/>",
                    paste(cl_Cir$id, ':',circleData()[circleData()$Point_Name==cl_Cir$id,]$Values, "Species","<br/>","<br/>",collapse=" "),
                    paste(getBirdNames(object=NCRN[[1]],names=getChecklist(NCRN,points=cl_Cir$id, years=input$MapYear), in.style="AOU", 
                                       out.style=input$MapNames),collapse="<br/>") ),
                
                individual=paste(collapse="<br/>", 
                                 paste(cl_Cir$id,"<br/>"),
                                 paste(circleData()[circleData()$Point_Name==cl_Cir$id,]$Values,"detected", collapse=" ")),
                
                bci=paste(sep="<br/>", cl_Cir$id, paste0('BCI Value: ',circleData()[circleData()$Point_Name==cl_Cir$id,]$BCI),
                          paste('BCI Category: ', circleData()[circleData()$Point_Name==cl_Cir$id,]$Values) )
            )
        )
  })
  
  ### Add additional layers
  
#   Ecoreg<-readLines("T:/I&M/MONITORING/Forest_Birds/BirdViz/Maps/ecoregion.geojson") %>% paste(collapse="\n") %>% fromJSON(simplifyVector=FALSE)
#   
#   Ecoreg_Facs<-unique( sapply(Ecoreg$features, function(feat) {feat$properties$Simplified}) )
#   
#   LayerColors<-colorFactor("RdYlBu", levels=Ecoreg_Facs)
#   
#   Ecoreg$features <- lapply(Ecoreg$features, function(feat) {
#     feat$properties$style <- list(
#       fillColor = LayerColors(feat$properties$Simplified),
#       color=LayerColors(feat$properties$Simplified),
#       opacity=0.75
#     )
#     feat
#   })
#   
#   
#   observe({leafletProxy("BirdMap") %>% 
#             addGeoJSON(geojson=Ecoreg)
#   })
  
})
