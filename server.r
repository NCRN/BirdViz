library(shiny)
library(leaflet)
library(NCRNbirds)
library(dplyr)
library(magrittr)
library(rgdal)
library(DT)
library(ggvis)
library(tidyr)
library(jsonlite, pos=100)



NCRN<-importNCRNbirds("./Data/")
ParkList<-getParkNames(NCRN, name.class="code")
names(NCRN)<-ParkList
names(ParkList)<-getParkNames(NCRN, name.class="short")

ParkBounds<-read.csv(file="./Data/boundboxes.csv", as.is=TRUE)



shinyServer(function(input,output,session){
 
  output$Test<-renderPrint(input$BirdMap_shape_click)

  ##### Set up Map #############
  
  output$BirdMap<-renderLeaflet({leaflet() %>%
     setView(lng=-77.8,lat=39.03,zoom=9)
  })

  
  ###toggles
  observe({
    ### Maps
    toggle( id="SpeciesControls" , condition= (input$MapValues=="individual"))
    toggleState( id='SpeciesValues', condition=( input$MapSpecies!=""))
    toggle(id="MapControlPanel", condition=("MapControls" %in% input$MapHide))
    toggle(id="ZoomPanel", condition=("Zoom" %in% input$MapHide))
    toggle(id="ExtraLayerPanel", condition=("ExtraLayers" %in% input$MapHide))
    toggle(id="EBirdTitle", condition=(input$MapValues=="individual"))
    toggle(id="MapEBird", condition = (input$MapValues=="individual"))
    toggle(id="MapEBirdDays", condition =(input$MapEBird & input$MapValues=="individual")) 
    
    ### Tables
    toggle(id="TableSpecies", condition=input$TableValues %in% c("individual","detects"))
    toggle(id="TableBand", condition=input$TableValues == "individual")
    toggle(id="TableNames", condition=input$TableValues%in% c("individual","detects"))
    toggle(id="TableYear", condition=input$TableValues %in% c("individual","richness","bci") )
  #  toggle(id="TableYear2", condition=input$TableValues == "richness")
  })
  

  ### Reactive Map UI Widgets
  
  #### Band to use for map

  MapBandUse<-reactive({ if(input$MapBand=="All") NA else seq(as.numeric(input$MapBand)) })
  #### List of species for map - needs to be named list.
  
  
  
  BirdNames<-reactive({
    BN<-getChecklist(object =  NCRN,
                     #years=input$MapYear,
                     band=MapBandUse())
    TempNames<-getBirdNames(object=NCRN[[1]], names=BN, in.style="AOU", out.style=input$MapNames)
    TempNames[is.na(TempNames)]<-"Needs Name"
    names(BN)<-TempNames
    BN [order(TempNames)]
  })
  

  
  observe({
    BirdNames()
    updateSelectizeInput(session,inputId="MapSpecies",label="Species", choices=c(BirdNames()),
                         options = list(placeholder='Choose a species'),server = FALSE,
                         selected=if(!input$MapSpecies==""){input$MapSpecies})
  })

#### Make map with Base Layer and Layer Controls
  
  observe({
    leafletProxy("BirdMap") %>% 
      
    clearTiles() %>% 
      
    addTiles(group="Map", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.2yxv8n84,nps.jhd2e8lb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q") %>% 
    addTiles(group="Imagery", urlTemplate="//{s}.tiles.mapbox.com/v4/mapbox.satellite,nps.gdipreks,nps.08c8af87/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q") %>% 
    addTiles(group="Slate", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.68926899,nps.502a840b/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q" ) %>% 
    {if("BaseLayers" %in% input$MapHide) 
      
    addLayersControl(map=., baseGroups=c("Map","Imagery","Slate"),
                     options=layersControlOptions(collapsed=F))}
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
     input$Zoom
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
                return(P %>% group_by(Point_Name) %>% 
                  mutate(Values=birdRichness(NCRN,points=Point_Name, 
                    years=input$MapYear,band=MapBandUse()
                  ))
                )
        )},
      
      individual={
        X<-CountXVisit(object=NCRN,years=input$MapYear,AOU=input$MapSpecies, band=MapBandUse() )
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
           return(P %>% left_join(BCI(object=NCRN, years=input$MapYear,points=P$Point_Name,
                                      band=MapBandUse()  ) %>% 
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
           individual=paste(getBirdNames(object=NCRN[[1]], names =  input$MapSpecies, in.style="AOU", 
                                         out.style = input$MapNames), "<br>", " Observed"),
           bci="Bird Community Index")
  })
  
  ### Color funciton for circles
  MapColors<-reactive({
    switch(input$MapValues,
      richness= colorNumeric(palette=c("cyan","magenta4","orangered3"),domain=circleData()$Values),
      individual=, bci=  colorFactor(palette=c("cyan","magenta4","orangered3","yellow"), domain=0:8)
    )
  })  


  ### Add Map Circle
  observe({
    input$Layers
    leafletProxy("BirdMap") %>%  
    clearGroup("Circles") %>% 
    addCircles(data=circleData(), layerId=circleData()$Point_Name, group="Circles", color=MapColors()(circleData()$Values),
          fillColor = MapColors()(circleData()$Values), opacity=0.8, radius=as.numeric(input$PointSize), fillOpacity = 1) 
  })
  
  ### Figure out values for Map legend
  LegendValues<-reactive({
    
    if(!input$MapEBird) unique(circleData()$Values) else{
           TempValues<-unique(c(circleData()$Values,EBirdData()$howMany))  
           TempValues[TempValues>8]<-"9+"
           return(TempValues)
    }
  })
  
  ### Add Legend
 observe({ 
    leafletProxy("BirdMap") %>% 
     removeControl(layerId="CircleLegend") %>% 
     {if("Legends" %in% input$MapHide) 
        addLegend(map=., layerId="CircleLegend",pal=MapColors(), 
              values=LegendValues(),
             na.label="Not Visited", title=circleLegend() )} #, className="panel panel-default info legend"
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
              paste(getChecklist(NCRN,points=ShapeClick$id, years=input$MapYear, out.style=input$MapNames),collapse="<br/>") ),
                
            individual=paste(collapse="<br/>", 
                paste(ShapeClick$id,"<br/>"),
                paste(circleData()[circleData()$Point_Name==ShapeClick$id,]$Values,"detected", collapse=" ")),
                
            bci=paste(sep="<br/>", ShapeClick$id, paste0('BCI Value: ',circleData()[circleData()$Point_Name==ShapeClick$id,]$BCI),
                      paste('BCI Category: ', circleData()[circleData()$Point_Name==ShapeClick$id,]$Values) )
            )
        ),
        Ecoregions=addPopups(map=.,lat=ShapeClick$lat, lng=ShapeClick$lng, popup=ShapeClick$id),
        Forested=addPopups(map=.,lat=ShapeClick$lat, lng=ShapeClick$lng, popup=ShapeClick$id),
        EBird=addPopups(map=., lat=ShapeClick$lat, lng=ShapeClick$lng, popup=ShapeClick$howMany)
        
      )}
  })
  
  ### Add additional layers
  withProgress(message="Loading ...  Please Wait",value=1,{
    Ecoregion<-readOGR(dsn="T:/I&M/MONITORING/Forest_Birds/BirdViz/Maps/ecoregion.geojson","OGRGeoJSON")
    Forested<-readOGR(dsn="T:/I&M/MONITORING/Forest_Birds/BirdViz/Maps/Forests.geojson","OGRGeoJSON")
  })
  
  observe({
    
   leafletProxy("BirdMap") %>% {
     switch(input$Layers,
      None=clearGroup(.,group=c("Ecoregions","Forested")) %>% removeControl(.,"LayerLegend"),
          
      Ecoregions=clearGroup(.,group="Forested") %>% 
            addPolygons(.,data=Ecoregion, group="Ecoregions",layerId=Ecoregion$Level3_Nam, stroke=FALSE, 
                              fillOpacity=.65, color=colorFactor("RdYlBu", levels=Ecoregion$Level3_Nam)(Ecoregion$Level3_Nam)),
    
      Forested=clearGroup(.,group="Ecoregions") %>% 
            addPolygons(.,data=Forested, group="Forested", layerId=Forested$MapClass, stroke=FALSE, 
                               fillOpacity=.65, color=colorFactor("Greens",levels=Forested$MapClass)(Forested$MapClass)) 

   )}
  })
  ### Add layer legends
  observe({
  leafletProxy("BirdMap") %>%   removeControl(layerId="LayerLegend") %>%
      {if("Legends" %in% input$MapHide) 
        switch(input$Layers,
          None=NA,
          Ecoregions= addLegend(.,title="Layer Legend",pal=colorFactor("RdYlBu", levels=Ecoregion$Level3_Nam), 
                                     values=Ecoregion$Level3_Nam, layerId="LayerLegend"),
          
          Forested= addLegend(.,title="Layer Legend",pal=colorFactor("Greens",levels=Forested$MapClass), values=Forested$MapClass,
              layerId="LayerLegend")
      )}
  })
  
  ### Get ebird data
  EBirdName<-reactive({getBirdNames(object=NCRN[[1]], names=input$MapSpecies, in.style="AOU", out.style="Latin")})

  EBirdGet<-function(Species,State,Days){           ### Get data from EBird
    fromJSON(url(paste0("http://ebird.org/ws1.1/data/obs/region_spp/recent?rtype=subnational1&r=US-",State,"&sci=",
                        Species,"&back=",Days,"&fmt=json")))
  }
  
  EBirdData<-reactive({ if(input$MapSpecies!="" & input$MapEBird ){
    withProgress(message="DownLoading ...  Please Wait",value=1,{
     rbind(EBirdGet(EBirdName(),"DC",input$MapEBirdDays),EBirdGet(EBirdName(),"MD",input$MapEBirdDays),
           EBirdGet(EBirdName(),"VA",input$MapEBirdDays),EBirdGet(EBirdName(),"WV",input$MapEBirdDays)
      ) %>% filter(!is.na(howMany))
    })
  }})

  ### add EBird cicles
  observe({
   if(input$MapSpecies!="" & input$MapEBird & class(EBirdData())=="data.frame" & input$MapValues=="individual"){
     leafletProxy("BirdMap") %>%  
       clearGroup("EBird") %>% 
       addCircles(data=EBirdData(), layerId=NULL, group="EBird",color=MapColors()(EBirdData()$howMany),fill = FALSE,
                  opacity=0.8, radius=as.numeric(input$PointSize))             
   } else {leafletProxy("BirdMap") %>% clearGroup("EBird")}
  })
  
  #########################################  Data Table Funcitons  ########################################################

  
  ##### Render Controls for Tables and figure out user inputs
  
  # Make Park Control and decide which park is requested
  output$ParkTableSelect<-renderUI({
    selectizeInput(inputId="ParkTable",label="Park", choices=c("All Parks"="All", ParkList ), selected="All" ) 
  })
  
  
  TableParkUse<-reactive({ if (input$ParkTable=="All") NCRN else NCRN[input$ParkTable] })  
  
  ## Make bird species control and figure out name for Captions and Titles
  BirdTableNames<-reactive({
    validate(
      need(input$ParkTable, "Working...")
    )
    BN2<-getChecklist(object = TableParkUse() ) 
    TempNames2<-getBirdNames(object=NCRN[[1]], names = BN2, in.style = "AOU", out.style = input$TableNames)
    TempNames2[is.na(TempNames2)]<-"Needs Name"
    names(BN2)<-TempNames2
    BN2 [order(TempNames2)]
  })
  
  observe({
    updateSelectizeInput(session,inputId="TableSpecies",label="Species", choices=BirdTableNames())
  })

  
  ## Bird name to use for titles and captions
  BirdName<-reactive(getBirdNames(object=NCRN[[1]], names =  input$TableSpecies, 
                                  in.style="AOU", out.style = input$TableNames))
  
  
  ## Band to display in titles and captions
  
  BandOut<-reactive({
    switch(input$TableBand,
           "1"="0-50 meters",
           "2"="0-100 meters",
           All="any distance")
  })
  
  
  ################## Data, Captons, Titles, for the Tables
  
  ### Individual tables, titles, captions, basedata used to calculate other tables
  
  IndividualBase<-reactive({
    CountXVisit(object=NCRN,
                years=input$TableYear, 
                band=if(input$TableBand=="All") NA else seq(as.numeric(input$TableBand)), 
                AOU=input$TableSpecies)
  })
  
  IndividualPoint<-reactive({
    IndividualBase() %>% 
      {if (input$ParkTable!="All") filter(.,Admin_Unit_Code==input$ParkTable) else .} %>% 
      rename("Visit 1"= Visit1, "Visit 2"=Visit2) %>% 
      mutate(Park=factor(getParkNames(object=NCRN[Admin_Unit_Code]) ), "Point Name"=factor(Point_Name) ) %>% 
      dplyr::select(Park, `Point Name`, Year, `Visit 1`,`Visit 2`)
  })
  
  IndividualPark<-reactive({
    IndividualBase() %>% 
    group_by(Admin_Unit_Code) %>% 
    summarize("Mean Visit 1"=round(mean(Visit1, na.rm=T),digits=2), 
             "Mean Visit 2"= round( mean(Visit2, na.rm=T),digits=2)) %>%
    dplyr::select(`Mean Visit 1`,`Mean Visit 2`) %>%
    rbind(c(IndividualBase() %>% 
    summarize("Mean Visit 1"=round(mean(Visit1, na.rm=T),digits=2), 
               "Mean Visit 2"= round(mean(Visit2, na.rm=T),digits=2) ) %>% 
    dplyr::select(`Mean Visit 1`,`Mean Visit 2`) %>% unname()) ) %>% 
    t() %>% "colnames<-"(c(getParkNames(NCRN),"All Parks"))
  })
 
  IndividualPointTitle<-reactive({paste(BirdName()," Data")})
  
  IndividualPointCaption<-reactive({  paste0('Number of ',BirdName(),'s detected at each monitoirng point during ',
    input$TableYear,'. This table includes birds found at ',BandOut(),' from the observer. To see all instances where the bird was
    detected, and not instances where the bird was absent, choose the "Individual Species - All detections" table instead.')
  })
  
  IndividualParkTitle<-reactive({paste("Mean",BirdName()," Detected per Point")})
  
  IndividualParkCaption<-reactive({paste0("Mean number of ",BirdName(),"s detected per monitoring point in each park in ",
                    input$TableYear,". This table includes birds found at ",BandOut()," from the observer.") })
       
  ####  Detects tables, titles, captions, basedata used to calculate other tables
  
  DetectsBase<-reactive({
    CountXVisit(object=NCRN,
                years=2007:2014, 
                band= NA, 
                AOU=input$TableSpecies)
  })
  
  DetectsPoint<-reactive({
    DetectsBase() %>% 
    {if (input$ParkTable!="All") filter(.,Admin_Unit_Code==input$ParkTable) else .} %>% 
      filter(Visit1 >0 | Visit2 >0) %>% 
      {if (nrow(.)==0 ) stop() else .} %>%
      rename("Visit 1"= Visit1, "Visit 2"=Visit2) %>% 
      mutate(Park=factor(getParkNames(object=NCRN[Admin_Unit_Code]) ), "Point Name"=factor(Point_Name) ) %>% 
      dplyr::select(Park, `Point Name`, Year, `Visit 1`,`Visit 2`)
  })
  
  DetectsPark<-reactive({
    DetectsBase() %>%
      group_by(Admin_Unit_Code) %>% 
      summarize("Points where Found"=sum((!is.na(Visit1) &Visit1 > 0) |(!is.na(Visit2) &Visit2>0)),
                "Total Points Monitored"=n()) %>% 
      dplyr::select(.,`Points where Found`,`Total Points Monitored`) %>% 
      rbind(c(DetectsBase() %>% 
                summarize( "Points where Found"=sum((!is.na(Visit1) &Visit1 > 0) |(!is.na(Visit2) &Visit2>0)),
                           "Total Points Monitored"=n()))) %>% 
      t() %>% "colnames<-"(c(getParkNames(NCRN),"All Parks"))
  })
  
  DetectsPointTitle<-reactive({paste(BirdName(),"Detections" )})
  
  DetectsPointCaption<-reactive({  paste0('Detections of ',BirdName(),'s across all distances and years. Each row indicates a monitoring point and year where ',BirdName(),'s were detected during at least one visit. If there were no detections of ', BirdName(),'s at a point during a given year, that point and year combination are not included in this table. For instances when a bird was not detected, choose the "Individual Species - All data from 1 year" table instead.')
  })
  
  DetectsParkTitle<-reactive({paste("Number of Points with", getBirdNames(object=NCRN[[1]], names =  input$TableSpecies, 
                                                      in.style="AOU", out.style = input$TableNames), "Summed Across All Years")})
  
  DetctsParkCaption<-reactive({paste0('"Points where Found" is the number of plots with ',BirdName(),'s detected duing at least one visit in a given year. Each year is treated seprately, so if a bird appears in a plot during 3 different years it will be counted three times. "Total Points Monitored" is the number of points monitored in park, added up over all years.' )})
  
  
  
  ####  Richness tables, titles, captions

  RichnessPoint<-reactive({
    withProgress(message="Calculating...  Please Wait",value=1,{
      getPoints(TableParkUse(),years=input$TableYear) %>% 
        group_by(Point_Name) %>% 
        mutate(Species=birdRichness(TableParkUse(),points=Point_Name, years=input$TableYear)) %>% 
        ungroup() %>%
        mutate(Park=factor(getParkNames(object=NCRN[Admin_Unit_Code]) ), "Point Name"=factor(Point_Name),
               Year=input$TableYear) %>% 
        #rowwise() %>% 
        #mutate(Year=               years=input$TableYear)$Year), collapse="-")) %>% 
        dplyr::select(Park,`Point Name`, Year, Species )
      })
  })
  
  RichnessPark<-reactive({
    data.frame(c(birdRichness(NCRN,years=input$TableYear, output="list"), 
                birdRichness(NCRN,years=input$TableYear))) %>%
    rbind (c(sapply(getPoints(NCRN,years=input$TableYear,output="list"),nrow),
             nrow(getPoints(NCRN,years=input$TableYear)))) %>% 
    "names<-"(c(getParkNames(NCRN),"All Parks")) %>% 
    "row.names<-"(c("Species","Monitoring Points"))
  })
  
  RichnessPointTitle<-reactive({
    paste0("Number of Species Detected per Monitoring Point: ",input$TableYear)})
  
  RichnessPointCaption<-reactive({  paste0("The number of different species found at each monitoring point during ", 
     input$TableYear2[1],". Note that monitoring began later at some points than at others.")
  })
  
  RichnessParkTitle<-reactive({paste0("Number of Species Detected per Park: ",input$TableYear)})
  
  RichnessParkCaption<-reactive({paste0("Number of species detected in the parks in ",input$TableYear,". Parks differ in the number of monitoring points and points differ in the number of years they have been visited, so differences between parks and years may be partially due to differences in sampling.") })
  
  ####  BCI tables, titles, captions, basedata used to calculate other tables
  
  BCIBase<-reactive({
    withProgress(message="Calculating...  Please Wait",value=1,{
     BCI(object=NCRN, years=input$TableYear)
   })
  })    

  
  BCIPoint<-reactive({
    BCIBase() %>% 
      {if (input$ParkTable!="All") filter(.,Admin_Unit_Code==input$ParkTable) else .} %>%
        dplyr::select(Point_Name,BCI,BCI_Category) %>% 
      rename("Point Name"=Point_Name, "BCI Category"=BCI_Category)
  })
  
  BCIPark<-reactive({
    BCIBase() %>% 
    group_by(Admin_Unit_Code) %>% 
    summarize("Mean BCI" = round (mean(BCI), digits=1)) %>%    
    dplyr::select(`Mean BCI`) %>%
    rbind(round(mean(BCIBase()$BCI),digits=1 )) %>% 
    mutate("Park BCI Category"=
      c("Low Integrity", "Medium Integrity","High Integrity","Highest Integrity")[findInterval(`Mean BCI`,
                                vec=c(0,40.1,52.1,60.1,77.1))] ) %>% 
    t() %>% "colnames<-"(c(getParkNames(NCRN),"All Parks") )
  })
  
  BCIPointTitle<-reactive(paste0("Bird Community Index by Point: ",input$TableYear))
  
  BCIPointCaption<-"The Bird Comminity Index (BCI) is an index of ecological integrity (REF). Each point is assinged a BCI score based on the species of birds found. Scores are then assigned to one of four BCI categories: Low, Medium. High or Highest Integiry. These scores are averaged to give a park-wide score. Note that the BCI of individual points may fluctuate from year to year due to random sampling as well as due to changes in the environment."
  
  BCIParkTitle<-reactive({paste0("Bird Community Index by Park: ",input$TableYear) })
  
  BCIParkCaption<-"The Bird Comminity Index (BCI) is an index of ecological integrity (REF). Each park s assinged a BCI score by averaging point scores, which in turn are based on the species of birds found. Scores are then assigned to one of four BCI categories: Low, Medium. High or Highest Integrity."
  
############# Create Tables and Title ouputs
  
### Point Table title:
  
  output$TableTitle<-renderText({
    validate(
      need(IndividualPointTitle(),"Working...")
    )
    switch(input$TableValues,
           individual=IndividualPointTitle(),
           detects=DetectsPointTitle(),
           richness=RichnessPointTitle(),
           bci=BCIPointTitle()
    )
  })
  
  ###Point Table Caption  
  PointTableCaption<-reactive({
    validate(
      need(input$TableYear,"Working..")
    )
    switch(input$TableValues,
            individual=IndividualPointCaption(),
             detects=DetectsPointCaption(),
            richness= RichnessPointCaption(),
            bci=BCIPointCaption #just text, so not reactive.
     )
  })

  ### Park Table title:
  
  output$ParkTableTitle<-renderText({
    validate(
      need(BirdName(),"Working..")
    )
    switch(input$TableValues,
           individual=IndividualParkTitle(),
           detects=DetectsParkTitle(),
           richness=RichnessParkTitle(),
           bci=BCIParkTitle()
    )
  })
  
  ###Park Table Caption  
  ParkTableCaption<-reactive({
    validate(
      need(BirdName(),"Working...")
    )
     switch(input$TableValues,
            individual=IndividualParkCaption(),
            detects=DetctsParkCaption(),
            richness=RichnessParkCaption(),
            bci=BCIParkCaption #just texzt so not reactive
    )
  })
    

  PointTableData<-reactive({
    validate(
      need(BirdName(),".")
    )
    switch(input$TableValues,
               individual=IndividualPoint(),
               detects=DetectsPoint(),
               richness=RichnessPoint(),
               bci=BCIPoint())
  })
  
  ParkTableData<-reactive({
    validate(
      need(IndividualPark(), "Working...")
    )
    switch(input$TableValues,
    individual=IndividualPark(),
    detects=DetectsPark(),
    richness=RichnessPark(),
    bci=BCIPark())
  })
  
  
  output$PointTable<-DT::renderDataTable (datatable(data=PointTableData(),rownames=F,caption=PointTableCaption(), 
                  class="display compact",selection="single"),server=F)
    
    
    
  output$ParkTable<-DT::renderDataTable({
    datatable(data=ParkTableData(), caption=ParkTableCaption(), class="display compact",
                  options=list(dom="t", ordering=FALSE), selection="none")
  })

  
  ## Point Select in the table

  
  ### refocus the map on click and make popup
  observeEvent(
    input$PointTable_rows_selected,{
      RowSelect<-input$PointTable_rows_selected #shorter name for convience
      
      PointSelected<-getPoints(NCRN) %>% 
      group_by(Point_Name) %>% 
      filter(Point_Name==as.character(PointTableData()[[RowSelect,"Point Name"]]))
     
    ### Map Selection updates
      leafletProxy("BirdMap") %>% setView(lng=PointSelected$Longitude,lat=PointSelected$Latitude,zoom=14)
      updateNavbarPage(session,inputId="MainNavBar", selected = "Map")
      
      updateRadioButtons(session, inputId="MapValues",
                         selected=ifelse(input$TableValues=="detects","individual",input$TableValues)) 
      updateSliderInput(session ,inputId="MapYear", 
                        value=ifelse(input$TableValues=="detects",PointTableData()[RowSelect,"Year"],input$TableYear  ))
      updateRadioButtons(session, inputId="MapBand",selected=ifelse(input$TableValues=="individual",input$TableBand,"All"))
      


      updateSelectizeInput(session, inputId="SpeciesValues", 
              selected=ifelse(input$TableValues %in% c("individual","detects"), "Maximum Observed", input$SpeciesValues))
      
      updateRadioButtons(session,inputId="MapNames", selected=input$TableNames)
      
      updateSelectizeInput(session,inputId="MapSpecies", 
                  selected=ifelse(input$TableValues %in% c("individual","detects"), input$TableSpecies, input$MapSpecies))
      
      
      ### move to map
   
      
      ### Make popup
      leafletProxy("BirdMap") %>% clearPopups() %>% 
        addPopups(
          map=.,lat=PointSelected$Latitude, lng=PointSelected$Longitude, 
          popup=switch(input$TableValues, 
            individual=,detects=paste(collapse="<br/>", 
              paste(PointSelected$Point_Name,"<br/>"),
              paste(max(PointTableData()[RowSelect,c("Visit 1","Visit 2")]),"detected", collapse=" ")),
            richness= paste(collapse="<br/>",
                paste(PointSelected$Point_Name,':',PointTableData()[RowSelect,"Species"], 
                                    "Species","<br/>","<br/>",collapse=" "),
                paste(getChecklist(NCRN,points=PointSelected$Point_Name, years=input$MapYear, 
                                   out.style=input$MapNames),collapse="<br/>")) ,
            bci=paste(sep="<br/>", PointSelected$Point_Name, paste0('BCI Value: ',
              PointTableData()[RowSelect,"BCI"]),
              paste('BCI Category: ',PointTableData()[RowSelect,"BCI Category"]) )
          )
        )
  })


  
  #############################  Plots Tab

  
  #   #### Park control for plots
  output$ParkPlotSelect<-renderUI({
    selectInput(inputId="ParkPlot",label="Park", choices=c("All Parks"="All", ParkList ) ) 
  })
  
  PlotParkUse<-reactive({ if (input$ParkPlot=="All") NCRN else NCRN[input$ParkPlot] })
  
  BirdPlotNames<-reactive({
    BN3<-getChecklist(object =NCRN) #, years=input$TableYear, band=1)
    TempNames3<-getBirdNames(object=NCRN[[1]], names=BN3, in.style="AOU", out.style="common")
    TempNames3[is.na(TempNames3)]<-"Needs Name"
    names(BN3)<-TempNames3
    BN3 [order(TempNames3)]
  })
  
  
  
  observe({
    updateSelectizeInput(session,inputId="PlotSpecies",label="Species", choices=BirdPlotNames())
  })
  
  PlotData<-

#     validate(
#       need(input$ParkPlot, "Working...")
#     )
    
    lapply(X=2007:2014, FUN=function(Z){CountXVisit(object=NCRN, years=Z, band=1, AOU="ACFL") %>%   
        summarize("Visit 1"=round(mean(Visit1, na.rm=T),digits=2),
                  "Visit 2"= round( mean(Visit2, na.rm=T),digits=2),Year=Z) %>%
         dplyr::select(Year, `Visit 1`,`Visit 2`) }) %>% 
    lapply(X=., data.frame, stringsAsFactors=FALSE) %>% do.call(rbind,.) %>% 
    gather(key=Visit, value=Mean, Visit.1, Visit.2)
#   
  
#   
  PlotData %>% 
    ggvis() %>% 
    layer_points(x=~Year, y=~Mean, fill=~Visit , size:=200, opacity:=.75) %>% 
    add_tooltip(function(x)paste("Year=",x$Year,"<br/>", " Mean Detected=",x$Mean), on="hover" ) %>% 
    add_axis(type="x",title="Year", format="####") %>%
    add_axis(type="y",title="Mean Detected" ) %>% 
    add_legend(scales="fill", title="Visit") %>% 
    bind_shiny(plot_id="PlotOut")

  
}) #End Shiny Server function
