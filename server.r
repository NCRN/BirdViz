library(shiny)
library(leaflet)
library(NCRNbirds)
library(dplyr)
library(magrittr)
library(rgdal)
library(DT)
library(tidyr)
library(ggvis)
library(shinyjs)
library(jsonlite, pos=100)




BirdData<-switch(Network,
                 ERMN= importERMNbirds("./Data/ERMN_NewProtocol"),
                 GULN= importGULNbirds("./Data/GULN"),
                 MIDN= importMIDNbirds("./Data/MIDN"),
                 NCRN= importNCRNbirds("./Data/NCRN"),
                 NETN= importNETNbirds("./Data/NETN")
)

ParkList<-getParkNames(BirdData, name.class="code")
names(BirdData)<-ParkList
names(ParkList)<-getParkNames(BirdData, name.class="short")

ParkBounds<-read.csv(file="./Data/boundboxes.csv", as.is=TRUE)



shinyServer(function(input,output,session){
  
  #  output$Test<-renderPrint(BoundsUse()[1])
  
  
  
  #### Toggles ####
  observe({
    ### Maps
    toggle( id="SpeciesControls" , condition= (input$MapValues=="individual"))
    toggleState( id='SpeciesValues', condition=( input$MapSpecies!=""))
    toggle(id="EBirdTitle", condition=(input$MapValues=="individual"))
    toggle(id="MapEBird", condition = (input$MapValues=="individual"))
    toggle(id="MapEBirdDays", condition =(input$MapEBird & input$MapValues=="individual")) 
    onclick(id="AboutMap", expr= toggle(id="AboutMapPanel"))
    onclick(id="CloseAboutMap", expr= toggle(id="AboutMapPanel")) 
    
    ### Tables
    toggle(id="TableSpecies", condition=input$TableValues %in% c("individual","detects"))
    toggle(id="TableNames", condition=input$TableValues%in% c("individual","detects"))
    toggle(id="TableYear", condition=input$TableValues %in% c("richness","bci") )
    toggle(id="TableYear2", condition=input$TableValues %in% c("individual") )
    toggle(id="TableZeroHide", condition=input$TableValues %in% c("individual") )
    onclick(id="AboutTables", expr= toggle(id="AboutTablesPanel"))
    onclick(id="CloseAboutTables", expr= toggle(id="AboutTablesPanel")) 
    
    ###Graphs
    toggle(id="PlotSpecies", condition=input$GraphOutputs=="Detects")
    toggle(id="PlotNames", condition=input$GraphOutputs=="Detects")
    onclick(id="AboutGraphs", expr= toggle(id="AboutGraphsPanel"))
    onclick(id="CloseAboutGraphs", expr= toggle(id="AboutGraphsPanel")) 
    
    ### SpeciesList
    toggle(id="PointListSelect", condition=input$SpeciesListType=="Points")
    onclick(id="AboutLists", expr= toggle(id="AboutListsPanel"))
    onclick(id="CloseAboutLists", expr= toggle(id="AboutListsPanel")) 
  })
  
  
  ##### Set up Map ####
  
  output$BirdMap<-renderLeaflet({
    leaflet() %>%
      setView(lng=mean(c(ParkBounds[ParkBounds$ParkCode==Network,]$LongE,ParkBounds[ParkBounds$ParkCode==Network,]$LongW)),
              lat=mean(c(ParkBounds[ParkBounds$ParkCode==Network,]$LatN,ParkBounds[ParkBounds$ParkCode==Network,]$LatS)),
              zoom=8 ) %>%
      setMaxBounds(lng1=ParkBounds[ParkBounds$ParkCode==Network,]$LongE,lng2=ParkBounds[ParkBounds$ParkCode==Network,]$LongW,
                   lat1=ParkBounds[ParkBounds$ParkCode==Network,]$LatN, lat2=ParkBounds[ParkBounds$ParkCode==Network,]$LatS)
  })
  
  
  #### Reactive Map UI Widgets ####
  
  ## Band to use for map
  
  MapBandUse<-reactive({ if(input$MapBand=="All") NA else seq(as.numeric(input$MapBand)) })
  
  ## List of species for map - needs to be named list.
  BirdNames<-reactive({
    BN<-getChecklist(object =  BirdData,
                     #years=input$MapYear,
                     band=MapBandUse())
    TempNames<-getBirdNames(object=BirdData[[1]], names=BN, in.style="AOU", out.style=input$MapNames)
    TempNames[is.na(TempNames)]<-"Needs Name"
    names(BN)<-TempNames
    BN [order(TempNames)]
  })
  
  observe({
    BirdNames()
    isolate(
      updateSelectizeInput(session,inputId="MapSpecies",label="Species", choices=c(BirdNames()),
                           options = list(placeholder='Choose a species'),server = FALSE,
                           selected="OVEN")#if(!input$MapSpecies==""){input$MapSpecies}
    )
  })
  
  #### Make map with Base Layer and Layer Controls ####
  
  NPSAttrib<-HTML("&copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a> 
                  &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors | 
                  <a class='improve-park-tiles' href='http://www.nps.gov/npmap/park-tiles/improve/' 
                  target='_blank'>Improve Park Tiles</a>")
  
  #### Add point to maps - will not appear correctly if layers are added first for some reason - new issue? ####
  observe({
    req(circleData()$Values)
    
    input$Layers
    
    leafletProxy("BirdMap") %>%
      clearGroup("Circles") %>%
      addCircles(data=circleData(), lng=circleData()$Longitude, lat=circleData()$Latitude, layerId=circleData()$Point_Name, group="Circles", color=MapColors()(circleData()$Values),
                 fillColor = MapColors()(circleData()$Values), opacity=0.8, radius=as.numeric(input$PointSize), fillOpacity = 1)
  })
  
  
  observe({
    leafletProxy("BirdMap") %>%
      
      clearTiles() %>%
      
      addTiles(group="Map", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.397cfb9a,nps.3cf3d4ab,nps.b0add3e6/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q", attribution=NPSAttrib, options=tileOptions(minZoom=8)) %>%
      addTiles(group="Imagery",urlTemplate="//{s}.tiles.mapbox.com/v4/nps.2c589204,nps.25abf75b,nps.7531d30a/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",attribution=NPSAttrib, options=tileOptions(minZoom=8)) %>%
      addTiles(group="Slate", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.9e521899,nps.17f575d9,nps.e091bdaf/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",
               attribution=NPSAttrib, options=tileOptions(minZoom=8) ) %>%
      
      addLayersControl(map=., baseGroups=c("Map","Imagery","Slate"), options=layersControlOptions(collapsed=T))
  })
  
  #### Zoom Control for Map ####
  output$ParkZoomControl<-renderUI({
    selectInput(inputId="ParkZoom",label=NULL,selectize=FALSE,
                choices=c("All Parks"=Network,ParkList))
  })
  
  #### Zoom the map ####
  observe({
    req(input$Zoom)
    input$Zoom
    isolate({
      BoundsUse<-reactive({ as.numeric(ParkBounds[ParkBounds$ParkCode==input$ParkZoom,2:5]) })
      leafletProxy("BirdMap") %>% fitBounds(lat1=BoundsUse()[1],lng1=BoundsUse()[2],lat2=BoundsUse()[3],lng2=BoundsUse()[4])
    })
  })
  
  
  #### Circle Data ####
  
  ### based on input$mapvalues, get relevant data and add it to map
  
  circleData<-reactive({
    P<-getPoints(BirdData,years=input$MapYear)
    switch(input$MapValues,
           
           richness={withProgress(message="Calculating...  Please Wait",value=1,
                                  return(P %>% group_by(Point_Name) %>% 
                                           mutate(Values=birdRichness(BirdData,points=Point_Name, 
                                                                      years=input$MapYear,band=MapBandUse()
                                           ))
                                  )
           )},
           
           individual={
             req(MapBandUse() | is.na(MapBandUse()) ) # needed as NA indicates "any distace" here.
             X<-CountXVisit(object=BirdData,years=input$MapYear,AOU=input$MapSpecies, band=MapBandUse() )
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
                             return(P %>% left_join(BCI(object=BirdData, years=input$MapYear,points=P$Point_Name,
                                                        band=MapBandUse()  ) %>% 
                                                      mutate(Values=factor(BCI_Category, 
                                                                           levels=c("Low Integrity","Medium Integrity","High Integrity","Highest Integrity"))) %>% 
                                                      dplyr::select(Point_Name,BCI,Values) )  )
           )}
    )
  })
  
  #### Circle Legends ####
  circleLegend<-reactive({
    switch(input$MapValues,
           richness="# of Species",
           individual={
             req(input$MapSpecies)
             Part1<-paste0(getBirdNames(object=BirdData[[1]], names =  input$MapSpecies, in.style="AOU", 
                                        out.style = input$MapNames), "s<br>Detected")
             Part2<-paste0("<br><svg height='15' width='20'> <circle cx='10' cy='10' r='5', stroke='black' fill='black'/>
                           </svg> NPS Data<br> <svg height='15' width='20'> <circle cx='10' cy='10' r='5' stroke='black' 
                           fill='transparent'/></svg> eBird Data")
             if(input$MapEBird) paste0(Part1, Part2) else Part1},
           bci="Bird Community Index")
  })
  
  #### Color funciton for circles ####
  MapColors<-reactive({
    req(circleData()$Values)
    switch(input$MapValues,
           richness= colorNumeric(palette="viridis",domain=circleData()$Values, reverse = TRUE),
           individual=colorFactor(palette="viridis", domain=0:8,reverse = TRUE),
           bci=colorFactor(palette="viridis", reverse = TRUE,
                           domain=c("Low Integrity","Medium Integrity","High Integrity","Highest Integrity"))
    )
  })  
  
  
  
  #### Figure out values for Map legend ####
  LegendValues<-reactive({
    
    if(!input$MapEBird) unique(circleData()$Values) else{
      TempValues<-unique(c(circleData()$Values,EBirdData()$howMany))  
      TempValues[TempValues>8]<-"9+"
      return(TempValues)
    }
  })
  
  #### Add Legend ####
  observe({
    leafletProxy("BirdMap") %>%
      removeControl(layerId="CircleLegend") %>%
      addLegend(map=., layerId="CircleLegend",pal=MapColors(),
                values=LegendValues(),opacity=1,
                na.label="Not Visited", title=circleLegend(), className="panel panel-default info legend" )
  })
  
  #### User Clicks on map not on a shape - popups close ####
  
  observeEvent(input$BirdMap_click, {
    leafletProxy("BirdMap") %>% 
      clearPopups()
  })
  
  
  
  #### Popup for user hoverin on a circle ####
  observeEvent(input$BirdMap_shape_mouseover, {  
    
    ShapeOver<-input$BirdMap_shape_mouseover
    
    
    leafletProxy("BirdMap") %>% 
      clearPopups() %>% {
        switch(ShapeOver$group,
               Circles= addPopups(map=.,lat=ShapeOver$lat+.001, lng=ShapeOver$lng, layerId="MouseOverPopup",
                                  popup=switch(input$MapValues,
                                               richness=paste0(ShapeOver$id, ': ',circleData()[circleData()$Point_Name==ShapeOver$id,]$Values, " Species"),
                                               
                                               individual=paste(collapse="<br/>", paste(ShapeOver$id,"<br/>"),
                                                                paste(circleData()[circleData()$Point_Name==ShapeOver$id,]$Values,"detected", collapse=" ")),
                                               
                                               bci=paste(sep="<br/>", ShapeOver$id, paste0('BCI Value: ',circleData()[circleData()$Point_Name==ShapeOver$id,]$BCI),
                                                         paste('BCI Category: ', circleData()[circleData()$Point_Name==ShapeOver$id,]$Values) )
                                  )
               ),
               EBird=addPopups(map=., lat=ShapeOver$lat+.001, lng=ShapeOver$lng, layerId="MouseOverPopup",
                               popup=paste0(EBirdData()[ShapeOver$id,"locName"],"<br>", EBirdData()[ShapeOver$id,"howMany"],
                                            " detected<br>",EBirdData()[ShapeOver$id,"obsDt"]))
        )}
  })
  
  #### remove mouse over popups when the mouse leaves ####
  observeEvent(input$BirdMap_shape_mouseout,{
    Sys.sleep(0.1)
    leafletProxy("BirdMap") %>% removePopup(layerId="MouseOverPopup")
  })
  
  #### Popup for user clicking on a shape ####
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
                                                                paste(getChecklist(BirdData,points=ShapeClick$id, years=input$MapYear,band=MapBandUse(), out.style=input$MapNames),collapse="<br/>") ),
                                               
                                               individual=paste(collapse="<br/>", 
                                                                paste(ShapeClick$id,"<br/>"),
                                                                paste(circleData()[circleData()$Point_Name==ShapeClick$id,]$Values,"detected", collapse=" ")),
                                               
                                               bci=paste(sep="<br/>", ShapeClick$id, paste0('BCI Value: ',circleData()[circleData()$Point_Name==ShapeClick$id,]$BCI),
                                                         paste('BCI Category: ', circleData()[circleData()$Point_Name==ShapeClick$id,]$Values) )
                                  )
               ),
               Ecoregions=, Forested=,addPopups(map=.,lat=ShapeClick$lat, lng=ShapeClick$lng, popup=ShapeClick$id),
               #Forested=addPopups(map=.,lat=ShapeClick$lat, lng=ShapeClick$lng, popup=ShapeClick$id),
               EBird=addPopups(map=., lat=ShapeClick$lat, lng=ShapeClick$lng, 
                               popup=paste0(EBirdData()[ShapeClick$id,"locName"],"<br>", EBirdData()[ShapeClick$id,"howMany"],
                                            " detected<br>",EBirdData()[ShapeClick$id,"obsDt"]))
        )}
  })
  
  #### Add additional layers ####
  
  
  withProgress(message="Loading ...  Please Wait",value=1,{
    Ecoregion<-readOGR(dsn="./Maps/Ecoregion.geojson",encoding="OGRGeoJSON")
    Forested<-readOGR(dsn="./Maps/Forests.geojson",encoding="OGRGeoJSON")
  })
  
  observe({
    leafletProxy("BirdMap") %>% {
      switch(input$Layers,
             None=clearGroup(.,group=c("Ecoregions","Forested")) %>% removeControl(.,"LayerLegend"),
             
             Ecoregions=clearGroup(.,group="Forested") %>%
               addPolygons(.,data=Ecoregion, group="Ecoregions",layerId=Ecoregion$MapClass, stroke=FALSE,
                           fillOpacity=.65, color=colorFactor("RdYlBu", levels=Ecoregion$MapClass)(Ecoregion$MapClass)),
             
             Forested=clearGroup(.,group="Ecoregions") %>%
               addPolygons(.,data=Forested, group="Forested", layerId=Forested$MapClass, stroke=FALSE,
                           fillOpacity=.65, color=colorFactor("Greens",levels=Forested$MapClass)(Forested$MapClass))
      )}
  })
  
  #### Add layer legends ####
  observe({
    leafletProxy("BirdMap") %>%   
      removeControl(layerId="LayerLegend") %>%
      {switch(input$Layers,
              None=NA,
              Ecoregions= addLegend(.,title="Layer Legend",pal=colorFactor("RdYlBu", levels=Ecoregion$MapClass),
                                    values=Ecoregion$MapClass, layerId="LayerLegend"),
              Forested= addLegend(.,title="Layer Legend",pal=colorFactor("Greens",levels=Forested$MapClass), values=Forested$MapClass,
                                  layerId="LayerLegend")
      )}
  })
  
  #### Get ebird data ####
  EBirdName<-reactive({getBirdNames(object=BirdData[[1]], names=input$MapSpecies, in.style="AOU", out.style="Latin")})
  
  EBirdGet<-function(Species,State,Days){           ### Get data from EBird
    tryCatch(                                       ### tryCatch is for when our name aren't in ebird (e.g. "unidentified bird")
      fromJSON(URLencode(paste0("http://ebird.org/ws1.1/data/obs/region_spp/recent?rtype=subnational1&r=US-",State,"&sci=",
                                Species,"&back=",Days,"&fmt=json"))),
      error=function(cond){return(list())}
    )
  }
  
  EBirdData<-reactive({ if(input$MapSpecies!="" & input$MapEBird ){
    withProgress(message="Downloading ...  Please Wait",value=1,{
      rbind(EBirdGet(EBirdName(),"DC",input$MapEBirdDays),EBirdGet(EBirdName(),"MD",input$MapEBirdDays),
            EBirdGet(EBirdName(),"VA",input$MapEBirdDays),EBirdGet(EBirdName(),"WV",input$MapEBirdDays)) %>% 
            {if(class(.)=="matrix" ) . else filter(.,!is.na(howMany))}
    })
  }})
  
  
  #### add EBird cicles ####
  observe({ 
    if(input$MapSpecies!="" & input$MapEBird & class(EBirdData() )=="data.frame" & input$MapValues=="individual"){
      leafletProxy("BirdMap") %>%  
        clearGroup("EBird") %>% 
        addCircles(data=EBirdData(), layerId=rownames(EBirdData()), group="EBird",
                   color=MapColors()(EBirdData()$howMany),fillColor = 'white',
                   opacity=1, fillOpacity=0, radius=as.numeric(input$PointSize))             
    } else {leafletProxy("BirdMap") %>% clearGroup("EBird")}
  })
  
  #########################################  Data Table Functions  ########################################################
  
  
  #### Render Controls for Tables and figure out user inputs ####
  
  #### Make Park Control and decide which park is requested ####
  output$ParkTableSelect<-renderUI({
    selectizeInput(inputId="ParkTable",label="Park", choices=c("All Parks"="All", ParkList ), selected="All" ) 
  })
  
  
  TableParkUse<-reactive({ if (input$ParkTable=="All") BirdData else BirdData[input$ParkTable] })  
  
  #### Make bird species control and figure out name for Captions and Titles ####
  BirdTableNames<-reactive({
    validate(
      need(input$ParkTable, "Working...")
    )
    BN2<-getChecklist(object = TableParkUse() ) 
    TempNames2<-getBirdNames(object=BirdData[[1]], names = BN2, in.style = "AOU", out.style = input$TableNames)
    TempNames2[is.na(TempNames2)]<-"Needs Name"
    names(BN2)<-TempNames2
    BN2 [order(TempNames2)]
  })
  
  observe({
    updateSelectizeInput(session,inputId="TableSpecies",label="Species", choices=BirdTableNames())
  })
  
  
  #### Bird name to use for titles and captions ####
  BirdName<-reactive(getBirdNames(object=BirdData[[1]], names =  input$TableSpecies, 
                                  in.style="AOU", out.style = input$TableNames))
  
  
  #### Band to display in titles and captions ####
  
  BandOut<-reactive({
    switch(input$TableBand,
           "1"="0-50 meters",
           "2"="0-100 meters",
           All="any distance")
  })
  
  
  #### Data, Captons, Titles, for the Tables ####
  
  #### Individual tables, titles, captions, basedata used to calculate other tables ####
  
  IndividualBase<-reactive({
    CountXVisit(object=BirdData,
                years=input$TableYear2[1]:input$TableYear2[2], 
                band=if(input$TableBand=="All") NA else seq(as.numeric(input$TableBand)), 
                AOU=input$TableSpecies)
  })
  
  IndividualPoint<-reactive({
    IndividualBase() %>% 
    {if(input$TableZeroHide) filter(.,Visit1 >0 | Visit2 >0) else . } %>% 
    {if (input$ParkTable!="All") filter(.,Admin_Unit_Code==input$ParkTable) else .} %>% 
      rename("Visit 1"= Visit1, "Visit 2"=Visit2) %>% 
      mutate(Park=factor(getParkNames(object=BirdData[Admin_Unit_Code]) ), "Point Name"=factor(Point_Name) ) %>% 
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
      t() %>% "colnames<-"(c(getParkNames(BirdData),"All Parks"))
  })
  
  IndividualPointTitle<-reactive({paste(BirdName()," Data")})
  
  IndividualPointCaption<-reactive({  paste0('Number of ',BirdName(),'s detected at each monitoirng point from ', input$TableYear2[1],' to ', input$TableYear2[2],'. This table includes birds found at ',BandOut(),' from the observer. To see all instances where the bird was detected, and not instances where the bird was absent, choose the "Individual Species - All detections" table instead.')
  })
  
  IndividualParkTitle<-reactive({paste("Mean number of ",BirdName()," Detected per Point")})
  
  IndividualParkCaption<-reactive({paste0("Mean number of ",BirdName(),"s detected per monitoring point in each park from ",
                                          input$TableYear2[1]," to ", input$TableYear2[2],". This is the number of birds detected during a visit divided by the number of monitoring points visited. This table includes birds found at ",BandOut()," from the observer.") })
  
  
  
  ####  Richness tables, titles, captions ####
  
  RichnessPoint<-reactive({
    withProgress(message="Calculating...  Please Wait",value=1,{
      getPoints(TableParkUse(),years=input$TableYear) %>% 
        group_by(Point_Name) %>% 
        mutate(Species=birdRichness(TableParkUse(),points=Point_Name, years=input$TableYear,
                                    band=if(input$TableBand=="All") NA else seq(as.numeric(input$TableBand)))) %>% 
        ungroup() %>%
        mutate(Park=factor(getParkNames(object=BirdData[Admin_Unit_Code])), 
               "Point Name"=factor(Point_Name),
               Year=input$TableYear) %>% 
        dplyr::select(Park,`Point Name`, Year, Species )
    })
  })
  
  RichnessPark<-reactive({
    data.frame(c(birdRichness(BirdData,years=input$TableYear, band=if(input$TableBand=="All") NA else seq(as.numeric(input$TableBand)),output="list"), 
                 birdRichness(BirdData,years=input$TableYear,band=if(input$TableBand=="All") NA else seq(as.numeric(input$TableBand))))) %>%
      rbind (c(sapply(getPoints(BirdData,years=input$TableYear,output="list"),nrow),
               nrow(getPoints(BirdData,years=input$TableYear)))) %>% 
      "names<-"(c(getParkNames(BirdData),"All Parks")) %>% 
      "row.names<-"(c("Species","Monitoring Points"))
  })
  
  RichnessPointTitle<-reactive({
    paste0("Number of Species Detected per Monitoring Point: ",input$TableYear)})
  
  RichnessPointCaption<-reactive({  paste0("The number of different species found at each monitoring point during ", 
                                           input$TableYear," based on detections at ", BandOut()," from the observer. Note that monitoring began later at some points than at others.")
  })
  
  RichnessParkTitle<-reactive({paste0("Number of Species Detected per Park: ",input$TableYear)})
  
  RichnessParkCaption<-reactive({paste0("Number of species detected in the parks in ",input$TableYear," based on detections at ", BandOut()," from the observer. Parks differ in the number of monitoring points and points differ in the number of years they have been visited, so differences between parks and years may be partially due to differences in sampling.") })
  
  
  ####  BCI tables, titles, captions, basedata used to calculate other tables  ####
  
  BCIBase<-reactive({
    withProgress(message="Calculating...  Please Wait",value=1,{
      BCI(object=BirdData, years=input$TableYear,band=if(input$TableBand=="All") NA else seq(as.numeric(input$TableBand)))
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
      t() %>% "colnames<-"(c(getParkNames(BirdData),"All Parks") )
  })
  
  BCIPointTitle<-reactive(paste0("Bird Community Index by Point: ",input$TableYear))
  
  BCIPointCaption<-reactive({paste0("The Bird Comminity Index (BCI) for each monitoring point during ", input$TableYear," at ", BandOut()," from the observer. Note that monitoring began later at some points than at others. The BCI  is an index of ecological integrity (O'Connell et al., 1998, 2000). Each point is assinged a BCI score based on the species of birds found. Scores are then assigned to one of four BCI categories: Low, Medium. High or Highest Integiry. These scores are averaged to give a park-wide score. Note that the BCI of individual points may fluctuate from year to year due to random sampling as well as due to changes in the environment.") })
  
  BCIParkTitle<-reactive({paste0("Bird Community Index by Park: ",input$TableYear) })
  
  BCIParkCaption<-reactive({  paste0("The Bird Comminity Index (BCI) for each park during.", input$TableYear," based on deteections at ", BandOut()," from the observer. The BCI  is an index of ecological integrity (O'Connell et al., 1998, 2000). Each park s assinged a BCI score by averaging point scores, which in turn are based on the species of birds found. Scores are then assigned to one of four BCI categories: Low, Medium. High or Highest Integrity.")})
  
  ####Create Tables and Title outputs ####
  
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
           bci=BCIPointCaption() 
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
           bci=BCIParkCaption()
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
  
  
  output$PointTable<-DT::renderDataTable( 
    datatable(
      data=PointTableData(),rownames=F,caption=PointTableCaption(), 
      class="display compact",selection="single", #extensions="TableTools", 
      options=list(dom = 'T<"clear">lfrtip'
                   #, tableTools=list(sSwfPath=copySWF('www',pdf=TRUE),aButtons=list('copy','print','csv','pdf'))
      )
    ),
    server=F)
  
  
  
  output$ParkTable<-DT::renderDataTable({
    datatable(
      data=ParkTableData(), caption=ParkTableCaption(), class="display compact", #extensions = 'TableTools',
      options=list(dom='T<"clear">t', ordering=FALSE
                   #, tableTools=list(sSwfPath=copySWF('.',pdf=TRUE),aButtons=list('copy','print','csv','pdf'))
      ), 
      selection="none")
  })
  
  
  
  #### Point Select in the table ####
  
  
  #### refocus the map on click and make popup ####
  observeEvent(
    input$PointTable_rows_selected,{
      RowSelect<-input$PointTable_rows_selected #shorter name for convience
      
      PointSelected<-getPoints(BirdData) %>% 
        group_by(Point_Name) %>% 
        filter(Point_Name==as.character(PointTableData()[[RowSelect,"Point Name"]]))
      
      #### Map Selection updates ####
      leafletProxy("BirdMap") %>% setView(lng=PointSelected$Longitude,lat=PointSelected$Latitude,zoom=14)
      updateNavbarPage(session,inputId="MainNavBar", selected = "Map")
      
      updateRadioButtons(session, inputId="MapValues",
                         selected=ifelse(input$TableValues=="detects","individual",input$TableValues)) 
      updateSliderInput(session ,inputId="MapYear", 
                        value=ifelse(input$TableValues %in% c("individual","detects"),PointTableData()[RowSelect,"Year"],input$TableYear  ))
      updateRadioButtons(session, inputId="MapBand",selected=ifelse(input$TableValues=="individual",input$TableBand,"All"))
      
      
      
      updateSelectizeInput(session, inputId="SpeciesValues", 
                           selected=ifelse(input$TableValues %in% c("individual","detects"), "Maximum Observed", input$SpeciesValues))
      
      updateRadioButtons(session,inputId="MapNames", selected=input$TableNames)
      
      updateSelectizeInput(session,inputId="MapSpecies", 
                           selected=ifelse(input$TableValues %in% c("individual","detects"), input$TableSpecies, input$MapSpecies))
      
      
      #### move to map ####
      
      
      #### Make popup ####
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
                                       paste(getChecklist(BirdData,points=PointSelected$Point_Name, years=input$MapYear, 
                                                          out.style=input$MapNames),collapse="<br/>")) ,
                       bci=paste(sep="<br/>", PointSelected$Point_Name, paste0('BCI Value: ',
                                                                               PointTableData()[RowSelect,"BCI"]),
                                 paste('BCI Category: ',PointTableData()[RowSelect,"BCI Category"]) )
          )
        )
    })
  
  
  
  
  
  ####  Graphs Tab ####
  
  #### Park control for graphs ####
  output$ParkPlotSelect<-renderUI({
    selectizeInput(inputId="ParkPlot",label="Park:", choices=c("All Parks"="All", ParkList), selected="All" ) 
  })
  
  PlotParkUse<-reactive({  if (input$ParkPlot=="All") BirdData else BirdData[[input$ParkPlot]] })
  
  
  BirdPlotNames<-reactive({
    BN3<-getChecklist(object =BirdData) #, years=input$TableYear, band=1)
    TempNames3<-getBirdNames(object=BirdData[[1]], names=BN3, in.style="AOU", out.style=input$PlotNames)
    TempNames3[is.na(TempNames3)]<-"Needs Name"
    names(BN3)<-TempNames3
    BN3 [order(TempNames3)]
  })
  
  
  
  observe({
    updateSelectizeInput(session,inputId="PlotSpecies",label="Species:", choices=BirdPlotNames())
  })
  
  
  PlotBandOut<-reactive({
    switch(input$PlotBand,
           "1"="0-50 meters",
           "2"="0-100 meters",
           All="any distance")
  })
  
  
  DetectsPlotData<-reactive({
    if(!is.null(input$ParkPlot)){
      CountXVisit(object=BirdData, 
                  band=if(input$PlotBand=="All") NA else seq(as.numeric(input$PlotBand)), 
                  AOU=input$PlotSpecies) %>% 
                  { if (input$ParkPlot=="All") . else filter(.,Admin_Unit_Code==input$ParkPlot)} %>% 
        mutate(Year=factor(Year,labels=paste(paste(c(Years$Start:Years$End), paste0("(",
                                                                                    sapply(X=Years$Start:Years$End,Y=PlotParkUse(),FUN=function(X,Y){nrow(getPoints(Y,years=X))}),")"))))) %>% 
        group_by(Year) %>% 
        
        summarize("Visit 1"=round(mean(Visit1, na.rm=T),digits=2), "Visit 2"= round( mean(Visit2, na.rm=T),digits=2)) %>% 
        gather(key=Visit, value=Mean, `Visit 1`, `Visit 2`)  
      
    }
  })
  
  
  RichnessPlotData<-reactive({
    if(!is.null(input$ParkPlot)){
      tbl_df(data.frame(Year=Years$Start:Years$End)) %>% 
        group_by(Year) %>% 
        mutate(Species=birdRichness(object=PlotParkUse(), years=Year,
                                    band=if(input$PlotBand=="All") NA else seq(as.numeric(input$PlotBand))) ) %>% 
        ungroup() %>% 
        mutate(Year=factor(Year,labels=paste(paste(c(Years$Start:Years$End), paste0("(",
            sapply(X=Years$Start:Years$End,Y=PlotParkUse(),FUN=function(X,Y){nrow(getPoints(Y,years=X))}),")"))))) 
    }
  })   
  
  BCIPlotData<-reactive({
    withProgress(message="Calculating...  Please Wait",value=1,{
      tbl_df(data.frame(Year=Years$Start:Years$End)) %>% 
        group_by(Year) %>% 
        mutate(BCI=BCI(object=PlotParkUse(), years=Year,
                       band=if(input$PlotBand=="All") NA else seq(as.numeric(input$PlotBand)))[["BCI"]] %>% mean(na.rm=T) %>% 
                 round(digits=1),
               "BCI Category"=c("Low Integrity", "Medium Integrity","High Integrity","Highest Integrity")[findInterval(BCI,
                       vec=c(0,40.1,52.1,60.1,77.1))]
        ) %>% 
        ungroup() %>% 
        mutate(Year=factor(Year,labels=paste(paste(c(Years$Start:Years$End), paste0("(",
                 sapply(X=Years$Start:Years$End,Y=PlotParkUse(),FUN=function(X,Y){nrow(getPoints(Y,years=X))}),")")))))
    })
  })
  
  #### Bird name to use for titles and captions ####
  PlotBirdName<-reactive(getBirdNames(object=BirdData[[1]], names =  input$PlotSpecies, 
                                      in.style="AOU", out.style = input$PlotNames))
  PlotParkName<-reactive(if (input$ParkPlot=="All") paste("All",Network,"Parks") else getParkNames(PlotParkUse(),"short" ))
  
  
  PlotDetectTitle<-reactive({paste0(PlotBirdName(),"s Detected per Point in ",PlotParkName() )})
  
  PlotRichnessTitle<-reactive({paste0("Number of Species Detected in ", PlotParkName())})
  
  PlotBCITitle<-reactive({paste0("Bird Community Index for ",PlotParkName())})
  
  
  PlotDetectCaption<-reactive({paste0("Average (mean) number of ",PlotBirdName()," detected per point in ",PlotParkName(), " at ", PlotBandOut(),". The horizontal axis indicates the year, with the number of points monitored in parenthesis. The vertical axis indicates the number of birds deteced divided by the number of points monitored. Additional birds were likely present at the points, but not detected.")})
  
  PlotRichnessCaption<-reactive({paste0("Number of bird species detected during monitoring in ",PlotParkName()," at ",PlotBandOut(),
                                        ". The horizontal axis indicates the year, with the number of points monitored in parenthesis. The vertical axis indicates the number of birds species detected during monitoring. Additional bird species were likely present at the points, but not detected.")})
  
  PlotBCICaption<-reactive({paste0("Bird Community Index (BCI) for ",PlotParkName()," at ", PlotBandOut(),
                                   ". The horizontal axis indicates the year, with the number of points monitored in parenthesis. The vertical axis shows the BCI, which indicates the conservation status of the bird community. The points on the graph are the average (mean) BCI for all points monitored during each year.")})
  
  
  observe({
    if(!is.null(input$ParkPlot) & input$GraphOutputs=="Detects"){
      DetectsPlotData %>% 
        ggvis() %>% 
        layer_points(x=~Year, y=~Mean, fill= ~Visit , size:=200, opacity:=.66) %>% 
        add_tooltip(function(x)paste("Year=",x$Year,"<br/>", " Mean Detected =",x$Mean), on="hover" ) %>% 
        add_axis(type="x",title="Year",  properties=axis_props(title=list(fontSize=14))) %>% #format="####",
        #scale_numeric("x", domain=c(2007,2016))%>% 
        add_axis(type="y",title="Mean # Birds Detected per Point", title_offset=45, 
                 properties=axis_props(title=list(fontSize=14))) %>% 
                 {if(max(DetectsPlotData()$Mean)==0) scale_numeric(.,"y",domain=c(0,.5)) else .} %>% 
        add_legend(scales="fill", title="Visit", properties=legend_props(title=list(fontSize=16), labels=list(fontSize=16))) %>% 
        add_axis("x", orient="top",ticks=0, title=PlotDetectTitle(),  # Annoying hack for plot title
                 properties=axis_props(axis=list(stroke="white"), ticks=list(stroke="white"),
                                       labels=list(fontSize=0),title=list(fontSize=24) )) %>% 
        set_options(width="750px", resizable="TRUE") %>% 
        bind_shiny("DetectsPlot")
    }
  })
  
  output$DetectsCaption<-renderText({
    validate(
      need( input$PlotSpecies ," "),
      need( input$ParkPlot , " "),
      need( input$PlotBand," ")
    )
    PlotDetectCaption()
  })
  
  observe({
    if(!is.null(input$ParkPlot) & input$GraphOutputs=="Richness"){
      RichnessPlotData %>% 
        ggvis() %>% 
        layer_points(x=~Year, y=~Species, size:=200, fill="blue", opacity:=.75) %>% 
        add_tooltip(function(x)paste("Year=",x$Year,"<br/>", "Species=",x$Species), on="hover" ) %>% 
        add_axis(type="x",title="Year" ) %>%   #,format="####"
        #scale_numeric("x", domain=c(2007,2016)) %>% 
        add_axis(type="y",title="Species Detected") %>% 
        scale_numeric("y",domain=c(0,150) )%>% 
        hide_legend(scales="fill" ) %>% 
        add_axis("x", orient="top",ticks=0, title=PlotRichnessTitle(),  # Annoying hack for plot title
                 properties=axis_props(axis=list(stroke="white"), tick=list(stroke="white"), 
                                       labels=list(fontSize=0),title=list(fontSize=24) )) %>% 
        set_options(width="750px", resizable="TRUE") %>% 
        bind_shiny("RichnessPlot")
    }
  })
  
  output$RichnessCaption<-renderText({
    validate(
      need( input$PlotSpecies ," "),
      need( input$ParkPlot , " "),
      need( input$PlotBand," ")
    )
    PlotRichnessCaption()
  })
  
  observe({
    if(!is.null(input$ParkPlot) & input$GraphOutputs=="BCI"){  
      BCIPlotData %>% 
        ggvis(x=~Year) %>% 
        layer_ribbons(y=0, y2 = 40, fill:="red", opacity:=.20) %>%
        layer_ribbons(y=40, y2 = 52, fill:="orange", opacity:=.20) %>%
        layer_ribbons(y=52, y2 = 60, fill:="yellow", opacity:=.20) %>%
        layer_ribbons(y= 60, y2 = 80, fill:="green", opacity:=.20) %>%
        layer_points(x=~Year,y=~BCI, fill=~`BCI Category`, size:=200, opacity:=.75,stroke="black") %>%
        scale_nominal("fill", range=c("red","orange","yellow","green"),
                      domain=c("Low Integrity", "Medium Integrity","High Integrity","Highest Integrity")) %>% 
        add_tooltip(function(x)paste("Year=",x$Year,"<br/>", "BCI=",x$BCI, "<br/>", x[["BCI Category"]]), on="hover" ) %>% 
        add_axis(type="x",title="Year") %>% #, format="####"
        #scale_numeric("x", domain=c(2007,2016),expand=0.008) %>% 
        add_axis(type="y",title="Bird Community Index (BCI)") %>% 
        scale_numeric("y",domain=c(0,80),expand=0) %>%
        add_legend("fill") %>% 
        hide_legend("stroke") %>% 
        add_axis("x", orient="top",ticks=0, title=PlotBCITitle(),  # Annoying hack for plot title
                 properties=axis_props(axis=list(stroke="white"), labels=list(fontSize=0),title=list(fontSize=24) )) %>% 
        set_options(width="750px", resizable="TRUE") %>% 
        bind_shiny("BCIPlot")
    }
  })
  
  output$BCICaption<-renderText({
    validate(
      need( input$PlotSpecies ," "),
      need( input$ParkPlot , " "),
      need( input$PlotBand," ")
    )
    PlotBCICaption()
  }) 
  
  
  #### Species Lists ####
  
  
  #### Park control for species list ####
  output$ParkListSelect<-renderUI({
    selectizeInput(inputId="ParkList",label="Park", choices=ParkList ) 
  })
  
  ListParkUse<-reactive({  if (input$ParkList=="All") BirdData else BirdData[[input$ParkList]] })
  
  output$PointListSelect <-renderUI({
    validate(
      need(input$ParkList, message="Please select a park.")
    )
    selectizeInput(inputId="ListPointsUse", choices=c("All Points"="All", getPoints(ListParkUse())$Point_Name),
                   label="Points (optional)", multiple=TRUE, selected="All"
    )
  })
  
  ListPoints<-reactive({
    validate(
      need(input$ListPointsUse, "Please select monitoring points.")
    )
    if("All" %in% input$ListPointsUse){NA}else{input$ListPointsUse}
  })
  
  
  #### Get species lists from IRMA / NPSpecies ####
  
  NPSpeciesURL<-reactive({
    validate(
      need(input$ParkList,"Choose a Park")
    )
    paste0("http://irmaservices.nps.gov/v3/rest/npspecies/checklist/",input$ParkList,"/bird?format=Json")
  })
  
  NPSpeciesList<-reactive({
    fromJSON(NPSpeciesURL())%>% 
      dplyr::select(CommonNames, ScientificName,Occurrence) %>% 
      arrange(CommonNames) %>% 
      rename('Latin Name'=ScientificName, 'Common Name'=CommonNames)
  })
  
  MonitoringList<-reactive({
    validate(
      need(input$ParkList, "Please select a park.")
    )
    tbl_df(data.frame( 'Latin.Name'= getChecklist(object=BirdData[[input$ParkList]], points=ListPoints(),out.style="Latin"))) %>% 
      mutate('Common Name' = getBirdNames(object=BirdData[input$ParkList], names=Latin.Name, in.style="Latin",out.style = "common")) %>% 
      rename('Latin Name'= Latin.Name) %>% arrange(`Common Name`) %>%  .[,c(2,1)]
  })
  
  
  output$SpeciesList<-DT::renderDataTable(server=FALSE,
                                          datatable(rownames=F,caption="Species List", class="display compact",selection="single",
                                                    data=switch(input$SpeciesListType,
                                                                All=NPSpeciesList(),
                                                                Points=MonitoringList()
                                                    )
                                          )
  )
  }) #End Shiny Server function