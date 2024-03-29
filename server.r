library(shiny)
library(leaflet)
library(NCRNbirds)
library(dplyr)
library(magrittr)
library(purrr)
library(rgdal)
library(DT)
library(tidyr)
library(shinyjs)
library(htmlwidgets)
library(Hmisc)
library(jsonlite, pos=100)


BirdData<-switch(Network,
                 ERMN= importERMNbirds("./Data/ERMN"),
                 ERMN2007= importERMNbirds2007("./Data/ERMN2007"),
                 GULN= importGULNbirds("./Data/GULN"),
                 MIDN= importMIDNbirds("./Data/MIDN"),
                 NCRN= importNCRNbirds("./Data/NCRN"),
                 NETN= importNETNbirds("./Data/NETN"),
                 NCBN= importNCBNbirds("./Data/NCBN")
)

ParkList<-getParkNames(BirdData, name.class="code")
names(BirdData)<-ParkList
names(ParkList)<-getParkNames(BirdData, name.class="short")

ParkBounds<-read.csv(file="./Data/boundboxes.csv", as.is=TRUE)

shinyServer(function(input,output,session){
  

  
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
  
  
  #### ReactiveValues
  
  RV<-reactiveValues(Map=F, Table=F, Plot=F)
  
  #### Map Panel ####
  
  #### Reactive Map UI Widgets ####
  ## Visits
  
  output$MapVisitSelect<-renderUI({
    radioButtons(inputId="MapVisit",label="Visit:", choices=c("All",
      as.character(seq(as.numeric(getDesign(BirdData, info= "visits")[[1]],1)))), selected="All",inline=TRUE ) 
  })
  

  ##Bands
  BandsAvailable<-getDesign(BirdData, info="bands")[[1]]
  BandChoices<-BandsAvailable$Band
  names(BandChoices)<-paste0("0-",BandsAvailable$MaxDistance, " meters")
  
  output$MapBandSelect<-renderUI({
    radioButtons(inputId = "MapBand", label="Distance from Observer", choices=BandChoices, inline=T)
    
  })
  
  
  
  
  ## Values to use for map
  
  MapBandUse<-reactive({seq(as.numeric(input$MapBand)) })# if(input$MapBand=="All") NA else seq(as.numeric(input$MapBand)) })
  MapVisitUse<-reactive({if(input$MapVisit=="All") NA else as.numeric(input$MapVisit)})
  
  ## List of species for map - needs to be named list.
  BirdNames<-reactive({
    BN<-getChecklist(object =  BirdData, band=MapBandUse())
    TempNames<-getBirdNames(object=BirdData[[1]], names=BN, in.style="AOU", out.style=input$MapNames)
    TempNames[is.na(TempNames)]<-"Needs Name"
    names(BN)<-TempNames
    BN [order(TempNames)]
  })
  observeEvent(input$MapSpecies, RV$Map<-TRUE, ignoreInit = T)
  
  observeEvent(BirdNames(), 
               handlerExpr = {
                 updateSelectizeInput(session,inputId="MapSpecies",label="Species", 
                                      choices=BirdNames(), options = list(placeholder='Choose a species'),
                                      selected=if(RV$Map) input$MapSpecies else BirdNames()[1])
               }
               
  )

  

  #### Zoom Control for Map ####
  output$ParkZoomControl<-renderUI({
    selectInput(inputId="ParkZoom",label=NULL,selectize=FALSE,
                choices=c("All Parks"=Network,ParkList))
  })
  ## Zoom radius
  BandZoomChoices<-BandsAvailable$MaxDistance
  names(BandZoomChoices)<-paste0(BandsAvailable$MaxDistance, " meters")
  
  output$MapBandZoomSelect<-renderUI({
    radioButtons(inputId = "PointSize", label="PointSize", choices=BandZoomChoices, inline=T)
    
  })
  
  ##### Set up Map ####
  
  output$BirdMap<-renderLeaflet({
    leaflet(BirdData) %>%
      setView(lng=mean(c(ParkBounds[ParkBounds$ParkCode==Network,]$LongE,ParkBounds[ParkBounds$ParkCode==Network,]$LongW)),
              lat=mean(c(ParkBounds[ParkBounds$ParkCode==Network,]$LatN,ParkBounds[ParkBounds$ParkCode==Network,]$LatS)),
              zoom=8 ) %>%
      setMaxBounds(lng1=ParkBounds[ParkBounds$ParkCode==Network,]$LongE,lng2=ParkBounds[ParkBounds$ParkCode==Network,]$LongW,
                   lat1=ParkBounds[ParkBounds$ParkCode==Network,]$LatN, lat2=ParkBounds[ParkBounds$ParkCode==Network,]$LatS)
  })
  

  
  #### Make map with Base Layer and Layer Controls ####
  
  NPSAttrib<-HTML("&copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a> 
                  &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors | 
                  <a class='improve-park-tiles' href='http://www.nps.gov/npmap/park-tiles/improve/' 
                  target='_blank'>Improve Park Tiles</a>")
  
  NPSbasic<-"https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  
  NPSimagery="https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck72fwp2642dv07o7tbqinvz4/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  
  NPSslate="https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  
  NPSlight="https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpia2u0auf01p9vbugvcpv/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  
  
  
  ## Add tiles
  observe({
    leafletProxy("BirdMap") %>%
      
      clearTiles() %>%
      
      addTiles(group="Map", urlTemplate=NPSbasic, attribution=NPSAttrib, options=tileOptions(minZoom=8)) %>%
      addTiles(group="Imagery",urlTemplate=NPSimagery, attribution=NPSAttrib, options=tileOptions(minZoom=8)) %>%
      addTiles(group="Slate", urlTemplate=NPSslate, attribution=NPSAttrib, options=tileOptions(minZoom=8) ) %>%
      addTiles(group="Light", urlTemplate=NPSlight, attribution=NPSAttrib, options=tileOptions(minZoom=8) ) %>%
      addLayersControl(map=., baseGroups=c("Map","Imagery","Light","Slate"), options=layersControlOptions(collapsed=T))
  })
  
  ## add circles
  observe({
    req(circleData()$Values)
    
    input$Layers
    
    leafletProxy("BirdMap") %>%
      clearGroup("Circles") %>%
      addCircles(data=circleData(), lng=circleData()$Longitude, lat=circleData()$Latitude, layerId=circleData()$Point_Name, group="Circles", color=MapColors()(circleData()$Values),
                 fillColor = MapColors()(circleData()$Values), opacity=0.8, radius=as.numeric(input$PointSize), fillOpacity = 1)
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
    req(input$MapValues, input$MapVisit)
    req(MapBandUse() | is.na(MapBandUse()) ) # needed as NA indicates "any distace" here.
    P<-getPoints(BirdData,years=input$MapYear)
    switch(input$MapValues,
           
           richness= return(P %>% left_join(birdRichness(BirdData, years=input$MapYear, band=MapBandUse(),
                                                    visits=MapVisitUse(), byPoint=T) %>% rename(Values=Richness))

           ),
           
           individual={
             X<-CountXVisit(object=BirdData,years=input$MapYear,AOU=input$MapSpecies, band=MapBandUse(), visits=MapVisitUse(), 
                                                                                              max=(input$MapVisit=="All") )
             if(input$MapVisit=="All") return(P %>% left_join(X %>% dplyr::select(Point_Name,Max) %>% rename(Values=Max))) else{
               return(P %>% left_join(X %>% dplyr::select(Point_Name,starts_with("Visit")) %>% rename(Values=2) ))
             }
           },
           bci={withProgress(message="Calculating...  Please Wait",value=1,
              return(P %>% left_join(BCI(object=BirdData, years=input$MapYear,points=P$Point_Name, band=MapBandUse(), 
                              visits=MapVisitUse(),type= {if(Network == "NETN") "NETN_Forest_BCI" else "Cent_Appal"}) %>% 
                arrange(BCI_Category) %>% 
                rename(Values=BCI_Category) %>% 
                mutate(Values=factor(Values, levels=c("Low Integrity","Medium Integrity","High Integrity","Highest Integrity"))) %>% 
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
               paste0(getBirdNames(object=BirdData[[1]], names =  input$MapSpecies, in.style="AOU", 
                                        out.style = input$MapNames), "s<br>Detected") },
           bci="Bird Community Index")
  })
  
  #### Color funciton for circles ####
  MapColors<-reactive({
    req(circleData()$Values)
    switch(input$MapValues,
           richness= colorNumeric(palette="viridis",domain=circleData()$Values, reverse = TRUE),
           individual=colorFactor(palette="viridis", domain=0:8,reverse = TRUE),
           bci=colorFactor(palette="BuGn",ordered = TRUE,
                           domain=c("Low Integrity","Medium Integrity","High Integrity","Highest Integrity"))
    )
  })  
  
  
  #### Figure out values for Map legend ####
  LegendValues<-reactive({
    unique(circleData()$Values)
  })
  
  #### Add Legend ####
  
  observe({
    #due to poor placement of "Not Visited" in legend, NA values are filtered here - revisit when that RLeaflet bug is fixed.
    leafletProxy("BirdMap") %>%
      removeControl(layerId="CircleLegend") %>%
      addLegend(map=., layerId="CircleLegend",pal=MapColors(),
                values=LegendValues()[(!is.na(LegendValues()))],opacity=1,na.label="Not Visited", 
                title=circleLegend(), className="panel panel-default info legend" )
  })
  
  #### Popup for user hovering on a circle ####
  observeEvent(input$BirdMap_shape_mouseover, {

    ShapeOver<-input$BirdMap_shape_mouseover


    leafletProxy("BirdMap") %>% {
     # clearPopups() %>% {
        switch(ShapeOver$group,
               Circles= addPopups(map=.,lat=ShapeOver$lat+.001, lng=ShapeOver$lng, layerId="MouseOverPopup",
                  popup=switch(input$MapValues,
                    richness=paste0(ShapeOver$id, ': ',circleData()[circleData()$Point_Name==ShapeOver$id,]$Values, " Species"),

                    individual=paste(collapse="<br/>", paste(ShapeOver$id,"<br/>"),
                      paste(circleData()[circleData()$Point_Name==ShapeOver$id,]$Values,"detected", collapse=" ")),

                    bci=paste(sep="<br/>", ShapeOver$id, paste0('BCI Value: ',circleData()[circleData()$Point_Name==ShapeOver$id,]$BCI),
                      paste('BCI Category: ', circleData()[circleData()$Point_Name==ShapeOver$id,]$Values) )
                    )
               )
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
                paste(ShapeClick$id, ':',circleData()[circleData()$Point_Name==ShapeClick$id,]$Values, "Species","<br/>","<br/>",collapse=" "),
                paste(getChecklist(BirdData,points=ShapeClick$id, years=input$MapYear,band=MapBandUse(),visit=MapVisitUse(), 
                                   out.style=input$MapNames),collapse="<br/>") ),
                                                 
                individual=paste(collapse="<br/>", 
                  paste(ShapeClick$id,"<br/>"),
                  paste(circleData()[circleData()$Point_Name==ShapeClick$id,]$Values,"detected", collapse=" ")),
                                               
                  bci=paste(sep="<br/>", ShapeClick$id,paste0('BCI Value: ',circleData()[circleData()$Point_Name==ShapeClick$id,]$BCI),
                     paste('BCI Category: ', circleData()[circleData()$Point_Name==ShapeClick$id,]$Values) )
              )
            )#,
            #Ecoregions=, Forested=,addPopups(map=.,lat=ShapeClick$lat, lng=ShapeClick$lng, popup=ShapeClick$id)
    )}
  })
  
  
  #### Add additional layers ####
  # withProgress(message="Loading ...  Please Wait",value=1,{
  #   Ecoregion<-readOGR(dsn="./Maps/Ecoregion.geojson",encoding="OGRGeoJSON")
  #   Forested<-readOGR(dsn="./Maps/Forests.geojson",encoding="OGRGeoJSON")
  # })
  # 
  # observe({
  #   leafletProxy("BirdMap") %>% {
  #     switch(input$Layers,
  #            None=clearGroup(.,group=c("Ecoregions","Forested")) %>% removeControl(.,"LayerLegend"),
  #            
  #            Ecoregions=clearGroup(.,group="Forested") %>%
  #              addPolygons(.,data=Ecoregion, group="Ecoregions",layerId=Ecoregion$MapClass, stroke=FALSE,
  #                          fillOpacity=.65, color=colorFactor("RdYlBu", levels=Ecoregion$MapClass)(Ecoregion$MapClass)),
  #            
  #            Forested=clearGroup(.,group="Ecoregions") %>%
  #              addPolygons(.,data=Forested, group="Forested", layerId=Forested$MapClass, stroke=FALSE,
  #                          fillOpacity=.65, color=colorFactor("Greens",levels=Forested$MapClass)(Forested$MapClass))
  #     )}
  # })
  
  #### Add layer legends ####
  # observe({
  #   leafletProxy("BirdMap") %>%   
  #     removeControl(layerId="LayerLegend") %>%
  #     {switch(input$Layers,
  #             None=NA,
  #             Ecoregions= addLegend(.,title="Layer Legend",pal=colorFactor("RdYlBu", levels=Ecoregion$MapClass),
  #                                   values=Ecoregion$MapClass, layerId="LayerLegend"),
  #             Forested= addLegend(.,title="Layer Legend",pal=colorFactor("Greens",levels=Forested$MapClass), values=Forested$MapClass,
  #                                 layerId="LayerLegend")
  #     )}
  # })

  #########################################  Data Table Functions  ########################################################
  
  
  #### Render Controls for Tables and figure out user inputs ####
  
  #### Make Park Control and decide which park is requested ####
  output$ParkTableSelect<-renderUI({
    selectizeInput(inputId="ParkTable",label="Park", choices=c("All Parks"="All", ParkList ), selected="All" ) 
  })
  
  
  TableParkUse<-reactive({ req(input$ParkTable)
    if (input$ParkTable=="All") BirdData else BirdData[input$ParkTable] })  
  
  
  output$TableBandSelect<-renderUI({
    radioButtons(inputId = "TableBand", label="Distance from Observer", choices=BandChoices, inline=T)
    
  })
  TableBandUse<-reactive({seq(as.numeric(input$TableBand)) })
  
  #### Make bird species control and figure out name for Captions and Titles ####
  BirdTableNames<-reactive({
    req(input$ParkTable)
    BN2<-getChecklist(object = TableParkUse() ) 
    TempNames2<-getBirdNames(object=BirdData[[1]], names = BN2, in.style = "AOU", out.style = input$TableNames)
    TempNames2[is.na(TempNames2)]<-"Needs Name"
    names(BN2)<-TempNames2
    BN2 [order(TempNames2)]
  })

  observeEvent(input$TableSpecies, RV$Table<-TRUE, ignoreInit = T)
  
  observeEvent(BirdTableNames(), 
    handlerExpr = {
      updateSelectizeInput(session,inputId="TableSpecies",label="Species", 
          choices=BirdTableNames(), options = list(placeholder='Choose a species'),
          selected=if(RV$Table) input$TableSpecies else BirdTableNames()[1])
      }
  
    )
  
  
  #### Bird name to use for titles and captions ####
  BirdName<-reactive({
    req(input$TableSpecies)
    getBirdNames(object=BirdData[[1]], names =  input$TableSpecies, in.style="AOU", out.style = input$TableNames)
    })
  
  
  #### Band to display in titles and captions ####
  
  BandOut<-reactive({
    req(input$TableBand)
    paste0("0-",BandsAvailable %>% filter(Band==input$TableBand) %>% pull(MaxDistance), " meters")
  })
  
  
  #### Data, Captions, Titles, for the Tables ####
  
  #### Individual tables, titles, captions, base data used to calculate other tables ####
  
  IndividualBase<-reactive({
    req(input$TableYear, input$TableBand, input$TableSpecies)
    CountXVisit(object=BirdData, years=input$TableYear2, band=TableBandUse(), AOU=input$TableSpecies, max = TRUE)
  })
  
  IndividualPoint<-reactive({
    IndividualBase() %>% 
      {if(input$TableZeroHide)  filter_at(.,vars(starts_with("Visit")), any_vars(.!=0)) else . } %>% 
      {if (input$ParkTable!="All") filter(.,Admin_Unit_Code==input$ParkTable) else .} %>% 
      rename_at(vars(starts_with("Visit")),  ~sub("Visit", "Visit ", .x)) %>% 
      mutate(Park=factor(getParkNames(object=BirdData[Admin_Unit_Code]) ), "Point Name"=factor(Point_Name) ) %>% 
      dplyr::select(Park, `Point Name`, Year, starts_with("Visit"), Maximum=Max)
  })
  
  IndividualPark<-reactive({
    IndividualBase() %>% 
      group_by(Admin_Unit_Code) %>% 
      dplyr::summarize_at(vars(starts_with("Visit"), Max), mean, na.rm=T) %>% 
      dplyr::select_at(vars(starts_with("Visit"), Max)) %>%
      rbind(c(IndividualBase() %>% 
                summarize_at(vars(starts_with("Visit"), Max), mean, na.rm=T) %>%
                dplyr::select_at(vars(starts_with("Visit"), Max)))) %>% 
      round(digits=2) %>% 
      rename_at(vars(starts_with("Visit")),  ~sub("Visit", "Mean Visit ", .x)) %>%
      rename(Maximum=Max) %>% 
      t() %>%
      "colnames<-"(c(getParkNames(BirdData)[getParkNames(BirdData,"code") %in% unique(IndividualBase()$Admin_Unit_Code)], "All Parks"))
  })
  
  
  IndividualPointTitle<-reactive({paste(BirdName()," Data")})
  
  IndividualPointCaption<-reactive({  paste0('Number of ',BirdName(),'s detected at each monitoring point from ', min(input$TableYear2),' to ', max(input$TableYear2),'. This table includes birds detected at ',BandOut(),' from the observer. To see all instances where the bird was detected, and not instances where the bird was absent, choose the "Individual Species - All detections" table instead.')
  })
  
  IndividualParkTitle<-reactive({paste("Mean number of ",BirdName()," Detected per Point")})
  
  IndividualParkCaption<-reactive({
    req(BirdName(), input$TableYear, BandOut())
    paste0("Mean number of ",BirdName(),"s detected per monitoring point in each park from ",min(input$TableYear2)," to ", max(input$TableYear2),". 
    This is the number of birds detected during a visit divided by the number of monitoring points visited. This table includes birds found at ",
    BandOut()," from the observer.") 
  })
  
  
  ####  Richness tables, titles, captions ####
  
  RichnessPoint<-reactive({
    req(TableParkUse(), input$TableYear, TableBandUse() )
    getPoints(TableParkUse(),years=input$TableYear) %>% 
    left_join(birdRichness(TableParkUse(),years=input$TableYear, band=TableBandUse(),byPoint=T))  %>% 
    mutate(Park=factor(getParkNames(object=BirdData[Admin_Unit_Code]))) %>% 
    dplyr::select(Park,`Point Name`=Point_Name, Species=Richness )
  })
  
  RichnessPark<-reactive({
    data.frame(c(birdRichness(BirdData, years=input$TableYear, band=TableBandUse(),output="list"), 
                 birdRichness(BirdData, years=input$TableYear, band=TableBandUse() ))) %>%
      rbind (c(sapply(getPoints(BirdData,years=input$TableYear,output="list"),nrow),
               nrow(getPoints(BirdData,years=input$TableYear)))) %>% 
      "colnames<-"(c(getParkNames(BirdData),"All Parks")) %>% 
      "row.names<-"(c("Species","Monitoring Points")) %>% 
      select_if(colSums(.)>0) 
  })
  
  RichnessPointTitle<-reactive({
    paste0("Number of Species Detected per Monitoring Point: ",input$TableYear)})
  
  RichnessPointCaption<-reactive({  paste0("The number of different species found at each monitoring point during ", 
    input$TableYear," based on detections at ", BandOut()," from the observer. Note that monitoring began later at some points 
    than at others.")
  })
  
  RichnessParkTitle<-reactive({paste0("Number of Species Detected per Park: ",input$TableYear)})
  
  RichnessParkCaption<-reactive({paste0("Number of species detected in the parks in ",input$TableYear," based on detections at ", BandOut()," from the observer. Parks differ in the number of monitoring points and points differ in the number of years they have been visited, so differences between parks and years may be partially due to differences in sampling.") })
  
  
  ####  BCI tables, titles, captions, basedata used to calculate other tables  ####
  
  BCIBase<-reactive({
    withProgress(message="Calculating...  Please Wait",value=1,{
      BCI(object=BirdData, years=input$TableYear ,band=TableBandUse(), type={if(Network == "NETN") "NETN_Forest_BCI" else "Cent_Appal"} )
    })
  })    
  
  BCIPoint<-reactive({
    BCIBase() %>% 
    {if (input$ParkTable!="All") filter(.,Admin_Unit_Code==input$ParkTable) else .} %>%
      dplyr::select(Admin_Unit_Code, Point_Name,BCI,BCI_Category) %>% 
      mutate(Park=factor(getParkNames(object=BirdData[Admin_Unit_Code]))) %>% 
      select(Park, "Point Name"=Point_Name, "BCI Category"=BCI_Category)
  })

    BCIPark<-reactive({
    BCIBase() %>% 
      group_by(Admin_Unit_Code) %>% 
      dplyr::summarize("Mean BCI" = round (mean(BCI), digits=1)) %>%    
      dplyr::select(`Mean BCI`) %>%
      rbind(round(mean(BCIBase()$BCI),digits=1 )) %>% 
      mutate("Park BCI Category"=
               c("Low Integrity", "Medium Integrity","High Integrity","Highest Integrity")[findInterval(`Mean BCI`,
                                                                  vec=c(0,40.1,52.1,60.1,77.1))] ) %>% 
      t() %>% 
      "colnames<-"(c(getParkNames(BirdData)[getParkNames(BirdData,"code") %in% unique(BCIBase()$Admin_Unit_Code)],"All Parks")  )
  })  #complicated colnames statement covers the case where some park have no data in certain years. 
  
  BCIPointTitle<-reactive(paste0("Bird Community Index by Point: ",input$TableYear))
  
  BCIPointCaption<-reactive({paste0("The Bird Comminity Index (BCI) for each monitoring point during ", input$TableYear," based on detections at ", BandOut(),
    " from the observer. Note that monitoring began later at some points than at others. The BCI  is an index of ecological integrity 
    (O'Connell et al., 1998, 2000). Each point is assigned a BCI score based on the species of birds found. Scores are then assigned to one of four 
    BCI categories: Low, Medium, High or Highest Integrity. These scores are averaged to give a park-wide score. Note that the BCI of individual points 
    may fluctuate from year to year due to random sampling as well as due to changes in the environment.") })
  
  BCIParkTitle<-reactive({paste0("Bird Community Index by Park: ",input$TableYear) })
  
  BCIParkCaption<-reactive({  paste0("The Bird Comminity Index (BCI) for each park during ", input$TableYear," based on detections at ", BandOut(),
    " from the observer. The BCI  is an index of ecological integrity (O'Connell et al., 1998, 2000). Each park is assigned a BCI score by averaging point 
    scores, which in turn are based on the species of birds found. Scores are then assigned to one of four BCI categories: Low, Medium, High or Highest 
    Integrity.")})
  
  #### Create Tables and Title outputs ####
  
  ### Point Table title:
  
  output$TableTitle<-renderText({

    switch(input$TableValues,
           individual=IndividualPointTitle(),
           richness=RichnessPointTitle(),
           bci=BCIPointTitle()
    )
  })
  
  ###Point Table Caption  
  PointTableCaption<-reactive({
    req(input$TableYear)

    switch(input$TableValues,
           individual=IndividualPointCaption(),
           richness= RichnessPointCaption(),
           bci=BCIPointCaption() 
    )
  })
  
  ### Park Table title:
  
  output$ParkTableTitle<-renderText({

    switch(input$TableValues,
           individual=IndividualParkTitle(),
           richness=RichnessParkTitle(),
           bci=BCIParkTitle()
    )
  })
  
  ###Park Table Caption  
  ParkTableCaption<-reactive({

    switch(input$TableValues,
           individual=IndividualParkCaption(),
           richness=RichnessParkCaption(),
           bci=BCIParkCaption()
    )
  })
  
  
  PointTableData<-reactive({
  req(RichnessPoint())
    switch(input$TableValues,
           individual=IndividualPoint(),
           richness=RichnessPoint(),
           bci=BCIPoint())
  })
  
  ParkTableData<-reactive({

    switch(input$TableValues,
           individual=IndividualPark(),
           richness=RichnessPark(),
           bci=BCIPark())
  })
  
  output$PointTable<-DT::renderDataTable( 
    datatable(
      data=PointTableData(),rownames=F,caption=PointTableCaption(), 
      class="display compact",selection="single", #extensions="TableTools", 
      options=list(dom = 'T<"clear">lfrtip',
                   lengthMenu=c(10, 25, 100, 250)
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
      leafletProxy("BirdMap") %>% 
        setView(lng=PointSelected$Longitude,lat=PointSelected$Latitude,zoom=14)
      
      updateNavbarPage(session,inputId="MainNavBar", selected = "Map")
      
      updateRadioButtons(session, inputId="MapValues",
                         selected=ifelse(input$TableValues=="detects","individual",input$TableValues)) 
      updateSliderInput(session ,inputId="MapYear", 
                        value=ifelse(input$TableValues %in% c("individual","detects"),PointTableData()[RowSelect,"Year"],input$TableYear  ))
      updateRadioButtons(session, inputId="MapBand",selected=ifelse(input$TableValues=="individual",input$TableBand,"All"))
      
      
      updateRadioButtons(session, inputId="MapVisitSelect", 
          selected=if(input$TableValues =="individual") "Maximum Observed" else input$MapVisitSelect)

      
      updateRadioButtons(session,inputId="MapNames", selected=input$TableNames)
      
      updateSelectizeInput(session,inputId="MapSpecies", 
                           selected=ifelse(input$TableValues %in% c("individual","detects"), input$TableSpecies, input$MapSpecies))
      
      
      #### move to map ####
    
      
      #### Make popup ####
      leafletProxy("BirdMap") %>% clearPopups() %>% 
        addPopups(
          map=.,lat=PointSelected$Latitude, lng=PointSelected$Longitude, 
          popup=switch(input$TableValues, 
            individual=paste(collapse="<br/>", 
                paste(PointSelected$Point_Name,"<br/>"),
                paste(PointTableData() %>% filter(row_number()==RowSelect) %>% 
                  dplyr::select(starts_with("Visit")) %>% max,"detected", collapse=" ")),
                       
            richness= paste(collapse="<br/>",
                paste(PointSelected$Point_Name,':',PointTableData()[RowSelect,"Species"], "Species","<br/>","<br/>",collapse=" "),
                paste(getChecklist(BirdData,points=PointSelected$Point_Name, years=input$MapYear,  band=TableBandUse(),
                            out.style=input$MapNames),collapse="<br/>")) ,
                      
            bci=paste(sep="<br/>", PointSelected$Point_Name, paste0('BCI Value: ',PointTableData()[RowSelect,"BCI"]),
                paste('BCI Category: ',PointTableData()[RowSelect,"BCI Category"]) )
          )
        )
    })
 
  
  ####  Graphs Tab ####

  #### Park control for graphs ####
  output$ParkPlotSelect<-renderUI({
    selectizeInput(inputId="ParkPlot",label="Park:", choices=c("All Parks"="All", ParkList), selected="All" ) 
  })
  
  output$VisitSelect<-renderUI({
    req(input$ParkPlot)
    radioButtons(inputId="PlotVisit",label="Visit:", choices=c("All",
       as.character(seq(as.numeric(getDesign(PlotParkUse(), info= "visits")[[1]],1)))), selected="All",inline=TRUE ) 
  })
  
  output$PlotBandSelect<-renderUI({
    radioButtons(inputId = "PlotBand", label="Distance from Observer", choices=BandChoices, inline=T)
  })
  

  PlotParkUse<-reactive({  if (input$ParkPlot=="All") BirdData else BirdData[[input$ParkPlot]] })
  PlotBandUse<-reactive({seq(as.numeric(input$PlotBand)) })
 
   PlotVisitUse<-reactive({ req(input$PlotVisit)
    if(input$PlotVisit=="All") NA else as.numeric(input$PlotVisit)
    })
   
   PlotBandOut<-reactive({
     req(input$PlotBand)
     paste0("0-",BandsAvailable %>% filter(Band==input$PlotBand) %>% pull(MaxDistance), " meters")
   })
   
  BirdPlotNames<-reactive({
    BN3<-getChecklist(object =BirdData) 
    TempNames3<-getBirdNames(object=BirdData[[1]], names=BN3, in.style="AOU", out.style=input$PlotNames)
    TempNames3[is.na(TempNames3)]<-"Needs Name"
    names(BN3)<-TempNames3
    BN3 [order(TempNames3)]
  })
  
  
  
  observeEvent(input$PlotSpecies, RV$Plot<-TRUE, ignoreInit = T)
  
  observeEvent(BirdPlotNames(), 
               handlerExpr = {
                 updateSelectizeInput(session,inputId="PlotSpecies",label="Species", 
                                      choices=BirdPlotNames(), options = list(placeholder='Choose a species'),
                                      selected=if(RV$Plot) input$PlotSpecies else BirdPlotNames()[1])
               }
               
  )
  

  
  PlotParkName<-reactive(if (input$ParkPlot=="All") paste("All",Network,"Parks") else getParkNames(PlotParkUse(),"short" ))
  
  #### Detects Plot ####
  PlotBirdName<-reactive(getBirdNames(object=BirdData[[1]], names =  input$PlotSpecies, 
                                      in.style="AOU", out.style = input$PlotNames))
  
  PlotDetectTitle<-reactive({paste0(PlotBirdName(),"s Detected per Point in ",PlotParkName() )})  
  
  PlotDetectCaption<-reactive({paste0("Average (mean) number of ",PlotBirdName()," detected per point in ",PlotParkName(), " at ", 
                                      PlotBandOut(),". The horizontal axis indicates the year, with the number of points monitored in parenthesis. The vertical 
          axis indicates the number of birds deteced divided by the number of points monitored. Additional birds were likely present 
          at the points, but not detected.")
  })
  
  observe({
    if(input$GraphOutputs=="Detects"){
      output$DetectsPlot <- renderPlot({
        req(input$ParkPlot, input$PlotSpecies)
        detectsPlot(object= PlotParkUse(),band=PlotBandUse(),AOU=input$PlotSpecies, visits =PlotVisitUse(),
                    max=(input$PlotVisit=="All"), plot_title = "")
      })}
  })
  
  output$DetectsCaption<-renderText({
    req(input$PlotSpecies, input$ParkPlot, input$PlotBand, input$PlotVisit)
    PlotDetectCaption()
  })
  
  #### Richness Plot ####
  
  PlotRichnessTitle<-reactive({paste0("Number of Species Detected in ", PlotParkName())})
  
  PlotRichnessCaption<-reactive({paste0("Number of bird species detected during monitoring in ",PlotParkName()," at ",PlotBandOut(),
    ". The horizontal axis indicates the year, with the number of points monitored in parenthesis. The vertical axis indicates the 
    number of birds species detected during monitoring. Additional bird species were likely present at the points, but not detected.")})
  
  observe({
    if(input$GraphOutputs=="Richness"){
      output$RichnessPlot <- renderPlot({
        req(input$ParkPlot)
        richnessPlot(object= PlotParkUse(), band=PlotBandUse(), visits=PlotVisitUse(),  plot_title = "")
      })
    }
  })
  
  output$RichnessCaption<-renderText({
    req(input$PlotSpecies,input$ParkPlot,input$PlotBand)
    PlotRichnessCaption()
  })
  
  
  #### BCI Plot ####
  PlotBCITitle<-reactive({paste0("Bird Community Index for ",PlotParkName())})
  
  PlotBCICaption<-reactive({paste0("Bird Community Index (BCI) for ",PlotParkName()," at ", PlotBandOut(),
      ". The horizontal axis indicates the year, with the number of points monitored in parenthesis.  
      The points on the graph denote the average (mean) BCI for points monitored during each year for the selected visits.")})
  
  output$BCICaption<-renderText({
    req(input$PlotSpecies,input$ParkPlot,input$PlotBand)
    PlotBCICaption()
  }) 
  
  observe({
    if(input$GraphOutputs=="BCI"){
      output$BCIPlot <- renderPlot({
        withProgress(message="Calculating...  Please Wait",value=1,{
        req(input$ParkPlot)
        BCIPlot(object= PlotParkUse(), band=PlotBandUse(), visits=PlotVisitUse(), caption=F, type={if(Network == "NETN") "NETN_Forest_BCI" else "Cent_Appal"} )
      })
      })  
    }
  })
  
  #### Species Lists ####
  
  
  #### Park control for species list ####
  output$ParkListSelect<-renderUI({
    selectizeInput(inputId="ParkList",label="Park", choices=ParkList ) 
  })
  
  ListParkUse<-reactive({  if (input$ParkList=="All") BirdData else BirdData[[input$ParkList]] })
  
  output$PointListSelect <-renderUI({
    req(input$ParkList)
    
    selectizeInput(inputId="ListPointsUse", choices=c("All Points"="All", getPoints(ListParkUse())$Point_Name),
                   label="Points (optional)", multiple=TRUE, selected="All"
    )
  })
  
  ListPoints<-reactive({
    req(input$ListPointsUse)
    if("All" %in% input$ListPointsUse){NA}else{input$ListPointsUse}
  })
  
  
  #### Get species lists from IRMA / NPSpecies ####
  
  NPSpeciesURL<-reactive({
    req(input$ParkList)
    paste0("http://irmaservices.nps.gov/v3/rest/npspecies/checklist/",input$ParkList,"/bird?format=Json")
  })
  
  NPSpeciesList<-reactive({
    fromJSON(NPSpeciesURL())%>% 
      dplyr::select(CommonNames, ScientificName,Occurrence) %>% 
      arrange(CommonNames) %>% 
      rename('Latin Name'=ScientificName, 'Common Name'=CommonNames)
  })
  
  MonitoringList<-reactive({
    req(input$ParkList)
    tbl_df(data.frame( 'Latin.Name'= getChecklist(object=BirdData[[input$ParkList]], points=ListPoints(),out.style="Latin"))) %>% 
      mutate('Common Name' = getBirdNames(object=BirdData[input$ParkList], names=Latin.Name, in.style="Latin",out.style = "common"),
          'AOU Code' = getBirdNames(object=BirdData[input$ParkList], names=Latin.Name, in.style="Latin",out.style = "AOU")) %>% 
      select(`Common Name`, 'Latin Name'= Latin.Name, `AOU Code`) %>% arrange(`Common Name`) 
  })
  
  
  output$SpeciesList<-DT::renderDataTable(server=FALSE,
    datatable(rownames=F,caption="Species List", class="display compact",selection="single",
              options=list(lengthMenu=c(10, 25, 100, 250)),
      data=switch(input$SpeciesListType,
        All=NPSpeciesList(),
        Points=MonitoringList()
      )
    )
  )
  }) #End Shiny Server function
