library(shiny)
library(leaflet)
library(NCRNbirds)
library(dplyr)
library(magrittr)
library(rgdal)
library(DT)
library(ggvis)
library(tidyr)
#library(jsonlite, pos=100)



NCRN<-importNCRNbirds("./Data/")
ParkList<-getParkNames(NCRN, name.class="code")
names(NCRN)<-ParkList
names(ParkList)<-getParkNames(NCRN, name.class="short")

ParkBounds<-read.csv(file="./Data/boundboxes.csv", as.is=TRUE)



shinyServer(function(input,output,session){
 
  output$Test<-renderPrint(NULL)
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
    
    ### Tables
    toggle(id="TableSpecies", condition=input$TableValues=="individual")
    toggle(id="TableBand", condition=input$TableValues=="individual")
    toggle(id="TableNames", condition=input$TableValues=="individual")
    toggle(id="TableYear", condition=input$TableValues %in% c("individual","bci") )
    toggle(id="TableYear2", condition=input$TableValues == "richness")
  })
  

  ### Reactive Map UI Widgets
  
  #### List of species for map - needs to be named list.
  
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
           individual=paste(getBirdNames(object=NCRN[[1]], names =  input$MapSpecies, in.style="AOU", 
                                         out.style = input$MapNames), "<br>", " Observed"),
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
    input$Layers
    leafletProxy("BirdMap") %>%  
    clearGroup("Circles") %>% 
    addCircles(data=circleData(), layerId=circleData()$Point_Name, group="Circles", color=MapColors()(circleData()$Values),
          fillColor = MapColors()(circleData()$Values), opacity=0.8, radius=50*as.numeric(input$PointSize), fillOpacity = 0.8) 
  })
  
  ### Add Legend
 observe({ 
    leafletProxy("BirdMap") %>% 
     removeControl(layerId="CircleLegend") %>% 
     {if("Legends" %in% input$MapHide) 
        addLegend(map=., layerId="CircleLegend",pal=MapColors(), values= circleData()$Values, 
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
        Forested=addPopups(map=.,lat=ShapeClick$lat, lng=ShapeClick$lng, popup=ShapeClick$id)
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
  
  #########################################  Data Table Funcitons  ########################################################
  
  
  #   #### Park control for tables
  output$ParkTableSelect<-renderUI({
    selectInput(inputId="ParkTable",label="Park", choices=c("All Parks"="All", ParkList ) ) 
  })
  
  
  
  BirdTableNames<-reactive({
    BN2<-getChecklist(object =NCRN) #, years=input$TableYear, band=1)
    TempNames2<-getBirdNames(object=NCRN[[1]], names=BN2, in.style="AOU", out.style=input$TableNames)
    TempNames2[is.na(TempNames2)]<-"Needs Name"
    names(BN2)<-TempNames2
    BN2 [order(TempNames2)]
  })
  
  
  
  observe({
    updateSelectizeInput(session,inputId="TableSpecies",label="Species", choices=BirdTableNames())
  })
  
  TableParkUse<-reactive({ if (input$ParkTable=="All") NCRN else NCRN[input$ParkTable] })
  
  ### Data for the Tables
  
  BaseData<-reactive({
    switch(input$TableValues,
           individual=CountXVisit(object=NCRN,
                                  years=input$TableYear, 
                                  band=if(input$TableBand=="All") NA else seq(as.numeric(input$TableBand)), 
                                  AOU=input$TableSpecies),
           richness=NA,
           bci= withProgress(message="Calculating...  Please Wait",value=1,{
             BCI(object=NCRN, years=input$TableYear)
           })
    )
  })    
  
  
  DataOut<-reactive({
    validate(
      need(input$ParkTable, "Working...")
    )
    switch(input$TableValues,
      individual= BaseData() %>% 
                  {if (input$ParkTable!="All") filter(.,Admin_Unit_Code==input$ParkTable) else .} %>% 
                  rename("Visit 1"= Visit1, "Visit 2"=Visit2) %>% 
                  mutate(Park=factor(getParkNames(object=NCRN[Admin_Unit_Code]) ), "Point Name"=factor(Point_Name) ) %>% 
                      dplyr::select(Park, `Point Name`, Year, `Visit 1`,`Visit 2`),

      richness= withProgress(message="Calculating...  Please Wait",value=1,{
        getPoints(TableParkUse(),years=input$TableYear2[1]:input$TableYear2[2]) %>% 
              group_by(Point_Name) %>% 
              mutate(Species=birdRichness(TableParkUse(),points=Point_Name, years=input$TableYear2[1]:input$TableYear2[2])) %>% 
              ungroup() %>%
              mutate(Park=factor(getParkNames(object=NCRN[Admin_Unit_Code]) ), "Point Name"=factor(Point_Name)) %>% 
              rowwise() %>% 
              mutate(Years=paste(range(getVisits(TableParkUse(),points=Point_Name,
                          years=input$TableYear2[1]:input$TableYear2[2])$Year), collapse="-")) %>% 
                  dplyr::select(Park,`Point Name`, Years,Species ) }),
      
      bci=BaseData() %>% 
      {if (input$ParkTable!="All") filter(.,Admin_Unit_Code==input$ParkTable) else .} %>%
        dplyr::select(Point_Name,BCI,BCI_Category)
      )
  })
  
  ParkDataOut<-reactive({
    validate(
      need(input$ParkTable, "Working...")
    )
    switch(input$TableValues,
      individual= BaseData() %>% 
        group_by(Admin_Unit_Code) %>% 
        summarize("Mean Visit 1"=round(mean(Visit1, na.rm=T),digits=2), "Mean Visit 2"= round( mean(Visit2, na.rm=T),digits=2)) %>%
        dplyr::select(`Mean Visit 1`,`Mean Visit 2`) %>%
        rbind(c(BaseData() %>% 
                summarize("Mean Visit 1"=round(mean(Visit1, na.rm=T),digits=2), 
                          "Mean Visit 2"= round(mean(Visit2, na.rm=T),digits=2) ) %>% 
                dplyr::select(`Mean Visit 1`,`Mean Visit 2`) %>% unname()) ) %>% 
        t() %>% "colnames<-"(c(getParkNames(NCRN),"All Parks")),
      
      richness= data.frame(c(birdRichness(NCRN,years=input$TableYear2[1]:input$TableYear2[2], output="list"), 
                    birdRichness(NCRN,years=input$TableYear2[1]:input$TableYear2[2]))) %>% 
                "names<-"(c(getParkNames(NCRN),"All Parks")) %>% 
                  "row.names<-"("Species"),
      
      bci=BaseData() %>% 
        group_by(Admin_Unit_Code) %>% 
        summarize("Mean BCI" = round (mean(BCI), digits=1)) %>%    
        dplyr::select(`Mean BCI`) %>%
        rbind(round(mean(BaseData()$BCI),digits=1 )) %>% 
          mutate("Park BCI Category"=
          c("Low Integrity", "Medium Integrity","High Integrity","Highest Integrity")[findInterval(`Mean BCI`,
                                                                          vec=c(0,40.1,52.1,60.1,77.1))] ) %>% 
        t() %>% "colnames<-"(c(getParkNames(NCRN),"All Parks"))
    )
  })
  
  
### Table title:
  
  output$TableTitle<-renderText({
    switch(input$TableValues,
           individual=paste(getBirdNames(object=NCRN[[1]], names =  input$TableSpecies, 
                                         in.style="AOU", out.style = input$TableNames)," Detections"),
           richness="Number of Species Detected per Point",
           bci="Bird Community Index per Point"
    )
  })
  
  ###Table Caption  
  TableCaption<-reactive({
    switch(input$TableValues,
           individual="Individual species table caption",
           richness="richness caption",
           bci="BCI caption"
    )
  })

  ### ParkTable title:
  
  output$ParkTableTitle<-renderText({
    switch(input$TableValues,
           individual=paste("Mean",getBirdNames(object=NCRN[[1]], names =  input$TableSpecies, 
                                         in.style="AOU", out.style = input$TableNames)," Detected per Point"),
           richness="Number of Species Detected per Park",
           bci="Bird Community Index per Park"
    )
  })
  
  ###Table Caption  
  ParkTableCaption<-reactive({
    switch(input$TableValues,
           individual="Individual species park table caption",
           richness=" Species richness park caption",
           bci="BCI park caption"
    )
  })
    ### The tables

  output$DataTable<-DT::renderDataTable(expr=DataOut(), rownames=F,caption=input$TableValues, class="display compact"  )
  
  output$DataTable2<-DT::renderDataTable(expr=ParkDataOut(), caption=ParkTableCaption(), class="display compact",
                                         options=list(dom="t", ordering=FALSE))

  
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
# # #   
# # #     X<-mtcars %>% ggvis(~wt, ~mpg, fill=~factor(cyl),size:=200) %>% 
# # #     layer_points() %>% 
# # #     add_legend(scales="fill",title="Legend Title") %>% 
# # #     add_tooltip(function(x) paste("wt=",x$wt," mpg=", x$mpg), on="hover")
# #   
     bind_shiny(plot_id="PlotOut")

  
}) #End Shiny Server function
