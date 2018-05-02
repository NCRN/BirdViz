library(shiny)
library(leaflet)
library(ggvis)
library(shinyjs)

navbarPage(title=HTML("<div> <a href=", NetworkURL,"> <img src='ah_small_black.gif',
                      alt='Forest Bird Visualizer'> </a> Forest Bird Visualizer</div>"),
           position = "static-top",inverse=TRUE, collapsible = FALSE, fluid=TRUE, windowTitle = paste(Network,"Forest Birds"),
           theme="https://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css", id="MainNavBar",
          
  tabPanel(title="Map", style="padding: 0",
    useShinyjs(),
    
    div(class="outer",
      tags$head(includeCSS("./www/mapstyles.css") ),# brings in  css file that lets map take up whole screen
      tags$head(HTML('<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />')), #icon for broswer tab
      tags$head(includeScript("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js"))
      
    ),
    
    
    fluidRow(
      column(2, style="padding: 0 0 0 10px",
      
    
     #### Map Controls Box ####
    
    div( id="MapControlPanel", class="panel panel-default controls", 
      
      h4("Map Controls",class="panel-heading"),

      tags$div(title="Choose the type of data you wish to see",
        selectizeInput(inputId="MapValues", label="Data to Map", choices=c("Individual Species"="individual",
                                      "Number of Species"="richness", "Bird Community Index (BCI)"="bci"))
      ),
      
      div(id="SpeciesControls",
          tags$div(title="Species to display on map",
                   selectizeInput(inputId="MapSpecies",choices=NULL,label="Species")), #updated in server.r
          
          tags$div(title="Use common name, Latin name, or American Ornithological Union code",
                   radioButtons(inputId="MapNames",label="Names:", choices=c("Common"="common","Latin"="Latin","AOU"="AOU"), inline=TRUE)),
          
          br(),
          
          tags$div(title="Choose which year's data to display",
                   sliderInput(inputId="MapYear", label="Year:", min=Years$Start,max=Years$End, value=Years$End, sep="",step=1, ticks=T)),
          
          tags$div(title="Include birds at what distance from the observer?",
                   selectizeInput(inputId="MapBand", label="Distance from Observer:",
                                  choices=NULL))# updated in server.r
          #c("0-50 meters"=1,"0-100 meters"=2,"Any distance"="All"))
          ),
          
          tags$div(title="Display bird observations from which visit? ",
                   selectizeInput(inputId="MapVisit", label="Visit", choices=NULL)), #updated in server.r
      #c("Maximum Observed", "Visit 1", "Visit 2"))),
          
        
    
      hr(),
    
      # h4("eBird Data",class="panel-heading", id="EBirdTitle"),
      # tags$div(title="Display citizen science from ebird (non-NPS data)",
      #   checkboxInput(inputId="MapEBird", label="Show recent eBird Data?")),
      # 
      # tags$div(title="# of days worth of data to display",
      # sliderInput(inputId="MapEBirdDays", label= "Display data from how many days prior to today?",min=1,max=30,sep="",value=14 )),
      # hr(),
      actionButton(inputId="AboutMap", class="btn btn-primary", label="About the map...")),
    
    #### Zoom Box ####
    div(class="panel panel-default controls",id="ZoomPanel",
               h4("Zoom to",class="panel-heading"),
               fluidRow(
                 column(9,tags$div(title="Choose a park and click 'Go'", uiOutput("ParkZoomControl"))),
                 column(3,tags$div(title="Choose a park and click 'Go'",
                                   actionButton(class="btn btn-primary btn-sm", inputId="Zoom", label="Go"))) 
               ),
               hr(),
               tags$div(title="Increases size of plots for easier viewing",
                        radioButtons(inputId="PointSize", label="Point size:", 
                                     choices=c("50m radius"=50, "100m radius"=100), selected="50", inline=TRUE)
               )
    ),
    ####  Map Layer BoX ####
    div(class="panel panel-default controls",id="ExtraLayerPanel",
               h4("Additional Layers", class="panel-heading"),
               tags$div(title="Add additional informtaion to the parks on the map.",
                        selectizeInput(inputId="Layers",label=NULL,choices=ExtraLayers)
               )
    )
    ), #End code for column on left
  
    column(10, style="padding: 0",
         div(leafletOutput("BirdMap",width = "100%", height = "1000px"))
    )
  
  ),#end of fluidRow
  
  ##### About the Map panel ####
  hidden(
    fixedPanel(class="panel panel-primary controls",draggable=TRUE,cursor="auto",top=80,bottom="auto",height="520",
               left=450,width="500",id="AboutMapPanel",style="padding: 0px",
               div(class="panel-heading", h4("About the Map" )),
               div(class="panel-body",style="height: 400px;  overflow-y: scroll",
                   includeHTML("./www/AboutMap.html")),
               div(class="panel-footer", 
                   actionButton(inputId="CloseAboutMap",class="btn btn-primary",label="Close"))  )
  )
),  #end of Map tab

####  Tables Tab ####
  tabPanel("Data Tables",
    column(3,
      wellPanel(
        h4(strong("Select Data:")),
        
        br(),
       
        tags$div(title="Select the type of data you are interested in.",
          radioButtons(inputId="TableValues", label="Type of Data", 
            choices=c("Individual Species"="individual","Number of Species"="richness", "Bird Community Index (BCI)"="bci"), inline=F)
        ),
        tags$div(title="Select a park.",uiOutput("ParkTableSelect"))
      ),
      wellPanel(
      
        h4(strong("Options:")),
      
        br(),
        
        tags$div(title="Select a species.",
                 selectizeInput(inputId="TableSpecies",choices=NULL,label="Species")), #updated in server.r
      
        tags$div(title="Select a year.",
                 sliderInput(inputId="TableYear", label="Year:", min=Years$Start,max=Years$End,value=Years$End, sep="",step=1, ticks=T)),
        
        tags$div(title="Select a range of one or more years.",
                 sliderInput(inputId="TableYear2", label="Year:", min=Years$Start,max=Years$End,value=c(Years$Start,Years$End), sep="",
                             step=1, ticks=T)),
        
      
        tags$div(title="Include birds at what distance from the observer?",
                 radioButtons(inputId="TableBand", label="Distance from Observer:",
                   choices=c("0-50 meters"=1,"0-100 meters"=2,"Any distance"="All"))),
        tags$div(title="Show only the data for points were the bird was detected",
                 checkboxInput(inputId="TableZeroHide", label="Hide points wth no detections", value=FALSE)
        ),
        hr(),
        tags$div(title="Use common name, Latin name, or American Ornithological Union code",
          radioButtons(inputId="TableNames",label="Names:", choices=c("Common"="common","Latin"="Latin","AOU"="AOU"), inline=TRUE)),
        
        actionButton(inputId="AboutTables", class="btn btn-primary", label="About the tables...")
      )
    ),
    
    column(9, 
      h2(textOutput("ParkTableTitle")),
      DT::dataTableOutput("ParkTable"),
      br(),
      br(),
      hr(style="border: solid 1px black"),
      h2(textOutput("TableTitle")),
      DT::dataTableOutput("PointTable"),
      br(),
      h3("Click on any row to see that location on the map.")
    ),
    
    ##### About the tables panel ####
    hidden(
      fixedPanel(class="panel panel-primary controls",draggable=TRUE,cursor="auto",top=80,bottom="auto",height="520",
                 left=475,width="500",id="AboutTablesPanel",style="padding: 0px",
                 div(class="panel-heading", h4("About the Tables" )),
                 div(class="panel-body",style="height: 400px;  overflow-y: scroll",
                     includeHTML("./www/AboutTables.html")),
                 div(class="panel-footer", 
                     actionButton(inputId="CloseAboutTables",class="btn btn-primary",label="Close"))  )
    )
  ),  # end Tables Tab
  
####   Graphs Tab ####
  tabPanel("Graphs",
    column(3,
      wellPanel(
        h4(strong("Select Data:")),
        br(),
        tags$div(title="Select a park.",uiOutput("ParkPlotSelect")),
        tags$div(title="Select a species.",
                 selectizeInput(inputId="PlotSpecies",choices=NULL,label="Species")), #updated in server.r
        tags$div(title="Include birds at what distance from the observer?",
                 radioButtons(inputId="PlotBand", label="Distance from Observer:",
                     choices=c("0-50 meters"=1,"0-100 meters"=2,"Any distance"="All")
        )),
        tags$div(title="Use commmon name, Latin name, or American Ornithological Union code",
                 radioButtons(inputId="PlotNames",label="Names:", choices=c("Common"="common","Latin"="Latin","AOU"="AOU"), 
                              inline=TRUE)),
        actionButton(inputId="AboutGraphs", class="btn btn-primary", label="About the graphs...")
      )
    ),
    column(9,
      tabsetPanel(id="GraphOutputs", type="pills", 
        tabPanel(tags$div(title="Graphs of number of birds observed","Individual Species"), value="Detects",
           ggvisOutput("DetectsPlot"),
           textOutput("DetectsCaption")
        ),
        tabPanel(tags$div(title="Graphs of number of species found", "Number of Species"), value="Richness",
          ggvisOutput("RichnessPlot"),
          textOutput("RichnessCaption")
        ),
        tabPanel(tags$div(title= "Graphs of Bird Community Index values","Bird Community Index"), value="BCI",
          ggvisOutput("BCIPlot"),
          textOutput("BCICaption")
        )
      )
    ),
    hidden(
      fixedPanel(class="panel panel-primary controls",draggable=TRUE,cursor="auto",top=80,bottom="auto",height="520",
                 left=475,width="500",id="AboutGraphsPanel",style="padding: 0px",
                 div(class="panel-heading", h4("About the Graphs" )),
                 div(class="panel-body",style="height: 400px;  overflow-y: scroll",
                     includeHTML("./www/AboutGraphs.html")),
                 div(class="panel-footer", 
                     actionButton(inputId="CloseAboutGraphs",class="btn btn-primary",label="Close"))  )
    )
    
  ), # end graphs tab

#### Species List Tab ####
  tabPanel("Species Lists",
    column(3,
      wellPanel(
        tags$div(title="Choose the type of species list you want",
                   radioButtons(inputId="SpeciesListType", label=strong("Choose a species list"),
                     choices=c("Only birds found during monitoring"="Points","All birds known from the park"="All"))),
        tags$div(title="Choose a park",uiOutput("ParkListSelect")),
        tags$div(title="Choose one or more monitoring points",uiOutput("PointListSelect")),
        actionButton(inputId="AboutLists", class="btn btn-primary", label="About the species lists...")
      )
    ),
    column(8,
           DT::dataTableOutput("SpeciesList")
    ),
    hidden(
      fixedPanel(class="panel panel-primary controls",draggable=TRUE,cursor="auto",top=80,bottom="auto",height="520",
                 left=475,width="500",id="AboutListsPanel",style="padding: 0px",
                 div(class="panel-heading", h4("About the Species Lists" )),
                 div(class="panel-body",style="height: 400px;  overflow-y: scroll",
                     includeHTML("./www/AboutLists.html")),
                 div(class="panel-footer", 
                     actionButton(inputId="CloseAboutLists",class="btn btn-primary",label="Close"))  )
    )
  ),  # end Species List tab

  navbarMenu(title="About the Project",
    tabPanel("Project Information",
             ProjectInfo
    ),
    tabPanel("Citations and References",
             Citations
    )
  )# end of About tab

           
)## end UI