library(shiny)
library(leaflet)
library(ggvis)
library(shinyjs)

navbarPage(title=HTML("<div> <a href='https://science.nature.nps.gov/im/units/ncrn/'> <img src='ah_small_black.gif',
                      alt='Forest Bird Visualizer'> </a> Forest Bird Visualizer</div>"),
           position = "static-top",inverse=TRUE, collapsible = FALSE, fluid=TRUE, windowTitle = "NCRN Birds",
           theme="https://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css", id="MainNavBar",
          
  tabPanel(title="Map", 
    useShinyjs(),
    #includeScript("http://www.nps.gov/lib/bootstrap/3.3.2/js/nps-bootstrap.min.js"),
    
    div(class="outer",
      tags$head(includeCSS("./www/mapstyles.css") ),# brings in  css file that lets map take up whole screen
      tags$head(HTML('<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />')), #icon for broswer tab
      tags$head(includeScript("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js")),
      leafletOutput("BirdMap", width = "100%", height = "100%")
    ),
    
     #### Map Controls Box ####
    
    fixedPanel(class="panel panel-default controls", draggable = TRUE, cursor="auto", top="80px", bottom="auto",
                height="auto", right="180px", left="auto", width="250px", id="MapControlPanel",
      
      h4("Map Controls",class="panel-heading"),
      
      #textOutput("Test"),

      tags$div(title="Choose the type of data you wish to see",
        radioButtons(inputId="MapValues", label="Data to Map", choices=c("Individual Species"="individual",
                                      "Number of Species"="richness", "Bird Community Index (BCI)"="bci"),inline=F
      )),
      
      tags$div(title="Include birds at what distance from the observer?",
        radioButtons(inputId="MapBand", label="Distance from Observer:",
         choices=c("0-50 meters"=1,"0-100 meters"=2,"Any distance"="All"))),
      
      div(id="SpeciesControls",
      tags$div(title="Species to display on map",
               selectizeInput(inputId="MapSpecies",choices=NULL,label="Species")), #updated in server.r
      tags$div(title="Display bird observations from Vist 1, Visit 2, or the maixmum of the two ",
               selectizeInput(inputId="SpeciesValues", label="Visit", choices=c("Maximum Observed", "Visit 1", "Visit 2")))
      ),
      
      tags$div(title="Choose which year's data to display",
               sliderInput(inputId="MapYear", label="Year:", min=2007,max=2015,value=2015, sep="",step=1, ticks=T)),
      hr(),
      tags$div(title="Use common name, Latin name, or American Ornithological Union code",
        radioButtons(inputId="MapNames",label="Names:", choices=c("Common"="common","Latin"="Latin","AOU"="AOU"), inline=TRUE)),
      hr(),
      h4("eBird Data",class="panel-heading", id="EBirdTitle"),
      tags$div(title="Display citizen science from ebird (non-NPS data)",
        checkboxInput(inputId="MapEBird", label="Show recent eBird Data?")),
      #hidden(
        tags$div(title="# of days worth of data to display",
      sliderInput(inputId="MapEBirdDays", label= "Display data from how many days prior to today?",min=1,max=30,sep="",value=14 )),
      #),
      hr(),
      actionButton(inputId="AboutMap", class="btn btn-primary", label="About the map...")
    ),
   
    ##### About the Map panel ####
    hidden(
    fixedPanel(class="panel panel-primary controls",draggable=TRUE,cursor="auto",top=80,bottom="auto",height="520",
               left=450,width="500",id="AboutMapPanel",style="padding: 0px",
               div(class="panel-heading", h4("About the Map" )),
               div(class="panel-body",style="height: 400px;  overflow-y: scroll",
                  includeHTML("./www/AboutMap.html")),
               div(class="panel-footer", 
                   actionButton(inputId="CloseAboutMap",class="btn btn-primary",label="Close"))  )
    ),

    ###### Zoom Box 
    fixedPanel(class="panel panel-default controls",draggable=TRUE,cursor="auto",top=80,bottom="auto",height="auto",
                  left=50,width=250,id="ZoomPanel",
      h4("Display",class="panel-heading"),
      h5("Zoom to:"),
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
    # Stoppe dto speed up ap

    # fixedPanel(class="panel panel-default controls", draggable=TRUE, cursor="auto", top="80%", bottom="auto",
    #            height="auto", left="50px", width="auto",id="ExtraLayerPanel",
    #            strong("Map Layers:"),
    #            tags$div(title="Add additional informtaion to the parks on the map.",
    #             radioButtons(inputId="Layers",label=NULL,choices=c("None","Ecoregions","Forested Area"="Forested"), 
    #                         selected="None", inline=TRUE))
    # ),
    # 
    ####  Show/Hide Panel
    
    fixedPanel(class="panel panel-default controls", draggable=TRUE, cursor="auto", top="90%", bottom="auto",
         height="auto", left="50px", width="auto",
      tags$div(title="You can hide controls that you are not using.",
          checkboxGroupInput(inputId="MapHide", label=strong("Show:"),inline=TRUE,
            choices=c("Zoom", "Map Controls"="MapControls", "Legends","Base Layers"="BaseLayers", "Map Layers"="ExtraLayers"),
            selected=c("MapControls","Legends","Zoom","BaseLayers","ExtraLayers"))
      )
    )
    
  ),  #end of Map tab

###############################  Tables Tab
  tabPanel("Data Tables",
    column(3,
      wellPanel(
        h4(strong("Select Data:")),
        
        br(),
       
        tags$div(title="Select the type of data you are interested in.",
          radioButtons(inputId="TableValues", label="Type of Data", 
            choices=c("Individual Species"="individual",
                      #"Individual Species - All detections"="detects", 
                    "Number of Species"="richness", "Bird Community Index (BCI)"="bci"), inline=F)
        ),
        tags$div(title="Select a park.",uiOutput("ParkTableSelect"))
      ),
      wellPanel(
      
        h4(strong("Options:")),
      
        br(),
        
        tags$div(title="Select a species.",
                 selectizeInput(inputId="TableSpecies",choices=NULL,label="Species")), #updated in server.r
      
        tags$div(title="Select a year.",
                 sliderInput(inputId="TableYear", label="Year:", min=2007,max=2015,value=2015, sep="",step=1, ticks=T)),
        
        tags$div(title="Select a range of oen or more years.",
                 sliderInput(inputId="TableYear2", label="Year:", min=2007,max=2015,value=c(2007,2015), sep="",step=1, ticks=T)),
        
      
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
    
    ##### About the tables panel
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
  
###   Graphs Tab
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

### Species List Tab
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
             includeHTML("./www/ProjectInfo.html")
    ),
    tabPanel("Citations and References",
             includeHTML("./www/Citations.html")
    )
  )# end of About tab

           
)## end UI