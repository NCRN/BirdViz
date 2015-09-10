library(shiny)
library(leaflet)
library(ggvis)
library(shinyjs)

navbarPage(title=HTML("<div> <a href='http://science.nature.nps.gov/im/units/ncrn/'> <img src='ah_small_black.gif',
                      alt='Forest Bird Visualizer'> </a> Forest Bird Visualizer</div>"),
           position = "static-top",inverse=TRUE, collapsible = FALSE, fluid=TRUE, windowTitle = "NCRN Birds",
           theme="http://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css", id="MainNavBar",
          
  tabPanel(title="Map", 
    useShinyjs(),
    includeScript("http://www.nps.gov/lib/bootstrap/3.3.2/js/nps-bootstrap.min.js"),
    div(class="outer",
      tags$head(includeCSS("./www/mapstyles.css") ),# brings in  css file that lets map take up whole screen
      tags$head(HTML('<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />')), #icon for broswer tab
      leafletOutput("BirdMap", width = "100%", height = "100%")
    ),
    
     ################ Map Controls Box
    
    fixedPanel(class="panel panel-default controls", draggable = TRUE, cursor="auto", top="80px", bottom="auto",
                height="auto", right="180px", left="auto", width="250px", id="MapControlPanel",
      
      h4("Map Controls",class="panel-heading"),
      
      textOutput("Test"),

      radioButtons(inputId="MapValues", label="Data to Map", choices=c("Individual Species"="individual",
                                      "Number of Species"="richness", "Bird Community Index (BCI)"="bci"),inline=F
      ),
      
      radioButtons(inputId="MapBand", label="Distance from Observer:",
                   choices=c("0-50 meters"=1,"0-100 meters"=2,"Any distance"="All")),
      
      div(id="SpeciesControls", 
       selectizeInput(inputId="MapSpecies",choices=NULL,label="Species"), #updated in server.r
       selectizeInput(inputId="SpeciesValues", label="Data", choices=c("Maximum Observed", "Visit 1", "Visit 2"))
      ),
      
      sliderInput(inputId="MapYear", label="Year:", min=2007,max=2014,value=2014, sep="",step=1, ticks=T),
      hr(),
      radioButtons(inputId="MapNames",label="Names:", choices=c("Common"="common","Latin"="Latin","AOU"="AOU"), inline=TRUE),
      hr(),
      h4("eBird Data",class="panel-heading", id="EBirdTitle"),
      checkboxInput(inputId="MapEBird", label="Show recent eBird Data?"),
      sliderInput(inputId="MapEBirdDays", label= "Display data from how many days prior to today?",min=1,max=30,sep="",value=14 )
    ),
   
    
    

    ###### Zoom Box 
    fixedPanel(class="panel panel-default controls",draggable=TRUE,cursor="auto",top=80,bottom="auto",height="auto",
                  left=50,width=250,id="ZoomPanel",
      h4("Display",class="panel-heading"),
      h5("Zoom to:"),
    fluidRow(
     column(9,tags$div(title="Choose a park and click 'Go'", uiOutput("ParkZoomControl"))),
      column(3,actionButton(class="btn btn-primary btn-sm", inputId="Zoom", label="Go")) 
             ),
      
         hr(),
        tags$div(title="Increases size of plots for easier viewing",
            radioButtons(inputId="PointSize", label="Point size:", 
                  choices=c("50m radius"=50, "100m radius"=100), selected="50", inline=TRUE)
        )
      
    ),
    
    
#     ######  Extra Layer BoX
    
    fixedPanel(class="panel panel-default controls", draggable=TRUE, cursor="auto", top="80%", bottom="auto",
               height="auto", left="50px", width="auto",id="ExtraLayerPanel",
               strong("Extra Layers:"),
               radioButtons(inputId="Layers",label=NULL,choices=c("None","Ecoregions","Forested Area"="Forested"), 
                            selected="None", inline=TRUE)
    ),
    
    ####  Show/Hide Panel
    
    fixedPanel(class="panel panel-default controls", draggable=TRUE, cursor="auto", top="90%", bottom="auto",
         height="auto", left="50px", width="auto",
      checkboxGroupInput(inputId="MapHide", label=strong("Show:"),inline=TRUE,
        choices=c("Zoom", "Map Controls"="MapControls", "Legends","Base Layers"="BaseLayers", "Extra Layers"="ExtraLayers"),
        selected=c("MapControls","Legends","Zoom","BaseLayers","ExtraLayers")
      )
    )
    
    
  ),

###############################  Tables Tab
  tabPanel("Data Tables",
    column(3,
      wellPanel(
        h4(strong("Select Data:")),
        
        br(),
       
        radioButtons(inputId="TableValues", label="Type of Data", 
          choices=c("Individual Species - All data from 1 year"="individual","Individual Species - All detections"="detects", 
                    "Number of Species"="richness", "Bird Community Index (BCI)"="bci"), inline=F),
       
        uiOutput("ParkTableSelect")
      ),
      wellPanel(
      
        h4(strong("Options:")),
      
        br(),
        
        selectizeInput(inputId="TableSpecies",choices=NULL,label="Species"), #updated in server.r
      
        sliderInput(inputId="TableYear", label="Year:", min=2007,max=2014,value=2014, sep="",step=1, ticks=T),
     # sliderInput(inputId="TableYear2", label="Year:", min=2007,max=2014,value=c(2007,2014), sep="",step=1, ticks=T),
      
        radioButtons(inputId="TableBand", label="Distance from Observer:",
                   choices=c("0-50 meters"=1,"0-100 meters"=2,"Any distance"="All")),
        hr(),
        radioButtons(inputId="TableNames",label="Names:", choices=c("Common"="common","Latin"="Latin","AOU"="AOU"), inline=TRUE)
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
    )
  ),  # end Tables Tab
  
###   Plots Tab
  tabPanel("Graphs",
    column(3,
      wellPanel(
        h4(strong("Select Data:")),
        br(),
        radioButtons(inputId="PlotValues", label="Type of Data", 
                     choices=c("Individual Species"="detects", 
                               "Number of Species"="richness", "Bird Community Index (BCI)"="bci"), inline=F),
        uiOutput("ParkPlotSelect")
        ),
      wellPanel(
        h4(strong("Options")),
        br(),
        selectizeInput(inputId="PlotSpecies",choices=NULL,label="Species"), #updated in server.r
        radioButtons(inputId="PlotBand", label="Distance from Observer:",
                     choices=c("0-50 meters"=1,"0-100 meters"=2,"Any distance"="All")
        ),
        radioButtons(inputId="PlotNames",label="Names:", choices=c("Common"="common","Latin"="Latin","AOU"="AOU"), inline=TRUE)
      )
    ),
    column(9,
      tabsetPanel(id="GrapOutputs", type="pills", 
        tabPanel(title="Individual Species",
           ggvisOutput("DetectsPlot")
        ),
        tabPanel(title="Number of Species",
          h3("test")
        ),
        tabPanel(title="Bird Community Index",
          h2("test")
        )
      )
    )
    
  ), # end plots tab
  tabPanel("Species Lists",
      h3("Add this")
  ),
  tabPanel("About",
              h3("Add this")
  )

           
)## end UI