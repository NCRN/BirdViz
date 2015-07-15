library(shiny)
library(leaflet)
library(shinyjs)

navbarPage(title=HTML("<div> <a href='http://science.nature.nps.gov/im/units/ncrn/'> <img src='ah_small_black.gif',
                      alt='Forest Bird Visualizer'> </a> Forest Bird Visualizer</div>"),
           position = "static-top",inverse=TRUE, collapsible = FALSE, fluid=TRUE, windowTitle = "NCRN Birds",
           theme="http://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css",
          
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
                height="auto", right="150px", left="auto", width="250px", id="MapControlPanel",
      
      h4("Map Controls",class="panel-heading"),
      
      textOutput("Test"),

      radioButtons(inputId="MapValues", label="Data to Map", choices=c("Individual Species"="individual",
                                      "Species Numbers"="richness", "Bird Community Index (BCI)"="bci"),inline=F
      ),
      
      div(id="SpeciesControls", 
       selectizeInput(inputId="MapSpecies",choices=NULL,label="Species"), #updated in server.r
       selectizeInput(inputId="SpeciesValues", label="Data", choices=c("Maximum Observed", "Visit 1", "Visit 2"))
      ),
      
      sliderInput(inputId="MapYear", label="Year:", min=2007,max=2014,value=2014, sep="",step=1, ticks=T),
      hr(),
      radioButtons(inputId="MapNames",label="Names:", choices=c("Common"="common","Latin"="Latin","AOU"="AOU"), inline=TRUE)
    ),
   
    
    

    ###### Zoom Box 
    fixedPanel(class="panel panel-default controls",draggable=TRUE,cursor="auto",top=80,bottom="auto",height="auto",
                  left=50,width=250,id="ZoomPanel",
      h4("Zoom to:"),
    fluidRow(
     column(9,tags$div(title="Choose a park and click 'Go'", uiOutput("ParkZoomControl"))),
      column(3,actionButton(class="btn btn-primary btn-sm", inputId="Zoom", label="Go")) 
             ),
      
         hr(),
        tags$div(title="Increases size of plots for easier viewing",
            radioButtons(inputId="PointSize", label=h4("Point size:"), 
                  choices=c("1X (to scale)"=1, "5X"=sqrt(5)), selected="1", inline=TRUE)
        )
      
    ),
    
    
#     ######  Base Layer BoX
#     
#     fixedPanel(class="panel panel-default controls", draggable=TRUE, cursor="auto", top="80%", bottom="auto",
#                height="auto", left="120px", width="250px",id="BaseLayerPanel",
#                strong("Base Layer:"),
#                radioButtons(inputId="MapBase",label=NULL,choices=c("Map","Imagery","Slate"), selected="Map", inline=TRUE)
#     ),
#     
    ####  Show/Hide Panel
    
    fixedPanel(class="panel panel-default controls", draggable=TRUE, cursor="auto", top="90%", bottom="auto",
               height="auto", left="120px", width="425px",
               checkboxGroupInput(inputId="MapHide", label=strong("Show:"),inline=TRUE,
                                  choices=c("Map Controls"="MapControls", "Legend", "Zoom","Base Layers"="BaseLayers"),
                                  selected=c("MapControls","Legend","Zoom","BaseLayers")
                )
               
               
    )
    
    
    
    
    
    
    
  ),
  tabPanel("Map Data", h2("test2"))
)