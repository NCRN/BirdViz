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
                height="auto", right="150px", left="auto", width="200px",
      
      h4("Map Controls",class="panel-heading"),
      
      selectizeInput(inputId="MapValues", label="Data to Map", choices=c("Nothing Really"="nothing",
                   "Species Numbers"="richness", "Individual Species"="individual")
      ),
      
      div(id="SpeciesControls", 
       selectizeInput(inputId="MapSpecies",choices=NULL,label="Species"), #updated in server.r
       selectizeInput(inputId="SpeciesValues", label="Data", choices=c("Visit 1", "Visit 2", "Maximum Observed"))
      ),
      
      sliderInput(inputId="MapYear", label="Select a year:", min=2007,max=2014,value=2014, sep="",step=1,ticks=TRUE)
    ),
    
    fixedPanel(class="panel panel-default controls", draggable=TRUE, cursor="auto", top="85%", bottom="auto",
              height="auto", left="120px", width="180px",
      strong("Base Layer"),
      radioButtons(inputId="MapBase",label=NULL,choices=c("Map","Imagery"), selected="Map", inline=TRUE)
    )
  ),
  tabPanel("Map Data", h2("test2"))
)