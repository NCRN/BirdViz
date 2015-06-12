library(shiny)
library(leaflet)
library(shinyjs)

navbarPage(title="Bird Test", position = "static-top",inverse=TRUE, collapsible = FALSE, fluid=TRUE, windowTitle = "NCRN Birds",
          
        tabPanel(title="Map", 
                 useShinyjs(),
                  div(class="outer",
                      tags$head(includeCSS("./www/mapstyles.css") ), # defines css file that lets map take up whole screen
                  leafletOutput("BirdMap", width = "100%", height = "100%")
                  ),
                 
                 ################ Map Controls Box
                 
                 fixedPanel( class="panel panel-primary controls", draggable = TRUE, cursor="auto", top="170px", bottom="auto",
                            height="auto", right="120px", left="auto", width="200px",
                            h4("Map Controls",class="panel-heading"),
                            selectizeInput(inputId="MapValues", label="Data to Map", choices=c("Nothing Really"="nothing",
                                                                                             "Species Numbers"="richness",
                                                                                              "Individual Species"="individual")
                            ),
                            
                            div(id="SpeciesControls", 
                               selectizeInput(inputId="MapSpecies",choices=NULL,label="Species"), #updated in server.r
                               selectizeInput(inputId="SpeciesValues", label="Data", 
                                              choices=c("Visit 1", "Visit 2", "Maximum Observed"))
                            ),
                            
                            
                            sliderInput(inputId="MapYear", label="Select a year:", min=2007,max=2014,
                                         value=2014, step=1,sep="",round=TRUE)
                 ),
                 
                 
                 
                 
                 
                 
                 fixedPanel(class="panel panel-primary controls", draggable=TRUE, cursor="auto", top="85%", bottom="auto",
                            height="auto", left="120px", width="180px",
                            strong("Base Layer"),
                            radioButtons(inputId="MapBase",label=NULL,choices=c("Map","Imagery"), selected="Map", inline=TRUE)
                 )
                  
        ),
        tabPanel("Map Data", h2("test2"))
)