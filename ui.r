library(shiny)
library(leaflet)
navbarPage(title="Bird Test", position = "static-top",inverse=TRUE, collapsible = FALSE, fluid=TRUE, windowTitle = "NCRN Birds",
          
        tabPanel(title="Map", 
                 
                  div(class="outer",
                      tags$head(includeCSS("./www/mapstyles.css") ), # defines css file that lets map take up whole screen
                  leafletOutput("BirdMap", width = "100%", height = "100%")
                  ),
                 
                 
                 fixedPanel(id="controls", class="panel panel-primary", draggable = TRUE, cursor="auto", top="170px", bottom="auto",
                            height="auto", right="120px", left="auto", width="200px", color="green",
                            h4("Map Controls",class="panel-heading"),
                            selectizeInput(inputId="mapValues", label="Data to Map", choices=c("Nothing Really"="nothing",
                                                                                             "Species Numbers"="richness"))
                 )
        ),
        tabPanel("test2", h2("test2"))
)