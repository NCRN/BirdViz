library(shiny)
library(leaflet)
navbarPage(title="Bird Test", position = "static-top",inverse=TRUE, collapsible = FALSE, fluid=TRUE, windowTitle = "NCRN Birds",
          
        tabPanel(title="Map", 
                 
                 div(class="outer",
                     tags$head(includeCSS("./www/mapstyles.css") ), # defines css file
                 leafletOutput("BirdMap", width = "100%", height = "100%") #,width = "100%", height = "100%")
                 )
                 
                 
                 
                 
                 ),
        tabPanel("test2", h2("test2"))
)