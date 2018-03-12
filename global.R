#### Network Specific Settings ####

Network<-"NCRN"

NetworkURL<-switch(Network,
                   ERMN=, GULN, MIDN=, NCRN=, NETN = paste0('https://science.nature.nps.gov/im/units/',tolower(Network),'/index.cfm')
)

Years<-switch(Network,
              ERMN=list(Start=NA, End=NA),
              GULN=list(Start=NA, End=NA),
              MIDN=list(Start=NA, End=NA),
              NCRN=list(Start=2007, End=2017),
              NETN=list(Start=NA, End=NA)
)

ExtraLayers<-switch(Network,
                    ERMN=c(None="None"),
                    GULN=c(None="None"),
                    MIDN=c(None="None"),
                    NCRN=c(None="None", "EcoRegions"="EcoReg","Forested Areas"="ForArea"),
                    NETN=c(None="None")
)

ProjectInfo<-switch(Network,
                    ERMN=includeHTML("<h1>Add Me!</h1>"),
                    GULN=includeHTML("<h1>Add Me!</h1>"),
                    MIDN=includeHTML("<h1>Add Me!</h1>"),
                    NCRN=includeHTML("./www/ProjectInfo.html"),
                    NETN=includeHTML("<h1>Add Me!</h1>")
)

Citations<-switch(Network,
                  ERMN=includeHTML("<h1>Add Me!</h1>"),
                  GULN=includeHTML("<h1>Add Me!</h1>"),
                  MIDN=includeHTML("<h1>Add Me!</h1>"),
                  NCRN=includeHTML("./www/Citations.html"),
                  NETN=includeHTML("<h1>Add Me!</h1>")
)