#### Network Specific Settings ####

Network<-"NETN"

NetworkURL<-switch(Network,
                   ERMN=, GULN=, MIDN=, NCRN=, NETN = paste0('https://science.nature.nps.gov/im/units/',tolower(Network),'/index.cfm')
)

Years<-switch(Network,
              ERMN=list(Start=2014, End=2018),
              #GULN=list(Start=NA, End=NA),
              MIDN=list(Start=2010, End=2018),
              NCRN=list(Start=2007, End=2018),
              NETN=list(Start=2006, End=2018)
)

## Note - first option listed for ExtraLayers will be chosen by default - "None" is highly recommended.
ExtraLayers<-switch(Network,
                    ERMN=c(None="None"),
                    #GULN=c(None="None"),
                    MIDN=c(None="None"),
                    NCRN=c(None="None", "Ecoregions"="Ecoregions","Forested Areas"="Forested"),
                    NETN=c(None="None")
)

ProjectInfo<-switch(Network,
                    ERMN=includeHTML("./www/ERMN/ProjectInfo_ERMN.html"),
                    #GULN=includeHTML("<h1>Add Me!</h1>"),
                    MIDN=includeHTML("./www/MIDN/ProjectInfo.html"),
                    NCRN=includeHTML("./www/NCRN/ProjectInfo.html"),
                    NETN=includeHTML("./www/NETN/ProjectInfo.html")
)

Citations<-switch(Network,
                  ERMN=includeHTML("./www/ERMN/Citations_ERMN.html"),
                  # GULN=includeHTML("<h1>Add Me!</h1>"),
                  MIDN=includeHTML("./www/MIDN/Citations.html"),
                  NCRN=includeHTML("./www/NCRN/Citations.html"),
                  NETN=includeHTML("./www/NETN/Citations.html")
)