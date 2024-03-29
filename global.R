#### Network Specific Settings ####

Network<-"ERMN"

NetworkURL<-switch(Network,
                   ERMN=, ERMN2007=, GULN=, MIDN=, NCRN=, NETN = , NCBN= paste0('https://www.nps.gov/im/',tolower(Network),'/index.htm')
)

Years<-switch(Network,
              ERMN=list(Start=2011, End=2018),
              ERMN2007=list(Start=2007, End=2012),
              #GULN=list(Start=NA, End=NA),
              MIDN=list(Start=2009, End=2020),
              NCRN=list(Start=2007, End=2017),
              NETN=list(Start=2006, End=2019),
              NCBN=list(Start=2015, End=2019)
)

## Note - first option listed for ExtraLayers will be chosen by default - "None" is highly recommended.
ExtraLayers<-switch(Network,
                    ERMN=c(None="None"),
                    ERMN2007=c(None="None"),
                    #GULN=c(None="None"),
                    MIDN=c(None="None"),
                    NCRN=c(None="None", "Ecoregions"="Ecoregions","Forested Areas"="Forested"),
                    NETN=c(None="None"),
                    NCBN=c(None="None")
)

ProjectInfo<-switch(Network,
                    ERMN=includeHTML("./www/ERMN/ProjectInfo_ERMN.html"),
                    ERMN2007=includeHTML("./www/ERMN2007/ProjectInfo_ERMN2007.html"),
                    #GULN=includeHTML("<h1>Add Me!</h1>"),
                    MIDN=includeHTML("./www/MIDN/ProjectInfo.html"),
                    NCRN=includeHTML("./www/NCRN/ProjectInfo.html"),
                    NETN=includeHTML("./www/NETN/ProjectInfo.html"),
                    NCBN=includeHTML("./www/NCBN/ProjectInfo.html")
)

Citations<-switch(Network,
                  ERMN=includeHTML("./www/ERMN/Citations_ERMN.html"),
                  ERMN2007=includeHTML("./www/ERMN2007/Citations_ERMN2007.html"),
                  # GULN=includeHTML("<h1>Add Me!</h1>"),
                  MIDN=includeHTML("./www/MIDN/Citations.html"),
                  NCRN=includeHTML("./www/NCRN/Citations.html"),
                  NETN=includeHTML("./www/NETN/Citations.html"),
                  NCBN=includeHTML("./www/NCBN/Citations.html")
)
