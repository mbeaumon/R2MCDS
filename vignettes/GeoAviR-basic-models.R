## ----warning=FALSE,message=FALSE-----------------------------------------
library(GeoAviR)
### Import and filter data
data(alcidae)
alcids<-distance.filter(alcidae, transect.id = "WatchID", distance.field = "Distance", 
                        distance.labels = c("A", "B", "C", "D"), 
                        distance.midpoints = c(25, 75, 150, 250), effort.field = "WatchLenKm", 
                        lat.field = "LatStart", long.field = "LongStart", 
                        sp.field = "Alpha", date.field = "Date")

### Run analysis with the MCDS engine. Here, the WatchID is used as the sample.
x<-distance.wrap(alcids, SMP_EFFORT="WatchLenKm",DISTANCE="Distance",SIZE="Count",
                 units=list(Type="Line",Distance="Perp",Length_units="Kilometers",
                            Distance_units="Meters",Area_units="Square kilometers"),
                 breaks=c(0,50,100,200,300), STR_LABEL="STR_LABEL", 
                 STR_AREA="STR_AREA",SMP_LABEL="WatchID", 
                 path="c:/temp/distance",
                 pathMCDS="C:/Distance 6",verbose=FALSE)

## ----warning=FALSE,message=FALSE,fig.height=5,fig.width=5,fig.align='center'----
x
####summary of one model
summary(x[[2]])
predicted_hist(x[[2]])

## ----warning=FALSE,message=FALSE,fig.height=5,fig.width=5,fig.align='center'----
##### Keep the 'best' model in the list
x.best <- keep.best.model(x)
summary(x.best)
predicted_hist(x.best)

## ----warning=FALSE,message=FALSE-----------------------------------------
library(GeoAviR)
### Import and filter data
data(alcidae)
alcids<-distance.filter(alcidae, transect.id = "WatchID", distance.field = "Distance", 
                        distance.labels = c("A", "B", "C", "D"), 
                        distance.midpoints = c(25, 75, 150, 250), effort.field = "WatchLenKm", 
                        lat.field = "LatStart", long.field = "LongStart", 
                        sp.field = "Alpha", date.field = "Date")

### Run analysis with the MCDS engine. Here, the WatchID is used as the sample.
x<-distance.wrap(alcids,SMP_EFFORT="WatchLenKm",DISTANCE="Distance",SIZE="Count",
                 units=list(Type="Line",Distance="Perp",Length_units="Kilometers",
                            Distance_units="Meters",Area_units="Square kilometers"),
                 breaks=c(0,50,100,200,300), SMP_LABEL="WatchID",
                 STR_LABEL="STR_LABEL",STR_AREA="STR_AREA",
                 estimator=list(c("HN","CO"),c("HA","PO")),
                 path="c:/temp/distance",
                 pathMCDS="C:/Distance 6",verbose=FALSE)
x
summary(x[[2]])

## ----warning=FALSE,message=FALSE,fig.height=5,fig.width=5,fig.align='center'----
##### Keep the 'best' model in the list
x.best <- keep.best.model(x)
summary(x.best)
predicted_hist(x.best)

