## ----warning=FALSE,message=FALSE,fig.height=5,fig.width=5,fig.align='center'----
library(GeoAviR)

### Import and filter data
data(quebec)
d <- distance.filter(quebec, transect.id = "WatchID", distance.field = "Distance", 
                     distance.labels = c("A", "B", "C", "D"), 
                     distance.midpoints = c(25, 75, 150, 250), 
                     effort.field = "WatchLenKm", lat.field = "LatStart", 
                     long.field = "LongStart", sp.field = "Alpha", date.field = "Date")

### Run analysis with the MCDS engine. Here, the WatchID is used as the sample.
x<-distance.wrap(d,SMP_EFFORT="WatchLenKm",DISTANCE="Distance",SIZE="Count",
                 units=list(Type="Line",Distance="Perp",Length_units="Kilometers",
                            Distance_units="Meters",Area_units="Square kilometers"),
                 breaks=c(0,50,100,200,300), estimator=list(c("HN","CO")),
                 lsub=list(Alpha=c("BLKI","GBBG", "HERG")), split=TRUE,
                 STR_LABEL="STR_LABEL",STR_AREA="STR_AREA",SMP_LABEL="WatchID",
                 path="c:/temp/distance",
                 pathMCDS="C:/Distance 6",verbose=FALSE)
x
#output for the Great Black-backed Gull 
summary(x[[2]])
predicted_hist(x[[2]])
##output for the Herring gull
summary(x[[1]])
predicted_hist(x[[1]])


## ----warning=FALSE,message=FALSE-----------------------------------------
#'### 
library(GeoAviR)

### Import and filter data
data(quebec)
d <- distance.filter(quebec, transect.id = "WatchID", distance.field = "Distance", distance.labels = c("A", "B", "C", "D"), 
                     distance.midpoints = c(25, 75, 150, 250), effort.field = "WatchLenKm", lat.field = "LatStart", 
                     long.field = "LongStart", sp.field = "Alpha", date.field = "Date")

### Run analysis with the MCDS engine. Here, the WatchID is used as the sample.
x<-distance.wrap(d,SMP_EFFORT="WatchLenKm",DISTANCE="Distance",SIZE="Count",
                 units=list(Type="Line",Distance="Perp",Length_units="Kilometers",
                            Distance_units="Meters",Area_units="Square kilometers"),
                 breaks=c(0,50,100,200,300), estimator=list(c("HN","CO")),
                 lsub=list(Alpha=c("HERG")), rare= list(Alpha=c("RBGU")), split=TRUE,
                 STR_LABEL="STR_LABEL",STR_AREA="STR_AREA",SMP_LABEL="WatchID",
                 path="c:/temp/distance",
                 pathMCDS="C:/Distance 6",verbose=FALSE)
x
##output for the Ring-billed Gull
summary(x)

