## ----warning=FALSE,message=FALSE-----------------------------------------
library(R2MCDS)
###set seed for reproductibility    
set.seed(062)
### Import and filter data
data(alcidae)
alcids <- mcds.filter(alcidae, transect.id = "WatchID", distance.field = "Distance", distance.labels = c("A", "B", "C", "D"), 
                          distance.midpoints = c(25, 75, 150, 250), effort.field = "WatchLenKm", lat.field = "LatStart", 
                          long.field = "LongStart", sp.field = "Alpha", date.field = "Date")

### Run analysis with the MCDS engine. Here, the WatchID is used as the sample.
x <- mcds.wrap(alcids,SMP_EFFORT="WatchLenKm",DISTANCE="Distance",SIZE="Count",
                   units=list(Type="Line",Distance="Perp",Length_units="Kilometers",
                              Distance_units="Meters",Area_units="Square kilometers"),
                   factor = c("ObserverName"), monotone = "none",
                   breaks=c(0,50,100,200,300), SMP_LABEL="WatchID",
                   STR_LABEL="STR_LABEL",STR_AREA="STR_AREA",
                   path="c:/temp/distance",
                   pathMCDS="C:/Distance 6",verbose=FALSE)
#Look at the output
x

## ------------------------------------------------------------------------
#Keep only the best model basec on AICc and look at the output
x.best <- keep.best.model(x)
summary(x.best)
predicted_hist((x.best))

