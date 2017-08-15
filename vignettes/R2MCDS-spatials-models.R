## ------------------------------------------------------------------------
library(R2MCDS)
library(rgdal)

### Import and filter the observation data
data(quebec)
df1 <- mcds.filter(quebec, transect.id = "WatchID", distance.field = "Distance", distance.labels = c("A", "B", "C", "D"), 
                     distance.midpoints = c(25, 75, 150, 250), effort.field = "WatchLenKm", lat.field = "LatStart", 
                     long.field = "LongStart", sp.field = "Alpha", date.field = "Date")
### Build a shapefile with transect starts
transect <- data.frame(lat=df1$LatStart,lon=df1$LongStart)
coordinates(transect) <- ~lon + lat
transect<-SpatialPointsDataFrame(transect,data=df1[,"Count",drop=FALSE])
proj4string(transect)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

## ------------------------------------------------------------------------
### Import data and transform into a a spatial polygon
data(zonegulf)
zonegulf<-spTransform(zonegulf,CRS(proj4string(transect)))

#overlay transect and Zones
transect_id <- over(transect,zonegulf)
df1$zone <- transect_id$id
df1$zone_area<- transect_id$area
df1 <- df1[!is.na(df1$zone),]


## ------------------------------------------------------------------------
### Build labels for samples. (zone +date)
df1$SMP_LABEL<-paste(df1$zone,df1$Date,sep="_")
### Aggregate effor by Sample unit
effort <- aggregate(WatchLenKm~SMP_LABEL,data=unique(df1[,c("SMP_LABEL","WatchID","WatchLenKm")]),sum)
names(effort)[2] <- "SMP_EFFORT"
df1 <- merge(df1,effort,sort=FALSE)

## ------------------------------------------------------------------------
###Run the analysis for the Gull species only
mod1 <- mcds.wrap(df1,SMP_LABEL="SMP_LABEL",SMP_EFFORT="SMP_EFFORT",
                   DISTANCE="Distance",SIZE="Count",
                   units=list(Type="Line",Distance="Perp",Length_units="Kilometers",
                              Distance_units="Meters",Area_units="Square kilometers"),
                   breaks=c(0,50,100,200,300), estimator=list(c("HA","CO")),
                   detection="All",lsub=list(Alpha=c("BLKI")),split=TRUE,empty=NULL,
                   STR_LABEL="SMP_LABEL",STR_AREA="zone_area",stratum="STR_LABEL",
                   path="c:/temp/distance",
                   pathMCDS="C:/Distance 6",verbose=FALSE)
mod1
summary(mod1)
predicted_hist((mod1))


