#'@title Calling  Distance 6.2 MCDS engine from R.
#'
#'@description This package will allow the user to call the MCDS engine from Distance 6.2 in the R environment.
#'
#'@name R2MCDS
#'@docType package
#'@details
#'\tabular{ll}{
#'Package: \tab GeoAviR\cr
#'Type: \tab Package\cr
#'Version: \tab 0.8.3\cr
#'Date: \tab 2015-10-26\cr
#'License: \tab GPL-2\cr
#'LazyLoad: \tab yes\cr
#'}
#'@references
#'S.T. Buckland, D.R. Anderson, K.P. Burnham, J.L Laake, D.L. Borchers and L. Thomas. 2001. \emph{Introduction to Distance Sampling}. Estimating abundance of biological populations. Oxford University Press.\cr
#'Royle, J. A., D. K. Dawson, and S. Bates. 2004. \emph{Modeling abundance effects in distance sampling}. Ecology 85:1591-1597\cr
#'Fiske, I. and R. B. Chandler. 2011. \emph{unmarked: An R package for fitting hierarchical models of wildlife occurrence and abundance}. Journal of Statistical Software 43:1-23.\cr 
#'Thomas, L., S.T. Buckland, E.A. Rexstad, J. L. Laake, S. Strindberg, S. L. Hedley, J. R.B. Bishop, T. A. Marques, and K. P. Burnham. 2010.  Distance software: design and analysis of distance sampling surveys for estimating population size.  Journal of Applied Ecology 47: 5-14.
#'@examples
#'########################################
#'### Simple models without stratification
#'### Import and filter data
#'data(alcidae)
#'alcids <- mcds.filter(alcidae, transect.id = "WatchID", distance.field = "Distance", distance.labels = c("A", "B", "C", "D"), 
#'                          distance.midpoints = c(25, 75, 150, 250), effort.field = "WatchLenKm", lat.field = "LatStart", 
#'                          long.field = "LongStart", sp.field = "Alpha", date.field = "Date") 
#'
#'### Run analysis with the MCDS engine. Here, the WatchID is used as the sample.
#'dist.out1 <- mcds.wrap(alcids, SMP_EFFORT="WatchLenKm",DISTANCE="Distance",SIZE="Count",
#'                          units=list(Type="Line",Distance="Perp",Length_units="Kilometers",
#'                                     Distance_units="Meters",Area_units="Square kilometers"),
#'                          breaks=c(0,50,100,200,300), estimator=list(c("HN","CO")),
#'                          STR_LABEL="STR_LABEL", STR_AREA="STR_AREA",SMP_LABEL="WatchID", 
#'                          path="c:/temp/distance",
#'                          pathMCDS="C:/Distance 6",verbose=FALSE)
#'
#'summary(dist.out1)
#'##END
NULL

#'@title sample dataset distance analysis
#'
#'@description A sample dataset containing observation of alcidae made in the Eastern Canada Seabirds At Sea (ECSAS) program.
#'@format  A data frame with 1670 observations on the following 14 variables.
#'  \describe{
#'    \item{\code{CruiseID}}{unique cruise ID.}
#'    \item{\code{WatchID}}{unique watch ID.}
#'    \item{\code{Observer}}{Categorical variable identifying the different observers.}
#'    \item{\code{Date}}{date of the watch.}
#'    \item{\code{StartTime}}{start time of the watch.}
#'    \item{\code{EndTime}}{end time of the watch.}
#'    \item{\code{LatStart}}{latitude at start of the watch, decimal degrees.}
#'    \item{\code{LongStart}}{longitude at start of the watch, decimal degrees.}
#'    \item{\code{WatchLenKm}}{distance travelled during the watch in kilometers (estimated from platform speed and observation length since many watches don't have start and end positions).}
#'    \item{\code{Alpha}}{four letter code of the species.}
#'    \item{\code{English}}{English name.}
#'    \item{\code{Latin}}{scientific name.}
#'    \item{\code{Distance}}{Classes of distances for the observations. A=0-50m, B=50-100m, C=100-200m, D=200-300m.}
#'    \item{\code{Count}}{number of individuals.}
#'  }
#'
#'@seealso \code{\link{mcds.filter}}
#'@docType data
#'@name alcidae
NULL

#'@title sample dataset for distance analysis.
#'
#'@description A sample dataset containing observation of seabirds made in the Eastern Canada Seabirds At Sea (ECSAS) programin the Gulf of St-Lawrence.
#'@format A data frame with 1291 observations on the following 14 variables.
#'  \describe{
#'    \item{\code{CruiseID}}{unique cruise ID.}
#'    \item{\code{WatchID}}{unique watch ID.}
#'    \item{\code{Observer}}{Categorical variable identifying the different observers.}
#'    \item{\code{Date}}{date of the watch.}
#'    \item{\code{StartTime}}{start time of the watch.}
#'    \item{\code{EndTime}}{end time of the watch.}
#'    \item{\code{LatStart}}{latitude at start of the watch, decimal degrees.}
#'    \item{\code{LongStart}}{longitude at start of the watch, decimal degrees.}
#'    \item{\code{WatchLenKm}}{distance travelled during the watch in kilometers (estimated from platform speed and observation length since many watches don't have start and end positions).}
#'    \item{\code{Alpha}}{four letter code of the species.}
#'    \item{\code{English}}{English name.}
#'    \item{\code{Latin}}{scientific name.}
#'    \item{\code{Distance}}{Classes of distances for the observations. A=0-50m, B=50-100m, C=100-200m, D=200-300m.}
#'    \item{\code{Count}}{number of individuals.}
#'  }
#'@seealso \code{\link{mcds.filter}}
#'@docType data
#'@name quebec
NULL


#'@title sample dataset for distance analysis.
#'
#'@description A sample dataset containing observation of alcidae made in the Eastern Canada Seabirds At Sea (ECSAS) program.
#'@format A data frame with 583 observations on the following 14 variables.
#'  \describe{
#'    \item{\code{CruiseID}}{unique cruise ID.}
#'    \item{\code{WatchID}}{unique watch ID.}
#'    \item{\code{Observer}}{Categorical variable identifying the different observers.}
#'    \item{\code{Date}}{date of the watch.}
#'    \item{\code{StartTime}}{start time of the watch.}
#'    \item{\code{EndTime}}{end time of the watch.}
#'    \item{\code{LatStart}}{latitude at start of the watch, decimal degrees.}
#'    \item{\code{LongStart}}{longitude at start of the watch, decimal degrees.}
#'    \item{\code{WatchLenKm}}{distance travelled during the watch in kilometers (estimated from platform speed and observation length since many watches don't have start and end positions).}
#'    \item{\code{Alpha}}{four letter code of the species.}
#'    \item{\code{English}}{English name.}
#'    \item{\code{Latin}}{scientific name.}
#'    \item{\code{Distance}}{Classes of distances for the observations. A=0-50m, B=50-100m, C=100-200m, D=200-300m.}
#'    \item{\code{Count}}{number of individuals.}
#'  }
#'@seealso \code{\link{mcds.filter}}
#'@docType data
#'@name laridae
NULL


#'@title Zones in the Gulf of St-Lawrence.
#'
#'@description This file gives the boundaries of fishing zones in the Gulf of St-Lawrence.
#'@format  A SpatialPolygonsDataFrame.
#'  \describe{
#'    \item{\code{id}}{the global polygon to which the point is}
#'    \item{\code{area}}{size of the fishing zone in km}
#'  }
#'@docType data
#'@name zonegulf
NULL