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
#'### Import and filter data
#'data(alcidae)
#'alcids<-filterECSAS(alcidae)
#'##END
NULL

#'@title sample dataset for the distance analysis
#'
#'@description A simulated sample dataset based on the observation made in the Eastern Canada Seabirds At Sea (ECSAS) program in the Gulf of St-Lawrence.
#'@format  A data frame with 281 observations on the following 19 variables.
#'  \describe{
#'    \item{\code{CruiseID}}{unique cruise ID.}
#'    \item{\code{Start.Date}}{start date of the cruise.}
#'    \item{\code{End.Date}}{end date of the cruise.}
#'    \item{\code{WatchID}}{unique 5-min watch ID.}
#'    \item{\code{Observer1}}{name of first Observer.}
#'    \item{\code{Date}}{date of the watch.}
#'    \item{\code{StartTime}}{start time of the watch.}
#'    \item{\code{EndTime}}{end time of the watch.}
#'    \item{\code{LatStart}}{latitude at start of the watch, decimal degrees.}
#'    \item{\code{LongStart}}{longitude at start of the watch, decimal degrees.}
#'    \item{\code{WatchLenKm}}{distance travelled during the watch in kilometers.}
#'    \item{\code{Snapshot}}{whether snapshot method was used or not for flying birds.}
#'    \item{\code{Alpha}}{four letter code of the species.}
#'    \item{\code{English}}{English name.}
#'    \item{\code{Latin}}{scientific name.}
#'    \item{\code{Distance}}{Classes of distances for the observations. A=0-50m, B=50-100m, C=100-200m, D=200-300m.}
#'    \item{\code{InTransect}}{Are the observations made in the transect or outside of the transect}
#'    \item{\code{Count}}{number of individuals.}
#'    \item{\code{Year}}{Year the observations were made.}
#'  }
#'
#'@seealso \code{\link{filterECSAS}}
#'@docType data
#'@name alcidae
#'@examples
#'data(alcidae)
#'###Check the na?ve detection histogram
#'hist.wrap(alcidae,Trn.Value="Count", Dist.class="Distance", Keep.class=c("A", "B", "C", "D"),
#'          Breaks=c(0,50,100,200,300), Trn.ID="WatchID",  Trn.length="WatchLenKm", Resume=F)
#'#END
NULL

#'@title sample dataset for the distance analysis.
#'
#'@description A simulated sample dataset based on the observation made in the Eastern Canada Seabirds At Sea (ECSAS) program in the Gulf of St-Lawrence.
#'@format A data frame with 7360 observations on the following 19 variables.
#'  \describe{
#'    \item{\code{CruiseID}}{unique cruise ID.}
#'    \item{\code{Start.Date}}{start date of the cruise.}
#'    \item{\code{End.Date}}{end date of the cruise.}
#'    \item{\code{WatchID}}{unique watch ID.}
#'    \item{\code{Observer1}}{name of first Observer.}
#'    \item{\code{Date}}{date of the watch.}
#'    \item{\code{StartTime}}{start time of the watch.}
#'    \item{\code{EndTime}}{end time of the watch.}
#'    \item{\code{LatStart}}{latitude at start of the watch, decimal degrees.}
#'    \item{\code{LongStart}}{longitude at start of the watch, decimal degrees.}
#'    \item{\code{WatchLenKm}}{distance travelled during the watch in kilometers (estimated from platform speed and observation length since many watches don't have start and end positions).}
#'    \item{\code{Snapshot}}{whether snapshot method was used or not.}
#'    \item{\code{Alpha}}{four letter code of the species.}
#'    \item{\code{English}}{English name.}
#'    \item{\code{Latin}}{scientific name.}
#'    \item{\code{Distance}}{Distance to bird (in meters).}
#'    \item{\code{InTransect}}{Are the observations made in the transect or outside of the transect}
#'    \item{\code{Count}}{number of individuals.}
#'    \item{\code{Year}}{Year the observations were made.}
#'  }
#'@docType data
#'@name quebec
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