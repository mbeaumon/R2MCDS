#' @export
#'@title Plot a detection histogram of apparent density.
#'
#'
#'@description This function will allow the user to visualise the histogram of observations in function of distance class.
#'@param dataset  A \code{\link{data.frame}} containing observations.
#'@param count which column in the data.frame hold the information for the cluster size.
#'@param dist.class which column in the data.frame hold the information for the distance class.
#'@param keep.class which class of observations should be used for the analysis.
#'@param breaks what are the intervals for the distance analysis.
#'@param color fill color for the bars
#'@param rescale set the height of the first bin
#'@details
#' Make a histogram of the observations in function of the distance class
#'@section Author:Christian Roy
#'@examples
#'###Import the data
#'data(alcidae)
#'
#'###Check the naïve detection histogram for alcidae
#'observation_hist(alcidae, count="Count", dist.class="Distance", keep.class=c("A", "B", "C", "D"), breaks=c(0,50,100,200,300), color="white")
#'#END


observation_hist <- function(dataset, count, dist.class, keep.class, breaks, color="white", rescale=NA){
    
    ##Keep only the distance class desired
    dataset <- droplevels(dataset[dataset[,dist.class]%in%keep.class,])
    ## Make sure to transform groups into individuals
    Observations <- rep(dataset[,dist.class],as.numeric(dataset[,count]))
    levels(Observations) <- sapply(2:length(breaks), function(i){ mean(c(breaks[i-1],breaks[i]))})
    ##Make histogram df
    d <- as.numeric(as.character(Observations))
    h <- hist(d, breaks = breaks, plot =FALSE)
    if( !is.na(rescale)){
      h$density <- h$density*rescale/h$density[1]
    }
    r <- data.frame(h[2:4], xmin = head(h$breaks, -1), xmax = h$breaks[-1])
    #Make graph
    ggplot() +
      geom_rect(data=r,aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = density),colour="black", fill=color) +
      scale_y_continuous(limits=c(0,max(h$density))) +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) 
  }

