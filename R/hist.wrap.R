#' @export
#'@title Plot a detection histogram of apparent density.
#'
#'
#'@description This function will allow the user to visualise the histogram of observations in function of distance class and re-bin his observations if necessary before the analysis.
#'@param Dataset  A \code{\link{data.frame}} containing observations.
#'@param Count which column in the data.frame hold the information for the cluster size.
#'@param Dist.class which column in the data.frame hold the information for the distance class.
#'@param Keep.class which class of observations should be used for the analysis.
#'@param Breaks what are the intervals for the distance analysis.
#'@color fill color for the bars
#'@details
#' Make a histogram of the observations in function of the distance class
#'@section Author:Christian Roy
#'@examples
#'###Import the data
#'data(alcidae)
#'
#'###Check the naïve detection histogram
#'hist.wrap(alcidae, Count="Count", Dist.class="Distance", Keep.class=c("A", "B", "C", "D"), Breaks=c(0,50,100,200,300), color="white")
#'#END


hist.wrap <-
  function(Dataset, Count, Dist.class, Keep.class, Breaks, color="white"){

    ##Keep only the distance class desired
    Dataset <- droplevels(Dataset[Dataset[,Dist.class]%in%Keep.class,])
    ## Make sure to transform groups into individuals
    Observations <- rep(Dataset[,Dist.class],as.numeric(Dataset[,Count]))
    levels(Observations) <- sapply(2:length(Breaks), function(i){ mean(c(Breaks[i-1],Breaks[i]))})
    ##Make histogram df
    d <- as.numeric(as.character(Observations))
    h <- hist(d, breaks = Breaks, plot =FALSE)
    h$density <-  h$density/sum(h$density, na.rm=T)
    r <- data.frame(h[2:4], xmin = head(h$breaks, -1), xmax = h$breaks[-1])
    #Make graph
    ggplot(r, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = density)) +
      geom_rect(colour="black", fill=color) +
      scale_y_continuous(limits=c(0,1)) +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) 
  }

