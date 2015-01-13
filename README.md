GeoAviR
=======
The package allows the user to create spatial grids, plot the observed animal density and run some basic distance analysis via the MCDS engine of Distance 6.2.
Version 0.7.1
=======
* Created the strip.wrap function to perform strip.transects
* Made extensive change to distance.wrap to improve rare species estimation
* Added a function to extract information about the cluster size estimaton for distance.wrap
* Improved the print and summary functions for the distanceFit objects
* Created distanceList class
* Added print and summary function for the distanceList objects

Version 0.7.0
=======
* hist.wrap now use ggplot2 to make the distance histogram
* all unmarked dependency were deleted
* MCDS functions output are now numeric
* resume.plot is deprecated
* Global.summary replace resume.plot
* distance.wrap output has been modified to accomodate change for Global.summary
* ECSASfilter and Distancefilter bugs have been fixed
* Added print and summary function for distanceFit objects
