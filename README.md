GeoAviR
=======
The package allows the user to create spatial grids, plot the observed animal density and run some basic distance analysis via the MCDS engine of Distance 6.2.

Version 0.9.0
=======
* Replaced hist.wrap with "observation_hist" and "predicted_hist" 
* Removed the gridding functions
* Removed the plotting functions
* Removed filterECSAS
* Updated vignette

Version 0.8.3
=======
* Fixed a bug in the strip transect function

Version 0.8.2
=======
* Improved the vignettes
* Anonymised the alcidae database

Version 0.8.1
=======
* Made changes to the vignettes
* Changed the example in distance.wrap
* Added code to foolproof the inputs of distance.wrap

Version 0.8.0
=======
* Added a vignette for model with covariates
* Added a vignette for simple models
* Added a vignette for stratified analysis
* Added the keep.best.model function to simplify output when many model are fitted to the same species
* Made cosmetic changes to the print and summary functions for the distanceList and distanceFit class
* Created the SpeciesList class 
* Redefined the distanceList class
* Added the units options to distance.wrap
* Made extensive change to distance.wrap to make sure each model is estimated independently

Version 0.7.2
=======
* Simplifed the help of distance.wrap
* Added the names of the covariates to the print.summary function
* Added the names of the covariates to the distance.wrap output
* Created a function to extract the names of the covariates in the distance output

Version 0.7.1
=======
* Created the strip.wrap function to perform strip.transects analysis
* Made modifications to distance.wrap to allow the usage of explanatory variables to compute detection functions
* Made extensive modifications to distance.wrap to improve rare species estimation
* Added a function to extract information about the cluster size estimaton by distance.wrap
* Improved the print and summary functions for the distanceFit objects
* Created distanceList class
* Added print and summary functions for the distanceList objects

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
