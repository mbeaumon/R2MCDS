#' @export
#'@title Fit the distance sampling model provided by the Distance software.
#'
#'
#'@description This function fits the distance sampling model provided by the Distance program using its MCDS engine.

#'@param dataset A \code{\link{data.frame}} with observations.

#'@param path The path where to store the input and output files of the MCDS engine.

#'@param pathMCDS The path where the MCDS engine is located.

#'@param STR_LABEL Name of the column to use as a stratum label.

#'@param STR_AREA Name of the column to use for the stratum area.

#'@param SMP_LABEL Name of the column to use for the transect/watch label.

#'@param units list of th units used for the analysis. Contains the Type of analysis (Line or Point), the Distance enfine to use (Perp or Radial), 
#'the Length units, the Distance units, and the Area_units.For the possible units of distance and area see Distance 6.2 documentation.     

#'@param SMP_EFFORT Length in km of the transect or the transect/watch unit.

#'@param SIZE Number of individuals in the observation.

#'@param breaks Interval in meters to be used in the analysis. 

#'@param lsub A named list giving the subsets to be used in the analysis. The names of the list are the names of columns used to subset the dataset.
#'Each element of the list is a vector giving the values to keep in the analysis for a given column. When a vector is \code{NULL},
#'all values are kept for this column. Default value \code{lsub = NULL}. See examples for further details.

#'@param stratum When \code{stratum = "STR_LABEL"}, the model will be stratified with the label.
#'When \code{stratum} is the name of a column in the data, the model will be
#'post-stratified according to this column (see Thomas et al. 2010). Default value
#'is \code{NULL}.

#'@param split When \code{split = TRUE}, separate models are ran according to the subsets determined by \code{lsub}. Default value \code{FALSE}.

#'@param period A vector of characters of length 2 containing the extreme dates for which the analysis
#'should be restricted. Dates have to be in the "yyyy-mm-dd" format.

#'@param detection Currently, set to \code{detection = "All"}. Can also be \code{detection = "Stratum"}.

#'@param multiplier Value by which the estimates of density or abundance are multiplied. Default value \code{multiplier = 2} meaning
#'only one-half of the transect is surveyed.

#'@param empty Determine how empty transects are to be selected in the analysis. When \code{empty = NULL}, all
#'empty transects are included for every element of \code{lsub}. For example, when models are splitted according
#'to species, empty transects and transects where a species was not detected need to be considered in the analysis for that species.
#'When \code{lsub} contains geographic or temporal elements, empty transects need to be restricted to the subsets given. In this case,
#'a vector of character has to be given with the names in \code{lsub} for which the empty transects are to be restricted. When \code{split = TRUE}
#'and \code{empty != NULL}, empty transects will be splitted according to the names in \code{empty}. In any case, it is assumed that
#'the \code{dataset} contains at least a line for every transect executed, either with or without an observation. See examples for
#'further details.

#'@param verbose When set to \code{TRUE}, prints the input file given to the MCDS engine. Default value \code{FALSE}.

#'@details
#'Uses the MCDS engine from program Distance. The function produces an input file and submits it to the MCDS engine
#'through the \code{\link{system}} function. The results are then extracted from the output files and returned as a
#'list object.

#'@return
#'An object of class \code{"distanceFit"}, when \code{split = FALSE}. When \code{split = TRUE} and \code{lsub != NULL},
#'a named list with the different models of class \code{"distanceFit"}.

#'Each object of class \code{"distanceFit"} is a named list with components \code{model_fitting}, \code{parameter_estimates},
#'\code{chi_square_test}, \code{density_estimate}, \code{detection} and \code{path}. Elements
#'of the list are accessible through the \code{$} operator. For each component
#'except \code{detection} and \code{path}, a \code{\link{list}} of length 2 is given with
#'component "\code{Global}" and "\code{Stratum}". Depending on the analysis chosen,
#'one of these component will be empty (\code{NULL}).

#'@references
#'Thomas, L., S.T. Buckland, E.A. Rexstad, J. L. Laake, S. Strindberg, S. L. Hedley, J. R.B. Bishop,
#'T. A. Marques, and K. P. Burnham. 2010. \emph{Distance software: design and analysis of distance sampling
#'surveys for estimating population size.} Journal of Applied Ecology 47: 5-14.
#'DOI: 10.1111/j.1365-2664.2009.01737.x

#'@section Author:Francois Rousseu, Christian Roy, Francois Bolduc

#'@examples
#'########################################
#'### Simple models without stratification
#'### Import and filter data
#'data(alcidae)
#'alcids<-filterECSAS(alcidae)
#'
#'### Run analysis with the MCDS engine. Here, the WatchID is used as the sample.
#'strip.out1 <-distance.wrap(alcids, SMP_EFFORT="WatchLenKm",SIZE="Count", breaks=c(0,300),
#'                          units=list(Type="Line",Distance="Perp",Length_units="Kilometers",
#'                                     Distance_units="Meters",Area_units="Square kilometers"),
#'                          STR_LABEL="STR_LABEL", STR_AREA="STR_AREA",SMP_LABEL="WatchID", 
#'                          path="c:/temp/distance",
#'                          pathMCDS="C:/Program Files (x86)/Distance 6")
#'
#'summary(strip.out1)
#'#END

strip.wrap <-
function(dataset,
																								path,
																								pathMCDS,
																								SMP_LABEL="SMP_LABEL",
																								SMP_EFFORT="SMP_EFFORT",
																								SIZE="SIZE",																				
																								STR_LABEL="STR_LABEL",
																								STR_AREA="STR_AREA",	
                                                units=list(Type="Line",Distance="Perp",Length_units="Kilometer",
                                                           Distance_units="Meter",Area_units="Square kilometer"),
																								breaks=c(0,300),  # make change
																								lsub=NULL,
																								stratum=NULL, 
																								split=TRUE,
																								period=NULL,
																								detection="All", #or "stratum" is possible
																								multiplier=2, 
																								empty=NULL,
																								verbose=FALSE
){
	
  #Check for units
  if(length(units)!= 5)
    stop("units list must includes values for Type, Distance, Length_units, Distance_units and Area_units")
  
  if(toupper(units$Type)%in%c("LINE","POINT", "CUE")==FALSE)
    stop("Type of sampling available are: Line, Point or Cue")
  
  if(toupper(units$Distance)%in%c("PERP","RADIAL")==FALSE)
    stop("Type of distance available are: Perp or Radial")
  
  if(toupper(units$Type)%in%c("POINT", "CUE") & toupper(units$Distance)!=c("RADIAL"))
    stop("For Points and Cue sampling scheme Distance must be set to radial")
  
  
  if(toupper(units$Length_units)%in%c("CENTIMETERS", "METERS", "KILOMETERS", "MILES", 
                                      "INCHES", "FEET", "YARDS", "NAUTICAL MILES")==FALSE)
    stop("Distance units must be one of thse: 'Centimeters', 'Meters', 'Kilometers', 'Miles', 'Inches', 
         'Feet', 'Yards', Or 'Nautical Miles'")  
  
  if(toupper(units$Distance_units)%in%c("CENTIMETERS", "METERS", "KILOMETERS", "MILES", 
                                        "INCHES", "FEET", "YARDS", "NAUTICAL MILES")==FALSE)
    stop("Distance units must be one of thse: 'Centimeters', 'Meters', 'Kilometers', 'Miles', 'Inches', 
         'Feet', 'Yards', Or 'Nautical Miles'")  
  
  if(toupper(units$Area_units)%in%c("SQUARE CENTIMETERS", "SQUARE METERS", "SQUARE KILOMETERS", "SQUARE MILES", 
                                    "SQUARE INCHES", "SQUARE FEET", "SQUARE YARDS", "HECTARES")==FALSE)
    stop("Distance units must be one of thse: 'Centimeters', 'Meters', 'Kilometers', 'Miles', 'Inches', 
         'Feet', 'Yards', Or 'Nautical Miles'")  
  
  #get the list of arguments
	arguments<-as.list(environment())
	if(!is.null(period)){
		dataset<-dataset[dataset$Date>=min(period) & dataset$Date<=max(period),]
	}
	
  if(length(breaks)>2)
  stop("strip wrap breaks only inlcude the minimum and maximum distance for the observations")
  
  #Make sure the directory exist to save output
	dir.create(path, recursive = TRUE, showWarnings = FALSE)

	if(!any("STR_AREA"==names(dataset)) && STR_AREA=="STR_AREA"){
		dataset<-cbind(STR_AREA=rep(1,nrow(dataset)),dataset,stringsAsFactors=F)
	}	
	if(!any("STR_LABEL"==names(dataset)) && STR_LABEL=="STR_LABEL"){
		dataset<-cbind(STR_LABEL=rep(1,nrow(dataset)),dataset,stringsAsFactors=F)
	}	
	if(!is.null(stratum)){
		if(STR_LABEL!="STR_LABEL"){
			dataset[,"STR_LABEL"]<-dataset[,STR_LABEL]	
		}
		dataset[,"STR_LABEL"]<-dataset[,stratum] # stratum as predominance over STR_LABEL
		STR_LABEL<-stratum	
	}
	if(!is.null(stratum) && stratum=="STR_LABEL"){
		if(is.null(STR_AREA)){
			stop("No area given (STR_AREA = NULL) and stratum = \"STR_LABEL\"")
		}else{	
			dataset[,"STR_AREA"]<-dataset[,STR_AREA]
		}
	}
	
	if(!is.null(empty) && !is.null(lsub)){
	 if(!all(empty%in%names(lsub))){stop("Names in empty do not correspond to names in lsub")}
	}
  transects<-dataset
  	if(!is.null(lsub)){  
  		for(i in 1:length(lsub)){
  			if(is.null(lsub[[i]])){next}
  			dataset<-dataset[dataset[,names(lsub)[i]]%in%lsub[[i]],]			 
  			if(names(lsub)[i]%in%empty){
  				transects<-transects[transects[,names(lsub)[i]]%in%lsub[[i]],]
  			}			
  		}
  	}

	DISTANCE <-"Distance" 
  dataset[,DISTANCE]<- mean(breaks) 
	transects[,DISTANCE]<-""
	
	######################## add-on
	if(!is.null(lsub) && split){
		if(!is.null(empty)){			
			transects<-dlply(transects,empty)
			dataset<-dlply(dataset,empty)			
			transects<-transects[names(dataset)]
			l<-1:length(dataset)
			names(l)<-names(transects)
			dataset<-llply(l,function(i){
				ans<-dlply(dataset[[i]],setdiff(names(lsub),empty))
			 empty.transects<-transects[[i]]
				n<-names(ans)
				ans<-lapply(ans,function(j){rbind(j,empty.transects)})
			 names(ans)<-n
				ans
			})
		 if(all(names(lsub)%in%empty)){
		 	name<-names(dataset)
		 	dataset<-unlist(dataset,recursive=FALSE,use.names=FALSE)
		  names(dataset)<-name
		 }else{
		 	dataset<-unlist(dataset,recursive=FALSE,use.names=TRUE)
		 }
		}else{
		 dataset<-dlply(dataset,names(lsub))
		 dataset<-llply(dataset,function(i){rbind(i,transects)})
		}
	}else{		
		dataset<-list(rbind(dataset,transects))
	}
	#########################

	dataset<-lapply(dataset,function(i){
	
		res<-i[!is.na(i[,DISTANCE]),] #add1
		good.label<-unique(res[res[,DISTANCE]!="",SMP_LABEL])#add2
		res<-res[(res[,DISTANCE]!="") | (res[,DISTANCE]=="" & !res[,SMP_LABEL]%in%good.label & !duplicated(res[,SMP_LABEL])),]#add3
		res[,DISTANCE]<-ifelse(is.na(res[,DISTANCE]),"",res[,DISTANCE])
		res[,SIZE]<-ifelse(res[,DISTANCE]=="","",res[,SIZE])
		if(all(res[,DISTANCE]=="")){
			res<-NULL
		}
		res
	})
	dataset<-dataset[!sapply(dataset,is.null)] #removes emptys transects when they are marked with "" for the species name

	if(!is.null(names(dataset))){
		names(dataset)<-gsub("\\.","\\_",names(dataset))
		names(dataset)<-gsub("'","",names(dataset))	# this was put for gyre d'anticosti #########
	}
	
	res.file<-paste(paste("results",names(dataset),sep="_"),".tmp",sep="")
	log.file<-paste(paste("log",names(dataset),sep="_"),".tmp",sep="")
	det.file<-paste(paste("detections",names(dataset),sep="_"),".tmp",sep="")
	dat.file<-paste(paste("data",names(dataset),sep="_"),".tmp",sep="")
	inp.file<-paste(paste("input",names(dataset),sep="_"),".tmp",sep="")
	#browser()
	lans<-vector(mode="list",length=length(dataset))
	names(lans)<-names(dataset)
	
	notrun<-NULL
	#browser()
	for(i in seq_along(dataset)){
		dat<-dataset[[i]]
		dat<-dat[!(is.na(dat[,DISTANCE]) | is.na(dat[,SIZE])),]
		#dat<-dat[!(duplicated(dat[,SMP_LABEL]) & dat[,DISTANCE]==""),]
		#######################################################
		### input
		opts<-list()
		opts[""]<-paste(gsub("/","\\\\\\\\",path),"\\\\",res.file[i],sep="")
		opts[""]<-paste(gsub("/","\\\\\\\\",path),"\\\\",log.file[i],sep="")
		opts[""]<-"None"
		opts[""]<-paste(gsub("/","\\\\\\\\",path),"\\\\",det.file[i],sep="")
		opts[""]<-"None"
		opts[""]<-"None"
		opts["Options;"]<-""
		opts["Type="] <- paste(units$Type,";",sep="")
		opts["Length /Measure="] <- paste("'",units$Length_units,"';",sep="")
		if(units$Distance=="Perp"){
		  opts["Distance=Perp /Measure="] <- paste("'",units$Distance_units,"';",sep="")    
		}else{
		  opts["Distance=Radial /Measure="] <- paste("'",units$Distance_units,"';",sep="")    
		}
		opts["Area /Units="] <- paste("'",units$Area_units,"';",sep="")
		opts["Object="]<-"Cluster;"
		opts["SF="]<-"1;"
		opts["Selection="]<-"Specify;"
		opts["Confidence="]<-"95;"
		opts["Print="]<-"Selection;"
		opts["End1;"]<-""
		opts["Data /Structure="]<-"Flat;"
		dat<-dat[,unique(c(STR_LABEL,STR_AREA,SMP_LABEL,SMP_EFFORT,DISTANCE,SIZE))] #the stratum part used to be ifelse(stratum=="STR_LABEL",STR_LABEL,stratum)
		names(dat)[1:6]<-c("STR_LABEL","STR_AREA","SMP_LABEL","SMP_EFFORT","DISTANCE","SIZE")
		
		opts["Fields="]<-paste(paste(names(dat),collapse=", "),";",sep="")
		opts["Infile="]<-paste(paste(gsub("/","\\\\\\\\",path),"\\\\",dat.file[i],sep="")," /NoEcho;",sep="")
		opts["End2;"]<-""
		opts["Estimate;"]<-""
		opts["Distance /Intervals="]<-paste(paste(breaks,collapse=",")," /Width=",max(breaks)," /Left=",min(breaks),";",sep="")
		
		if(is.null(stratum)){
			opts["Density="]<-"All;"
			opts["Encounter="]<-"All;"
			opts["Detection="]<-"All;"
			opts["Size="]<-"All;"
		}else{
			if(stratum=="STR_LABEL"){
				
				opts["Density="]<-"Stratum /DESIGN=strata /WEIGHT=area;" # in post stratify and stratify only, both lines are the same
				opts["Encounter="]<-"Stratum;"
				opts["Detection="]<-paste(detection,";",sep="")
				opts["Size="]<-"All;"
			}else{
				opts["Density="]<-"Stratum /DESIGN=strata /WEIGHT=area;"
				opts["Encounter="]<-"Stratum;"
				opts["Detection="]<-paste(detection,";",sep="")
				opts["Size="]<-"All;"
				opts["Fields="]<-paste(paste(names(dat),collapse=", "),";",sep="")
				
			}
		}	

		opts[["Estimator1"]]<- "/KEY=UNIF /NAP=0 ;"
    opts["Pick="]<-"AICC;"
		opts["GOF;"]<-""
		opts["Cluster /Bias="]<-"GXLOG;"
		opts["VarN="]<-"Empirical;"
		opts["Multiplier1="]<-paste(multiplier," /Label='Sampling fraction';",sep="")
		opts["End3;"]<-""
    
		names(opts)[grep("Estimator",names(opts))]<-"Estimator" #to get the nb out which are used to prevent overwriting previously
		names(opts)[grep("End",names(opts))]<-"End;"
    names(opts)[grep("Multiplier",names(opts))]<-"Multiplier="
		opts<-lapply(opts,paste,collapse="")
		input<-paste(names(opts),unlist(opts),sep="")
		if(verbose){cat(input,fill=1)}  #prints the input file
		write.table(input,file.path(path,inp.file[i]),row.names=FALSE,quote=FALSE,col.names=FALSE)
		dat$SMP_LABEL<-paste(as.numeric(factor(dat$SMP_LABEL)),dat$SMP_LABEL,sep=".")
		dat<-dat[order(dat[,"STR_LABEL"],dat[,"SMP_LABEL"]),] #always order according to the first column/ str_label, otherwise MCDS creates too many stratum or samples
		w<-which(is.na(dat[,"DISTANCE"]) | dat[,"DISTANCE"]=="")
		if(any(w)){
			dat[w,"SIZE"]<-"" #previously NA was given, but gives status 3 with distance
			dat[w,"DISTANCE"]<-"" #previously not here but don't know why
		}
		dat<-dat[dat[,"STR_LABEL"]!="",]#temporary to get rid of stratum when it is species and it is an empty transect, has to be clarified what to do
		write.table(dat,file.path(path,dat.file[i]),na="",sep="\t",row.names=FALSE,col.names=FALSE,quote=FALSE)
		
		####################################################
		### running distance
		
		cmd<-paste(shQuote(file.path(pathMCDS,"MCDS"))," 0, ",shQuote(file.path(path,inp.file[i])),sep="")
		system(cmd,wait=TRUE,ignore.stdout=FALSE,ignore.stderr=FALSE,invisible=TRUE)
		
		####################################################
		### output
		
    ### subset original observations
		input.data <- list()
    input.data[[1]] <- dat
		input.data[[2]] <- breaks
		names(input.data)<-c("observations","breaks")
    
		log<-readLines(file.path(path,log.file[i]))
		res<-readLines(file.path(path,res.file[i]))
		if(any(grep("No fit possible",log)) || length(res)<1L){
			notrun<-c(notrun,names(lans)[i])
			next
		}
		
		x<-readLines(file.path(path,res.file[i]))
		y<-readLines(file.path(path,det.file[i]))
		ans<-vector(mode="list",length=8)
		ans[[1]] <-input.data
    ans[[2]] <-model_fittingMCDS(x, units=units)
		ans[[3]] <-parameter_estimatesMCDS(x)
		ans[[4]] <- "No covariates in strip transect models"
		ans[[5]] <-chi_square_testMCDS(x)
		ans[[6]] <-density_estimateMCDS(x)
		ans[[7]] <-"No cluster size evaluation are made for stript transects models"
		ans[[8]] <-detection_probabilityMCDS(y, covariates=NULL)
		ans[[9]]<- AIC_MCDS(x)
		ans[[10]] <-path
		names(ans)<-c("input_data","model_fitting","parameter_estimates","covar_key","chi_square_test","density_estimate","cluster_size","detection","AIC","path")
		class(ans)<-"distanceFit"
		#ans
		lans[[i]]<-ans
	}
	if(!is.null(notrun)){warning(paste("No models for the following combinations: ",paste(notrun,collapse=" "),". See log files." ,sep=""),immediate.=TRUE)}
	lans<-lans[!sapply(lans,is.null)]
	class(lans) <- "SpeciesList"
  if(length(lans)==1){lans[[1]]}else{lans}
}
