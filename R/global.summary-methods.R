#'@export
#'@title Create a PDF file that summarize the output of the Distance software. 
#'
#'
#'@description The file contains a chi-square test table, a predicted density table, a summary of the the detection function, a summary for the model and a histogram of detection. 
#'@param model The output of a distance model fitted with the \code{\link{mcds.wrap}} function (of class \code{distanceFit} or \code{distanceList}).
#'@param species vector containing the name of species in each output.
#'@param file Name for the file.
#'@param directory Where to save the output.
#'@details
#'This function creates a visual summary of the output file saved as a PDF. See the help files for \code{\link{mcds.wrap}} for other examples.
#'@return
#'This function will prints all the summary tables and an histogram with the predicted detection function in on a 11 X 8 inches PDF. If Global summary is applied on a list in will produce a serie of PDF. 
#'@references 
#'S.T. Buckland, D.R. Anderson, K.P. Burnham, J.L Laake, D.L. Borchers and L. Thomas. 2001. \emph{Introduction to Distance Sampling}. Estimating abundance of biological populations. Oxford University Press.
#'@section Author:Christian Roy
#'@examples
#'### Import and filter data
#'data(alcidae)
#'alcids<-filterECSAS(alcidae)
#'
#'### Set arguments and parameters
#'path<-"c:/temp/distance"
#'pathMCDS<-"C:/Program Files (x86)/Distance 6"
#'breaks<-c(0,50,100,200,300)
#'STR_LABEL<-"STR_LABEL"
#'STR_AREA<-"STR_AREA"
#'SMP_LABEL<-"WatchID"
#'SMP_EFFORT<-"WatchLenKm"
#'DISTANCE<-"Distance"
#'SIZE<-"Count"
#'
#'### Run analysis with the MCDS engine. Here, the WatchID is used as the sample.
#'x<-mcds.wrap(alcids,path=path,pathMCDS=pathMCDS,breaks=breaks,STR_LABEL=STR_LABEL,STR_AREA=STR_AREA,SMP_LABEL=SMP_LABEL,SMP_EFFORT=SMP_EFFORT,DISTANCE=DISTANCE,SIZE=SIZE,verbose=FALSE)
#'global.summary(model=x, species="alcids", file="alcidae_global", directory="C:/temp/distance") 
#'END


#'@export
#'@rdname global.summary
#'
global.summary <-
	function (model, ...) {
		UseMethod("global.summary", model)
	}



#'@export
#'@rdname global.summary

global.summary.distanceFit <- function(model, species, file="tempo", directory="C:/temp/distance"){
	
	dir.create(directory, recursive = TRUE, showWarnings = FALSE)
	sink(paste(directory,paste(file,".Rnw",sep=""),sep="/"))
	cat("
					\\documentclass[twocolumn]{article}
					\\usepackage[english]{babel}
					\\usepackage[top=1in, bottom=1in, left=0.5in, right=0.5in,landscape]{geometry}
					\\usepackage[font=small,skip=4pt]{caption}
					\\setlength{\\floatsep}{2em}
					\\begin{document}
					\\setkeys{Gin}{height=0.45\\textwidth}
					\\setkeys{Gin}{width=0.45\\textwidth}
					\\title{GeoAviR output for \\Sexpr{species}}
					\\author{Detection function:\\Sexpr{model$model_fitting$Global$Type}}
					\\date{}
					\\maketitle
					<<tables_code, echo=FALSE,fig=TRUE, results=tex>>=
					print(xtable(model$parameter_estimates$Global, digits =3, caption ='Parameters estimates'), size='\\\\small', 
					caption.placement = getOption('xtable.caption.placement','top'))
					print(xtable(model$chi_square_test$Global, digits =3, caption ='Chi-sqaured test on model fit'), size='\\\\small',
					caption.placement = getOption('xtable.caption.placement','top'))
					print(xtable(model$density_estimate$Global, digits =3, caption='Density Estimates'), size='\\\\small',
					caption.placement = getOption('xtable.caption.placement','top'))
					print(xtable(model$model_fitting$Global$Parameters, digits =3, caption='Model description'), size='\\\\small',
					caption.placement = getOption('xtable.caption.placement','top'))
					##Make the histogram
					p <- observation_hist(model[['input_data']][['observations']], count='SIZE', dist.class='DISTANCE',
					keep.class=as.character(unique(sort(as.numeric(model[['input_data']][['observations']]$DISTANCE)))),
					breaks=model[['input_data']][['breaks']], color='powderblue',
					rescale=model$detection[['Global']][,'predicted'][1]) +
					labs(title = 'Detection probability vs. distance', x = 'Distance', y = 'Detection probability')
					
					pred.df <- data.frame(x=model$detection[['Global']][,'distance'],y=model$detection[['Global']][,'predicted'])
					
					## add predicted detection
					p <- p + geom_line(data=pred.df, aes(x=x,y=y, xmin=0, xmax=max(pred.df), ymin=0, ymax=1), linetype=1, size=1.25)
					
					##print figure
					print(p)
					@
					\\end{document}
					",fill=TRUE)
	sink()
	attach(environment()) 
	wd <- getwd()
	setwd(directory)
	Sweave(paste(directory,paste(file,".Rnw",sep=""),sep="/"))
	tools::texi2pdf(paste(directory,paste(file,".tex",sep=""),sep="/"))
	
	#Clean wd
	to.remove <- dir()[grep(file, dir())][-match(paste(file,".pdf",sep=""),dir()[grep(file, dir())])]
	file.remove(to.remove)
	setwd(wd)
	detach(environment()) 
}  



#'@export
#'@rdname global.summary

global.summary.distanceList <- function(model, species, file="tempo", directory="C:/temp/distance"){
	
	
	nloop <- length(model)
	for(i in 1:nloop){
		nfile <- paste(file,names(model)[i], sep="_")
		nmodel <- model[[i]]  
		dir.create(directory, recursive = TRUE, showWarnings = FALSE)
		sink(paste(directory,paste(nfile,".Rnw",sep=""),sep="/"))
		cat("
						\\documentclass[twocolumn]{article}
						\\usepackage[english]{babel}
						\\usepackage[top=1in, bottom=1in, left=0.5in, right=0.5in,landscape]{geometry}
						\\usepackage[font=small,skip=4pt]{caption}
						\\setlength{\\floatsep}{2em}
						\\begin{document}
						\\setkeys{Gin}{height=0.45\\textwidth}
						\\setkeys{Gin}{width=0.45\\textwidth}
						\\title{GeoAviR output for \\Sexpr{species[i]}}
						\\author{Detection function:\\Sexpr{nmodel$model_fitting$Global$Type}}
						\\date{}
						\\maketitle
						<<tables_code, echo=FALSE,fig=TRUE, results=tex>>=
						print(xtable(nmodel$parameter_estimates$Global, digits =3, caption ='Parameters estimates'), size='\\\\small',
						caption.placement = getOption('xtable.caption.placement','top'))
						print(xtable(nmodel$chi_square_test$Global, digits =3, caption ='Chi-sqaured test on model fit'), size='\\\\small',
						caption.placement = getOption('xtable.caption.placement','top'))
						print(xtable(nmodel$density_estimate$Global, digits =3, caption='Density Estimates'), size='\\\\small',
						caption.placement = getOption('xtable.caption.placement','top'))
						print(xtable(nmodel$model_fitting$Global$Parameters, digits =3, caption='Model description'), size='\\\\small',
						caption.placement = getOption('xtable.caption.placement','top'))
						
						##Make the histogram
						p <- observation_hist(nmodel[['input_data']][['observations']], count='SIZE', dist.class='DISTANCE',
						keep.class=as.character(unique(sort(as.numeric(nmodel[['input_data']][['observations']]$DISTANCE)))),
						breaks=nmodel[['input_data']][['breaks']], color='powderblue',
						rescale=nmodel$detection[['Global']][,'predicted'][1]) +
						labs(title = 'Detection probability vs. distance', x = 'Distance', y = 'Detection probability')
						
						pred.df <- data.frame(x=nmodel$detection[['Global']][,'distance'],y=nmodel$detection[['Global']][,'predicted'])
						
						## add predicted detection
						p <- p + geom_line(data=pred.df, aes(x=x,y=y, xmin=0, xmax=max(pred.df), ymin=0, ymax=1), linetype=1, size=1.25)
						
						##print figure
						print(p)
						@
						\\end{document}
						",fill=TRUE)
		sink()
		wd <- getwd()
		setwd(directory)
		attach(environment()) 
		Sweave(paste(directory,paste(nfile,".Rnw",sep=""),sep="/"))
		tools::texi2pdf(paste(directory,paste(nfile,".tex",sep=""),sep="/"))
		#Clean wd
		to.remove <- dir()[grep(nfile, dir())][-match(paste(nfile,".pdf",sep=""),dir()[grep(nfile, dir())])]
		file.remove(to.remove)
		setwd(wd)
		detach(environment()) 
	}
}  




