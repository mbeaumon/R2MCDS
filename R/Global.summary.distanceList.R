Global.summary.distanceList <- function(model, file="tempo", directory="C:/temp/distance"){
  
  
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
\\title{GeoAvir output}
\\date{}
\\maketitle
<<tables_code, echo=FALSE,fig=TRUE, results=tex>>=
print(xtable(nmodel$parameter_estimates$Global, digits =3, caption ='Parameters estimates'), size='\\\\small')
print(xtable(nmodel$chi_square_test$Global, digits =3, caption ='Chi-sqaured test on model fit'), size='\\\\small')
print(xtable(nmodel$density_estimate$Global, digits =3, caption='Density Estimates'), size='\\\\small')
print(xtable(nmodel$model_fitting$Global$Parameters, digits =3, caption='Model description'), size='\\\\small')
##Make the histogram
p <- hist.wrap(nmodel[['input_data']][['observations']], Count='SIZE', Dist.class='DISTANCE',
Keep.class=as.character(unique(sort(as.numeric(nmodel[['input_data']][['observations']]$DISTANCE)))),
Breaks=nmodel[['input_data']][['breaks']], color='powderblue',
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

