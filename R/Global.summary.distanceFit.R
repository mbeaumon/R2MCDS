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


