resume.plot.unmarkedFitDS <-
function(model,sampling.units="Watches",obs.units="Birds",title=NULL,
             subtitle=NULL, file="tempo", directory="C:/temp/distance",
             PDF=T){
		
    #make sure directory exist
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    
    if(PDF==T){
          #open device
          pdf(file=paste(directory,"/",file,".pdf", sep=""),  width=11, height=8)
          #make graph
          par(pin=c(11,8.5),mar=c(1,1,1,1)+0.1)
      		m<-matrix(c(1,2,3,4,1,5,6,6),4,2)
      		layout(m,widths = c(1.25,1.25,1.25,1.25,0.75,0.75,0.75,0.75))
      		plot(0,0,type="n",xaxt="n",yaxt="n",bty="n")
      		text(0,0.5,title,font=2,cex=5,adj=c(0.5,0.5))
      		text(0,-0.5,subtitle,font=2,cex=3,adj=c(0.5,0.5))
      		par(mar=c(1,4,4,2)+0.1)
      		textplot(.abundancetable(model), halign = "center", valign = "center", cmar=1)
      		title("Density estimate", cex.main = 2)
      		textplot(.detectiontable(model), halign = "center", valign = "center", cmar=1)
      		title("Parameters Estimates", cex.main = 2)
      		legend("bottom", legend = "*SE backcalculated from CI for more precision use parboot()", bty = "n")
      		textplot(.chisquaretable(model), halign = "center", valign = "center", cmar=1)
      		title("Chi-square test on model fit", cex.main = 2)
      		textplot(.modeltable(model, sampling.units = sampling.units, obs.units = obs.units), show.rownames = FALSE, cmar=1)
      		title("Model Fitting", cex.main = 2)
      		par(mar = c(5, 5, 4, 2))
      		hist(model, col = "grey70", lty = 2, lwd = 3, cex.lab = 1.75, cex.axis=1.5)
      		#close device
      		dev.off()
		}else{
		    png(file=paste(directory,"/",file,".png", sep=""), res=600, width=11, height=8, units = "in")
        #make graph
        par(pin=c(11,8.5),mar=c(1,1,1,1)+0.1)
    		m<-matrix(c(1,2,3,4,1,5,6,6),4,2)
    		layout(m,widths = c(1.25,1.25,1.25,1.25,0.75,0.75,0.75,0.75))
    		plot(0,0,type="n",xaxt="n",yaxt="n",bty="n")
    		text(0,0.5,title,font=2,cex=5,adj=c(0.5,0.5))
    		text(0,-0.5,subtitle,font=2,cex=3,adj=c(0.5,0.5))
    		par(mar=c(1,4,4,2)+0.1)
    		textplot(.abundancetable(model), halign = "center", valign = "center", cmar=1)
    		title("Density estimate", cex.main = 2)
    		textplot(.detectiontable(model), halign = "center", valign = "center", cmar=1)
    		title("Parameters Estimates", cex.main = 2)
    		legend("bottom", legend = "*SE backcalculated from CI for more precision use parboot()", bty = "n")
    		textplot(.chisquaretable(model), halign = "center", valign = "center", cmar=1)
    		title("Chi-square test on model fit", cex.main = 2)
    		textplot(.modeltable(model, sampling.units = sampling.units, obs.units = obs.units), show.rownames = FALSE, cmar=1)
    		title("Model Fitting", cex.main = 2)
    		par(mar = c(5, 5, 4, 2))
    		hist(model, col = "grey70", lty = 2, lwd = 3, cex.lab = 1.75, cex.axis=1.5)
    		#close device
    		dev.off()
		}    
		#Return parameters to normal
 	  par(mar = c(5, 5, 4, 2))
    layout(matrix(c(1, 1)))
}
