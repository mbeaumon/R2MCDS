resume.plot.distanceList <-
function(model,subset=NULL,stratum=FALSE,title=NULL,subtitle=NULL,
        file="tempo", directory="C:/temp/distance", PDF=T)
{
	if(!is.null(subset)){
		if(length(subset)>1){stop("Only one subset is allowed at a time")}
		if(is.na(match(subset,names(model)))){stop("Subset not in found in the set of models")}
		model<-model[[subset]]
	}else{
	 model<-model[[1]]
	 warning("No subset given, first model chosen")
	}
	
  #make sure directory exist
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  
  if(PDF==T){
    #open device
    pdf(file=paste(directory,"/",file,".pdf", sep=""),  width=11, height=8)
    
    #title
  	par(pin=c(11,8.5),mar=c(1,1,1,1)+0.1)
  	mg<-matrix(c(1,2,3,4,4,1,5,6,6,7),5,2)
  	l<-layout(mg,widths=c(1,0.75),heights=c(0.75,1,1,1,0.75))
  	plot(0,0,type="n",xaxt="n",yaxt="n",bty="n")
  	text(0,0.5,title,font=2,cex=5,adj=c(0.5,0.5),col="cornflowerblue")
  	text(0,-0.6,subtitle,font=2,cex=3,adj=c(0.5,0.5),col="cornflowerblue")
  	par(mar=c(1,4,4,2)+0.1)
  	#parameter estimate
  	y<-model$parameter_estimates
  	if(!stratum || is.null(y[["Stratum"]])){
  		yy<-y[["Global"]]
  		if(is.null(yy)){yy<-y[["Stratum"]]}	
  	}else{
  		yy<-y[["Stratum"]]
  	}
  	textplot(yy, halign = "center", valign = "center", show.rownames = FALSE, cmar=1)
  	title("Parameters Estimates", cex.main = 2)
  	legend("bottom", legend = "*SE backcalculated from CI for more precision use parboot()",bty = "n")
  	#chi-square
  	y<-model$chi_square_test
  	if(!stratum || is.null(y[["Stratum"]])){
  		yy<-y[["Global"]]
  		if(is.null(yy)){yy<-y[["Stratum"]]}	
  	}else{
  		yy<-y[["Stratum"]]
  	}
  	textplot(yy,halign = "center", valign = "center", show.rownames = FALSE, cmar=1)
  	title("Chi-square test on model fit", cex.main = 2)
  	#density estimate
  	y<-model$density_estimate
  	if(!stratum || is.null(y[["Stratum"]])){
  		yy<-y[["Global"]]
  		if(is.null(yy)){yy<-y[["Stratum"]]}	
  	}else{
  		yy<-y[["Stratum"]]
  	}
  	textplot(yy, halign = "center", valign = "center", show.rownames = FALSE, cmar=1)
  	title("Density Estimates", cex.main = 2)
  	#model fitting
  	y<-model$model_fitting
  	if(!stratum || is.null(y[["Stratum"]])){
  		yy<-y$Global$Parameters
  		if(is.null(yy)){yy<-y[["Stratum"]]}	
  	}else{
  		yy<-y$Global$Parameters
  	}
  	textplot(yy, show.rownames = FALSE, cmar=1)
  	title("Model Fitting", cex.main = 2)
  	#graph
  	par(mar = c(5, 5, 4, 2))
  	ans<-model$detection[["Global"]]
  	cutdist<-which(ans[,"observed"]==0 & duplicated(ans[,"distance"]))-1
  	obs<-ans[cutdist,"observed"]
  	plot(0,0,xlim=range(c(0,model$detection[["Global"]]$distance)),ylim=c(0,1),yaxt="n",xlab="Distance (m)",ylab="Detection probability",type="n",main="Detection probability vs. distance",cex.main=2)
  	axis(2,at=seq(0,1,by=0.1),labels=seq(0,1,by=0.1),las=2)
  	r<-range(ans[,"distance"])
  	r<-(r[2]-r[1])*0.005
  	deb<-min(model$detection[["Global"]]$distance)
  	for(i in seq_along(obs)){
  		rect(xleft=deb+r, ybottom=0, xright=ans[cutdist[i],"distance"]-r, ytop=obs[i],col="lightblue",border="lightblue")
  		deb<-ans[cutdist[i],"distance"]
  	}
  	lines(ans[,"distance"],ans[,"predicted"],lwd=2)
  	legend(x="topright",legend=c("Observed","Predicted"),lty=c(0,1),lwd=c(0,2),pch=c(15,NA),col=c("lightblue","black"),inset=c(0.1,0.1),pt.cex=2)
  	#info
  	par(mar=c(1,0,0,0)+0.1)
  	plot(0,0,type="n",ylim=c(-3,8),xlim=c(0,50),xaxt="n",yaxt="n",bty="n")
  	text(0,7,"p: probability of observing an object in the defined area",adj=c(0,0.5))
  	text(0,6,"f(0): probability density function at distance 0",adj=c(0,0.5))
  	text(0,5,"ESW: effective strip width",adj=c(0,0.5))
  	text(0,4,"D: bird density (nb birds / km2)",adj=c(0,0.5))
  	text(0,3,"DS: group density (nb groups / km2)",adj=c(0,0.5))
  	text(0,2,"E(S): expected group size",adj=c(0,0.5))
  	text(0,1,"N: nb of birds in the area",adj=c(0,0.5))
  	text(0,-1,"Detailed results in:",adj=c(0,0.5))
  	text(0,-2,model[["path"]],adj=c(0,0.5),col="red")
  	#close device
  	dev.off()
  }else{
    png(file=paste(directory,"/",file,".png", sep=""), width=11, res=600, height=8, units = "in")
    #title
  	par(pin=c(11,8.5),mar=c(1,1,1,1)+0.1)
  	mg<-matrix(c(1,2,3,4,4,1,5,6,6,7),5,2)
  	l<-layout(mg,widths=c(1,0.75),heights=c(0.75,1,1,1,0.75))
  	plot(0,0,type="n",xaxt="n",yaxt="n",bty="n")
  	text(0,0.5,title,font=2,cex=5,adj=c(0.5,0.5),col="cornflowerblue")
  	text(0,-0.6,subtitle,font=2,cex=3,adj=c(0.5,0.5),col="cornflowerblue")
  	par(mar=c(1,4,4,2)+0.1)
  	#parameter estimate
  	y<-model$parameter_estimates
  	if(!stratum || is.null(y[["Stratum"]])){
  		yy<-y[["Global"]]
  		if(is.null(yy)){yy<-y[["Stratum"]]}	
  	}else{
  		yy<-y[["Stratum"]]
  	}
  	textplot(yy, halign = "center", valign = "center", show.rownames = FALSE, cmar=1)
  	title("Parameters Estimates", cex.main = 2)
  	legend("bottom", legend = "*SE backcalculated from CI for more precision use parboot()",bty = "n")
  	#chi-square
  	y<-model$chi_square_test
  	if(!stratum || is.null(y[["Stratum"]])){
  		yy<-y[["Global"]]
  		if(is.null(yy)){yy<-y[["Stratum"]]}	
  	}else{
  		yy<-y[["Stratum"]]
  	}
  	textplot(yy,halign = "center", valign = "center", show.rownames = FALSE, cmar=1)
  	title("Chi-square test on model fit", cex.main = 2)
  	#density estimate
  	y<-model$density_estimate
  	if(!stratum || is.null(y[["Stratum"]])){
  		yy<-y[["Global"]]
  		if(is.null(yy)){yy<-y[["Stratum"]]}	
  	}else{
  		yy<-y[["Stratum"]]
  	}
  	textplot(yy, halign = "center", valign = "center", show.rownames = FALSE, cmar=1)
  	title("Density Estimates", cex.main = 2)
  	#model fitting
  	y<-model$model_fitting
  	if(!stratum || is.null(y[["Stratum"]])){
  		yy<-y[["Global"]]
  		if(is.null(yy)){yy<-y[["Stratum"]]}	
  	}else{
  		yy<-y[["Stratum"]]
  	}
  	textplot(yy, show.rownames = FALSE, cmar=1)
  	title("Model Fitting", cex.main = 2)
  	#graph
  	par(mar = c(5, 5, 4, 2))
  	ans<-model$detection[["Global"]]
  	cutdist<-which(ans[,"observed"]==0 & duplicated(ans[,"distance"]))-1
  	obs<-ans[cutdist,"observed"]
  	plot(0,0,xlim=range(c(0,model$detection[["Global"]]$distance)),ylim=c(0,1),yaxt="n",xlab="Distance (m)",ylab="Detection probability",type="n",main="Detection probability vs. distance",cex.main=2)
  	axis(2,at=seq(0,1,by=0.1),labels=seq(0,1,by=0.1),las=2)
  	r<-range(ans[,"distance"])
  	r<-(r[2]-r[1])*0.005
  	deb<-min(model$detection[["Global"]]$distance)
  	for(i in seq_along(obs)){
  		rect(xleft=deb+r, ybottom=0, xright=ans[cutdist[i],"distance"]-r, ytop=obs[i],col="lightblue",border="lightblue")
  		deb<-ans[cutdist[i],"distance"]
  	}
  	lines(ans[,"distance"],ans[,"predicted"],lwd=2)
  	legend(x="topright",legend=c("Observed","Predicted"),lty=c(0,1),lwd=c(0,2),pch=c(15,NA),col=c("lightblue","black"),inset=c(0.1,0.1),pt.cex=2)
  	#info
  	par(mar=c(1,0,0,0)+0.1)
  	plot(0,0,type="n",ylim=c(-3,8),xlim=c(0,50),xaxt="n",yaxt="n",bty="n")
  	text(0,7,"p: probability of observing an object in the defined area",adj=c(0,0.5))
  	text(0,6,"f(0): probability density function at distance 0",adj=c(0,0.5))
  	text(0,5,"ESW: effective strip width",adj=c(0,0.5))
  	text(0,4,"D: bird density (nb birds / km2)",adj=c(0,0.5))
  	text(0,3,"DS: group density (nb groups / km2)",adj=c(0,0.5))
  	text(0,2,"E(S): expected group size",adj=c(0,0.5))
  	text(0,1,"N: nb of birds in the area",adj=c(0,0.5))
  	text(0,-1,"Detailed results in:",adj=c(0,0.5))
  	text(0,-2,model[["path"]],adj=c(0,0.5),col="red")
  	#close device
  	dev.off()
  	}
  #reset
	par(mar = c(5, 5, 4, 2))
	layout(matrix(c(1, 1)))	
}
