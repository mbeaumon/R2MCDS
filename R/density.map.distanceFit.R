density.map.distanceFit <-
function(x,shp=NULL,shp.lab,by.stratum=TRUE,subset=NULL,background.shp=NULL,background.shp.lab=NULL,Title="Density"){
	if(class(x)=="distanceFit"){
		if(is.null(x$density_estimate$Stratum)){
			stop("No stratum or different models from which to illustrate densities")
		}
	}
 if(length(subset)>1L){
		subset<-subset[1]
	  warning("only the first element of subset will be used")
	}
	if(by.stratum){
		if(class(x)!="distanceFit"){ #used to be length(x)>1L
			if(is.null(subset)){
				subset<-names(x)[1]
				warning("No subset name given, the first element will be used")
			}
			w<-which(names(x)==subset)
			if(!any(w)){
				 stop(paste("No model for the subset:",subset))
			}	 
			dens<-x[[w]]$density_estimate$Stratum	
		}else{
			dens<-x$density_estimate$Stratum
		}
		names(dens)[which(names(dens)=="Stratum")]<-"Region"
	}else{		
		l<-lapply(x,function(i){
			y<-i$density_estimate
			if(is.null(subset)){
				ans<-y$Global
			}else{
				ans<-y$Stratum
				ans<-ans[ans$Stratum==subset,]			
			}
			ans
		})
		dens<-ldply(l)
		names(dens)[which(names(dens)==".id")]<-"Region"  	
	}	
	##
	if(is.null(dens)){
		dens<-x$density_estimate$Global
		dens<-dens[dens$Parameters=="D",]
		warning("No stratum in the model")
		return(dens)
	}
	dens<-dens[dens$Parameters=="D",]
	if(is.null(shp)){
		return(dens)
	}	
	if(is.na(match(shp.lab,names(shp@data)))){
		stop(paste("Column name ","\"",shp.lab,"\""," not found in the shapefile attributes",sep=""))
	}	
	mat<-match(gsub("'","",shp@data[,shp.lab]),gsub("'","",dens[,"Region"])) #the gsub here is to eliminate the ' in gyre d'anticosti in the shapefile
	if(any(is.na(mat))){
		if(all(is.na(mat))){
			stop(paste("No stratum found in the",shp.lab,"column of the shapefile"))
		}else{
			warning(paste("No information in the model for the following stratum:",paste(shp@data[,shp.lab][is.na(mat)],collapse=", ")))
		}
	}
	shp$Estimates<-dens[,"Estimates"][mat]
	temp<-shp@data
	row.names(temp)<-1:nrow(temp)
	shp@data<-temp
	sx<-fortify(shp,region=shp.lab)
	if(!is.null(background.shp)){  
		#if(is.null(background.shp.lab)){stop("provide a label name for the background shapefile")}
		if(is.null(background.shp.lab)){
		 sy<-fortify(background.shp,region=background.shp.lab)
		}else{
			sy<-fortify(background.shp)
		}	
	}
	sx<-merge(sx,shp@data[,c(shp.lab,"Estimates")],by.x="id",by.y=shp.lab,sort=FALSE)
	sx$Estimates<-as.numeric(sx$Estimates)		
	breaks<-round(seq(min(sx$Estimates,na.rm=T),max(sx$Estimates,na.rm=T),length.out=10),2)
	labels<-breaks	
	#browser()
	p<-ggplot(sx[!is.na(sx$Estimates),],aes(x=long,y=lat,group=group))
	p<-p+theme(plot.background=element_rect(fill = "grey80"),panel.background=element_rect(fill="white"),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.key=element_rect(colour="grey80"))
	if(!is.null(background.shp)){  
		p<-p+geom_polygon(data=sy,aes(x=long,y=lat),color="grey70",fill="lightblue",alpha=0.95)
	}
	p<-p+geom_polygon(aes(fill=Estimates, group=group))
	#p<-p+geom_polygon(data=sx[is.na(sx$Estimates),],aes(group=group),fill="grey")
	p<-p+scale_fill_gradient2(low="white",mid="red",high="darkred",midpoint=quantile(breaks,probs=0.5,names=FALSE),name=paste(subset,"\nnb birds / km2",sep=""),breaks=breaks,labels=labels,limits=range(c(sx$Estimates,breaks),na.rm=TRUE),guide="legend")
	p<-p+ggtitle(Title)
	if(!is.null(background.shp)){  
	  p<-p+geom_path(data=sy,aes(x=long,y=lat),color="grey70")		
	}
	p
}
