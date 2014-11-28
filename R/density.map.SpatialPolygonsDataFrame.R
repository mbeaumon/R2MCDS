density.map.SpatialPolygonsDataFrame <-
function(x,observations,only.visits=TRUE,background.shp=NULL,Title=NULL){	
	if(!is.null(background.shp)){
		if(class(background.shp)[1]!="SpatialPolygonsDataFrame")
			stop("background.shp must be a shapefile")
		if(proj4string(x)==proj4string(background.shp))
			stop("background.shp and Observation.grid must be in the same projection")
	}
	if(class(x@data[,observations])!= "numeric"){
		x@data[,observations] <- as.numeric(x@data[,observations])
	}	
	max.seen<-max(x@data[,observations])
	max.seen<-ifelse(max.seen ==0, 1, max.seen)
	if(max.seen <= 20){
		up.scale<-ceiling(max.seen)*3 + 1
		my.colors<-c("white", rev(heat_hcl(up.scale, c = c(80, 30), l = c(30, 90), power = c(1/5, 2)))[seq(1,up.scale, by=3)])
		my.colors2<-c("#0000ff01", rev(heat_hcl(up.scale, c = c(80, 30), l = c(30, 90), power = c(1/5, 2)))[seq(1,up.scale, by=3)])
	}else{
		up.scale<- 20*3
		my.colors<-c("white", rev(heat_hcl(up.scale, c = c(80, 30), l = c(30, 90), power = c(1/5, 2)))[seq(1,up.scale, by=3)])
		my.colors2<-c("#0000ff01", rev(heat_hcl(up.scale, c = c(80, 30), l = c(30, 90), power = c(1/5, 2)))[seq(1,up.scale, by=3)])
	}
	par(pin=c(8.5, 11), mar=c(1, 4, 4, 2) +0.1)
	layout(mat=matrix(1 :2, nrow=2, ncol=1), heights=c(6, 1))
	par(pin=c(8.5, 11), mar=c(1, 4, 4, 2) +0.1)
	if(only.visits){
		dataset<-x[x$VISITED>0,]
	}else{
		dataset<-x
	}	
	plot(dataset, col=my.colors[dataset@data$VISITED+1], axes=TRUE)
	if(!is.null(background.shp)){
		plot(background.shp, col="cornflowerblue", add=TRUE)
	}
	if(is.null(Title)==F){
		title(Title)
	}
	plot.col <- ifelse(ceiling(dataset@data[,observations])>=20, 20, ceiling(dataset@data[,observations]))
	plot(dataset, col=my.colors2[plot.col+1], add=T)
	par(mar=c(2,1,1,1))
	color.scale(my.colors)
	index<-seq(1,length(my.colors),1)
	place<-(c(0,(index/length(my.colors))[1:(length(my.colors) - 1)]) + index/length(my.colors))/2
	mtext(text="Not\nVisited", side=1, at=place[1], padj=0.5, cex=0.75)
	my.text<-c(seq(0, (length(my.colors2)-3)), paste(length(my.colors2)-2,"+", sep=""))
	mtext(text=my.text, side=1, at=place[2:length(place)], cex=0.75)
	#END of the function
}
