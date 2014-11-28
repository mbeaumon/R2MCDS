model_fittingMCDS <-
function(x){
	#browser()
	res<-vector("list",length=2)
	names(res)<-c("Global","Stratum")
	w<-grep("Detection Fct/Global/Parameter Estimates",x)
	if(any(w)){
	  g<-grep(" Effort",x)
	  g<-g[g>w][1]
	  l<-strsplit(x[g:(g+4)]," ")
	  ans<-sapply(l,function(i){
		  i<-i[i!=""]
		  i[length(i)]
	  })
	  ans<-as.numeric(ans)
	  names(ans)<-c("effort","samples","width","left","observations")
	  l<-strsplit(x[(g+7):(g+8)]," ")
	  l<-sapply(l,function(i){
	  	i<-paste(i[i!=""],collapse=" ")
	  })
	  ans<-c(ans,l)
	  ans<-data.frame(Descriptives=paste(names(ans),ans,sep=" : "))
	  res$Global<-ans
 }
	stratum_names<-get_stratum_names(x)
	l<-any(unlist(sapply(paste("Detection Fct/",stratum_names,"/Parameter Estimates",sep=""),function(i){grep(i,x,fixed=TRUE)})))
	if(!is.null(stratum_names) && l){		
		mf<-lapply(stratum_names,function(i){
			temp<-paste("Detection Fct/",i,"/Parameter Estimates",sep="")
			w<-grep(temp,x,fixed=TRUE)
			if(any(w)){
				g<-grep(" Effort",x)
				g<-g[g>w][1]
				l<-strsplit(x[g:(g+4)]," ")
				ans<-sapply(l,function(i){
					i<-i[i!=""]
					i[length(i)]
				})
				ans<-as.numeric(ans)
				names(ans)<-c("effort","samples","width","left","observations")
				l<-strsplit(x[(g+7):(g+8)]," ")
				l<-sapply(l,function(i){
					i<-paste(i[i!=""],collapse=" ")
				})
				ans<-c(ans,l)
				ans<-data.frame(Descriptives=paste(names(ans),ans,sep=" : "))				
			  return(ans)
			}else{
			  return(NULL)
			}			
		})
	  names(mf)<-stratum_names
	  res$Stratum<-mf
	}						
	res			
}
