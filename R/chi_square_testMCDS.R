chi_square_testMCDS <-
function(x){
	res<-vector("list",length=2)
	names(res)<-c("Global","Stratum")
	w<-grep("Detection Fct/Global/Chi-sq GOF Test",x)
	if(any(w)){
		tab<-dtable(x,w,char="---------")
		tab<-data.frame(Stratum="Global",tab,stringsAsFactors=F)
		names(tab)<-c("Stratum","Cell","CutsLo","CutsUp","Observed","Predicted","Chi.values")
	  res$Global<-tab
	}
	stratum_names<-get_stratum_names(x)
	l<-any(unlist(sapply(paste("Detection Fct/",stratum_names,"/Chi-sq GOF Test",sep=""),function(i){grep(i,x,fixed=TRUE)})))
	if(!is.null(stratum_names) && l){		
		ans<-lapply(stratum_names,function(i){
			temp<-paste("Detection Fct/",i,"/Chi-sq GOF Test",sep="")
			y<-sapply(temp,grep,x=x,fixed=TRUE)
			if(!any(y)){return(NULL)}
			tab<-dtable(x,y,char="---------")
			tab<-data.frame(Stratum=i,tab,stringsAsFactors=F)
			tab		
		})						
		ans<-do.call("rbind",ans)		
		names(ans)<-c("Stratum","Cell","CutsLo","CutsUp","Observed","Predicted","Chi.values")
		res$Stratum<-ans			
	}
	res
}
