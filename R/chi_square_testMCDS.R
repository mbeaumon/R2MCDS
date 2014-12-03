chi_square_testMCDS <-
function(x){
	res<-vector("list",length=2)
	names(res)<-c("Global","Stratum")
	w<-grep("Detection Fct/Global/Chi-sq GOF Test",x)
	if(any(w)){
		tab<-dtable(x,w,char="---------")
		tab<-data.frame(Stratum="Global",tab,stringsAsFactors=F)
		names(tab)<-c("Stratum","Distance bin","Breaks","Break2","Observed","Predicted","Chi.values")
		tab[,3:7]<- sapply(1:5, function(i){as.numeric(tab[,i+2])})
		tab[,"Breaks"] <- paste("[",tab[,"Breaks"]," - ",tab[,"Break2"],"[", sep="")
		tab <- tab[,-4]
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
		names(ans)<-c("Stratum","Distance bin","Breaks","Break2","Observed","Predicted","Chi.values")
		ans[,3:7]<- sapply(1:5, function(i){as.numeric(ans[,i+2])})
		ans[,"Breaks"] <- paste("[",ans[,"Breaks"]," - ",ans[,"Break2"],"[", sep="")
		ans <- ans[,-4]
    res$Stratum<-ans			
	}
	res
}
