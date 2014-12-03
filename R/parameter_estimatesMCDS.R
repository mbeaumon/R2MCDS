parameter_estimatesMCDS <-
function(x){
	x<-gsub("A\\( ","A\\(",x)
	res<-vector("list",length=2)
	names(res)<-c("Global","Stratum")
	w<-grep("Detection Fct/Global/Parameter Estimates",x)
	if(any(w)){	  
		ans<-dtable(x,w=w,char="---------")	  
		ans<-data.frame(Stratum="Global",ans,stringsAsFactors=F)
		names(ans)<-c("Stratum","Parameters","Estimates","SE","% of var.","95% Lower","95% Upper")
		ans[,3:7]<- sapply(1:5, function(i){as.numeric(ans[,i+2])})
    res$Global<-ans	
	}
	stratum_names<-get_stratum_names(x)
	l<-any(unlist(sapply(paste("Detection Fct/",stratum_names,"/Parameter Estimates",sep=""),function(i){grep(i,x,fixed=TRUE)})))
	if(!is.null(stratum_names) && l){				
		ans<-lapply(stratum_names,function(i){
			temp<-paste("Detection Fct/",i,"/Parameter Estimates",sep="")
			y<-sapply(temp,grep,x=x,fixed=TRUE)
			if(!any(y)){return(NULL)}
			tab<-dtable(x,y,char="---------")
			tab<-data.frame(Stratum=i,tab,stringsAsFactors=F)
			tab		
		})						
		ans<-do.call("rbind",ans)	
		names(ans)<-c("Stratum","Parameters","Estimates","SE","% of var.","95% Lower","95% Upper")
		ans[,3:7]<- sapply(1:5, function(i){as.numeric(ans[,i+2])})
		res$Stratum<-ans			
	}	  
	res
}
