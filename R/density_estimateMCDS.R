density_estimateMCDS <-
function(x){
	res<-vector("list",length=2)
	names(res)<-c("Global","Stratum")
	w<-grep("Density Estimates/Global",x)
	if(any(w)){	  
	  ans<-dtable(x,w=w,char="---------")	  
	  #ans<-ans[c(3,1,2),]
	  ans<-data.frame(Stratum="Global",ans,stringsAsFactors=F)
	  names(ans)<-c("Stratum","Parameters","Estimates","SE*","% of var.","95% Lower","95% Upper")
	  res$Global<-ans	
	}
	stratum_names<-get_stratum_names(x)
	g<-grep("Density Estimates/Global",x)
  #browser()
  l<-any(unlist(sapply(paste("Density Estimates/",stratum_names,sep=""),function(i){grep(i,x,fixed=TRUE)})))
	
	if(!is.null(stratum_names) && l){		
		ans<-lapply(stratum_names,function(i){
			temp<-paste0("\t","Density Estimates/",i,"\t")   # I added the \t because could found to place ex: parc347 et parc3479
      #browser()
      y<-sapply(temp,grep,x=x,fixed=TRUE)
      #if(!is.null(dim(y))){
      # which(sapply(strsplit(x[y[,1]],"/"),function(j){j[2]})
      if(!any(y)){return(NULL)}
			tab<-dtable(x,y,char="---------")
			tab<-data.frame(Stratum=i,tab,stringsAsFactors=F)
      tab		
		})						
	  ans<-do.call("rbind",ans)		
		names(ans)<-c("Stratum","Parameters","Estimates","SE*","% of var.","95% Lower","95% Upper")
		res$Stratum<-ans			
	}
	g<-grep("Estimation Summary - Density&Abundance",x)
  if(length(g)==2L){
    if(any(grep("Pooled Estimates:",x[g[2]+2]))){
    	gg<-grep("Estimation Summary",x)
    	gg<-gg[gg>g[2]]
    	if(any(gg)){gg<-min(gg)-1}else{gg<-length(x)}
    	ans<-dtable(x=x,char=c(g[2]+5,gg))	
    	ans<-data.frame(Stratum="Global",ans,stringsAsFactors=F)
    	names(ans)<-c("Stratum","Parameters","Estimates","SE*","% of var.","95% Lower","95% Upper")
    	res$Global<-ans
    }
  }  
	res
}
