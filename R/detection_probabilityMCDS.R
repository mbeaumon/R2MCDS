detection_probabilityMCDS <-
function(x){
	res<-vector("list",length=2)
	names(res)<-c("Global","Stratum")
	ans<-sandwich.table(val="Detection Fct",vec=x,offset=c(5,0))
	ans<-lapply(ans,function(i){
		l<-strsplit(i," ")
		l<-lapply(l,function(i){i<-i[i!=""]})
		#w<-which(sapply(l,length)==1)[1]+1
		#l<-l[w:length(l)]
		l<-l[!sapply(sapply(l,function(j){grep("\\*",j)}),any)]
		l<-ldply(l,as.numeric)
		names(l)<-c("distance","observed","predicted")  
		l
	})
	w<-grep("Detection Fct",x)
	g<-grep("Pooled data",x[w+1])
	if(any(g)){
		names(ans)[w[g]]<-"Global"
	}
	gg<-setdiff(seq_along(w),g)
	if(any(gg)){
		names(ans)[gg]<-sapply(strsplit(x[w[gg]],"/"),function(i){i[2]})
	}
	ans
}
