sandwich.table <-
function(val,vec,to.end=TRUE,offset=c(0,0),val.pos=FALSE){
	if(val.pos){
		pos<-val 
	}else{	  
		if(length(val)==1){
			pos<-grep(val,vec)
		}else{
			#browser()
			pos<-sort(unlist(sapply(val,function(i){grep(i,vec)})))
		}
	}
	if(!any(pos)){return(vec)}
	posoff<-c(sapply(pos,function(i){seq(i-offset[2],i+offset[1],by=1)}),recursive=TRUE)
	w<-setdiff(pos[1]:ifelse(to.end,length(vec),pos[length(pos)]),posoff)
	s<-split(w,findInterval(w,pos))
	lapply(s,function(i){vec[i]})
}
