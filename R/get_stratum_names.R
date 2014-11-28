get_stratum_names <-
function(x){
	y<-strsplit(gsub("Stratum: ","",unique(x[grep("Stratum:",x)])),"")
	if(length(y)==0){return(NULL)}
	ans<-sapply(y,function(i){
		w<-range(which(i!=" "))
		paste(i[w[1]:w[2]],collapse="")
	})
	ans
}
