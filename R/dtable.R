dtable <-
function(x,w,char,nbcol=6){ # if length(char)==1 c'est le character encerclant, si length(char)==2, c'est la position du début et fin du tableau
	if(length(char)==1L){
	  tab<-grep(char,x)
	  tab<-tab[tab>w][1:2]+c(1,-1)
	}else{
	  tab<-char
		}
	l<-strsplit(x[tab[1]:tab[2]]," ")
	l<-lapply(l,function(i){
		i<-i[i!=""]	    
		i<-i[1:nbcol]
	})
	ans<-ldply(l)
	ans
}
