TABLE <-
function(a,b,title=""){
  e<-title
  d<-table(a,b)
  t1=as.matrix(d)
   t1<-cbind(rownames(t1),t1)
  t1<-rbind(colnames(t1),t1)
  t1[1,1]="X"
  t1<-as.matrix(t1)
  t2<-t1[-1,]
  t2<-t2[,-1]
  showData2(t2,title=title)
}

