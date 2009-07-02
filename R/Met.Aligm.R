Met.Aligm <-
function(datos,Peaks)
{
  tkconfigure(console,cursor="watch")
  picos<-datos$datos
  picos3<-as.matrix(picos)
  rownames(picos3)<-picos[,1]
  picos3<-picos3[,-1]
  Require("caMassClass")
  Try(msc.peaks.align(Peaks, SampFrac=0.1, BinSize=c(0, 0.01)))
  biomarcadores<-msc.peaks.align(Peaks, SampFrac=0.1, BinSize=c(0, 0.01))
  picos6<-msc.biomarkers.fill( picos3, biomarcadores$Bmrks, biomarcadores$BinBounds, FillType=2)
  V1<-(biomarcadores$BinBounds[,1]+biomarcadores$BinBounds[,2])/2
  picos7<-cbind(V1,picos6)
  dim(picos7)
  picos7<-as.data.frame(picos7)
  
  datos$datos<<-picos7
  memory<<-memory+1
  memory.data[[memory]]<<-list(generation=memory,datos=datos$datos,
  info=datos$info)     #For undo function


  showData2(picos7,title="Peaks")
  tkconfigure(console,cursor="arrow")

}

