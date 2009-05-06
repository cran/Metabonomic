Met.Aligm <-
function(datos,Peaks){
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
  Met.display<-function(t1)
  {
  t1<-as.matrix(t1)
  tclArray1 = tclArray()
  Require("tcltk")
  tclArray1 = tclArray()
  for (i in (1:dim(t1)[1]))
  {
  for (j in (1:dim(t1)[2]))
  {
  tclArray1[[i,j]] <- t1[i,j]
  tclArray1[[i,0]] <- i
tclArray1[[0,j]] <- as.character(colnames(t1)[j])
  }
  }
  Require("tcltk2")
  tt <- tktoplevel()
  fonttable <- tkfont.create(family="times",size=12)
  tkwm.title(tt,"Metabolites Selection")
  table1 <- tk2table(tt,rows=dim(t1)[1]+1,cols=dim(t1)[2]+1,titlerows=1,titlecols=1,
    xscrollcommand=function(...) tkset(xscr,...),yscrollcommand=function(...) tkset(yscr,...),
selectmode = "extended",colwidth = "13")
  xscr <-tkscrollbar(tt,orient="horizontal", command=function(...)tkxview(table1,...))
  yscr <- tkscrollbar(tt,command=function(...)tkyview(table1,...))

  tkgrid(table1,yscr)
  tkgrid.configure(yscr,sticky="nsw")
  tkgrid(xscr,sticky="new")
  tkconfigure(table1,variable=tclArray1,background="white",foreground="black",font=fonttable,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
  }
  datos$datos<<-picos7
  memory<<-memory+1
  memory.data[[memory]]<<-list(generation=memory,datos=datos$datos,
  info=datos$info)     #For undo function


  Met.display(picos7)
  tkconfigure(console,cursor="arrow")

}

