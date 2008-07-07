`Met.Order` <-
function(datos)
{
  tkconfigure(console,cursor="watch")
  Met.display.info<-function(t1)
  {
  t1<-as.matrix(t1)
  tclArray1 = tclArray()
  Require("tcltk")
  tclArray1 = tclArray()
  for (i in (1:dim(t1)[1])){
    for (j in (1:dim(t1)[2])){
      tclArray1[[i,j]] <- t1[i,j]
  tclArray1[[i,0]] <- i
tclArray1[[0,j]] <- as.character(colnames(t1)[j])
   }
}
Require("tcltk2")
  tt <- tktoplevel()
  fonttable <- tkfont.create(family="times",size=12)
  tkwm.title(tt,"Metabolites Order")
  table1 <- tk2table(tt,rows=dim(t1)[1]+1,cols=dim(t1)[2]+1,titlerows=1,titlecols=0,
    xscrollcommand=function(...) tkset(xscr,...),yscrollcommand=function(...) tkset(yscr,...))
  xscr <-tkscrollbar(tt,orient="horizontal", command=function(...)tkxview(table1,...))
  yscr <- tkscrollbar(tt,command=function(...)tkyview(table1,...))

  tkgrid(table1,yscr)
  tkgrid.configure(yscr,sticky="nsw")
  tkgrid(xscr,sticky="new")
  tkconfigure(table1,variable=tclArray1,background="white",foreground="black",font=fonttable,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
  return (table1)
  tkwait.window(tt)
  }

  Require("hddplot")
  Try(orderFeatures(datos$datos[,-1],cl=datos$info[,categoria],FUN=aovFbyrow))
  orden=orderFeatures(datos$datos[,-1],cl=datos$info[,categoria],FUN=aovFbyrow)
 
  ordtotal.a<<-cbind(Metabolites=datos$datos[orden,1],Numeration=orden)
  Met.display.info(ordtotal.a)
  tkconfigure(console,cursor="arrow")
}

