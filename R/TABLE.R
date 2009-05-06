TABLE <-
function(a,b,title=""){
  e<-title
  d<-table(a,b)
  t1=as.matrix(d)
   t1<-cbind(rownames(t1),t1)
  t1<-rbind(colnames(t1),t1)
  t1[1,1]="X"
  t1<-as.matrix(t1)
  tclArray1 = tclArray()
  #construction(t1)
  #Met.displayInTable2(t1,t1,title=e)

  Require("tcltk")
  tclArray1 = tclArray()
  for (i in (1:dim(t1)[1]))
    for (j in (1:dim(t1)[2]))
      tclArray1[[i-1,j-1]] <- t1[i,j]
  
  Require("tcltk2")
  tt <- tktoplevel()
  fonttable <- tkfont.create(family="times",size=12)
  tkwm.title(tt,e)
  table1 <- tk2table(tt,rows=dim(t1)[1]+1,cols=dim(t1)[2]+1,titlerows=0,titlecols=0,
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

