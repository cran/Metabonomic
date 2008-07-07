`Met.Baseline` <-
function(datos){
  Require("caMassClass")
  tkconfigure(console,cursor="watch")
  f<-datos$datos
  f2<-f
  e<-Met.modalDialog("Base line","Baseline correction. Bandwidth to be passed to loess","0.01",entryWidth=10,returnValOnCancel="ID_CANCEL")
  tkmessageBox(title="Base line",message="The graphical display will be launched in the R console.",icon="info",type="ok")

  for (i in 2:dim(f)[2])
  {
  windows()
  f0<-f[,c(1,i)]
  Try(bslnoff(f0, method = "loess", bw =e, plot = TRUE,
      xlab="Chemical Shift (ppm)",xlim=c(10,0),new=TRUE))
  f1<-bslnoff(f0, method = "loess", bw =e, plot = TRUE,
      xlab="Chemical Shift (ppm)",xlim=c(10,0),new=TRUE)
  f2[,i]<-f1[,2]
  }

  eleccion<-tclvalue(tkmessageBox(title="Baseline", message="Would you like to correct any adjustment?",icon="question",type="yesno",default="no"))

  decision<-"yes"
  while (decision!="no")
  {
if (eleccion=="yes")
{
a<-Met.modalDialog2("Baseline","Which sample would you like to correct?        ","Bandwidth","","0.01",entryWidth=10,returnValOnCancel="ID_CANCEL")
f0<-f[,c(1,a$a)]

f1<-bslnoff(f0, method = "loess", bw = a$b, plot = TRUE,xlab="ppm")
f2[,a$a]<-f1[,2] 
}
decision<-tclvalue(tkmessageBox(message="Correct another sample?",icon="question",type="yesno",default="no"))
  }
  memory<<-memory+1
  datos$datos<<-f2
  memory.data[[memory]]<<-list(generation=memory,datos=datos$datos,
  info=datos$info)     #For undo function
  tkconfigure(console,cursor="arrow")
}

