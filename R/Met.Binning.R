Met.Binning <-
function(datos)
{
  tkconfigure(console,cursor="watch")

  library('relimp')
  Ma<-datos$datos
  longitud<-Ma[dim(Ma)[1],1]-Ma[1,1]
  require(PROcess)
  #Require("caMassClass")
  paso<-Met.modalDialog("Binning","Size of the'bin'       ","0.04",entryWidth=8)
  breaks<-round(longitud/paso)
  Try(binning(Ma, breaks = breaks))
  Bin<-binning(Ma, breaks = breaks)
  c.s<-round(Bin[,1],3)
  Bin[,1]<-c.s
  showData2(Bin)

  #fix(Bin)
  datos$datos<<-as.data.frame(Bin)
  memory<<-memory+1
  memory.data[[memory]]<<-list(generation=memory,datos=datos$datos,
  info=datos$info)     #For undo function

  tkconfigure(console,cursor="arrow")
 
}

