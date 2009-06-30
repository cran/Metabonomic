Met.Load.Data <-
function()
{
  Require("relimp")
  #Load data file
  memory<<-1
  ReturnVal <- tkmessageBox(title="Load Data",message="Select the input file",icon="info",type="ok")
  Require("tcltk")
  fileName<-tclvalue(tkgetOpenFile())
  a<-unlist(strsplit(fileName,"/"))
  fin<-length(a)-1
  b<-paste(a[1],sep="/")
  for(i in 2:fin)
  {
b<-paste(b,a[i],sep="/")
  }
setwd(b)

  if (!nchar(fileName))
    tkmessageBox(message="No file selected!")
  else
    tkmessageBox(message=paste("The selected file is",fileName))
  tkconfigure(console,cursor="watch")

  datos<-read.table(fileName,sep="\t",header=FALSE)
  for (i in 1:dim(datos)[2])
if (is.numeric(datos[1,i])==FALSE)
datos<-datos[,-i]

  #Load info file
  ReturnVal <- tkmessageBox(title="Load Data",message="Select the info file",icon="info",type="ok")
  fileName2<-tclvalue(tkgetOpenFile())
  if (!nchar(fileName2))
    tkmessageBox(message="No file selected!")
  else
    tkmessageBox(message=paste("The selected file is",fileName2))
  info=read.table(fileName2,sep="\t",header=TRUE)
 # showData2(datos,title="Load data")
  datos<<-list(datos=datos,info=info)
  memory.data[[memory]]<<-list(generation=memory,datos=datos,info=info)
  tclvalue(tkmessageBox(message=" Spectra file loaded",icon="info",type="ok",default="ok"))
 
tkconfigure(console,cursor="arrow")

}

