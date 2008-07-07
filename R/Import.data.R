`Import.data` <-
function(){
  tkconfigure(console,cursor="watch")
  memory<<-1
  tkmessageBox(title="Import Data",message="Select the the first Bruker spectrum",icon="info",type="ok")
  spectrum<-tclvalue(tkgetOpenFile()) #Load the spectrun(bin format)
  a<-unlist(strsplit(spectrum,"/"))
  fin<-length(a)-1
  b<-paste(a[1],sep="/")
  for(i in 2:fin)
  {
b<-paste(b,a[i],sep="/")
  }
  characteristic<-paste(b,"procs",sep="/") #Load the information
  ficha<-readLines(con = characteristic, n = -1, ok = TRUE, warn = TRUE,
          encoding = "Latin-1")
  n.points<-as.numeric(unlist(strsplit(ficha[83]," "))[2])
  offset<-as.numeric(unlist(strsplit(ficha[61]," "))[2]) 
  swh<-as.numeric(unlist(strsplit(ficha[85]," "))[2])
  sw<-swh/500
  first.ppm<-offset-sw
  c.shift<-seq(from =offset , to =first.ppm ,length.out = n.points)
  C.Shift<-data.frame()
  C.Shift<-c.shift
  S<-readBin(spectrum, what="int", n = 33000, size = NA_integer_, signed = FALSE,
        endian = .Platform$endian)
  C.Shift<-cbind(C.Shift,S)
  plot(C.Shift[,1],C.Shift[,2],"l",xlim=c(offset,first.ppm))

  answer<-tclvalue(tkmessageBox(title="Import Data",message="Import another spectrum?",icon="info",type="yesno"))

  while (answer=="yes")
  {
spectrum<-tclvalue(tkgetOpenFile()) #Load the spectrun(bin format)
a<-unlist(strsplit(spectrum,"/"))
fin<-length(a)-1
b<-paste(a[1],sep="/")
for(i in 2:fin)
{
b<-paste(b,a[i],sep="/")
}
characteristic<-paste(b,"procs",sep="/") #Load the information
ficha<-readLines(con = characteristic, n = -1, ok = TRUE, warn = TRUE,
          encoding = "Latin-1")
n.points<-as.numeric(unlist(strsplit(ficha[83]," "))[2])
offset<-as.numeric(unlist(strsplit(ficha[61]," "))[2]) 
swh<-as.numeric(unlist(strsplit(ficha[85]," "))[2])
sw<-swh/500
first.ppm<-offset-sw
c.shift<-seq(from =offset , to =first.ppm ,length.out = n.points)
S<-readBin(spectrum, what="int", n = 33000, size = NA_integer_,
 signed = FALSE,endian = .Platform$endian)
C.Shift<-cbind(C.Shift,S)
  answer<-tclvalue(tkmessageBox(title="Import Data",message="Import another spectrum?",icon="info",type="yesno"))

  }
  tkmessageBox(title="Import Data",message="Select the info file",icon="info",type="ok")
  fileName2<-tclvalue(tkgetOpenFile()) #open info file
  if (!nchar(fileName2))
    tkmessageBox(message="No file selected!")
  else
    tkmessageBox(message=paste("The selected file is",fileName2))
  info=read.table(fileName2,sep="\t",header=TRUE)
  
  ii<-order(as.vector(C.Shift[,1]))
  C.Shift<-C.Shift[ii,]
  datos<<-list(datos=as.data.frame(C.Shift),info=info)
  library('relimp')
  showData2(datos$datos,title="Import Data")
  memory.data[[memory]]<<-list(generation=memory,datos=as.data.frame(C.Shift),info=info)
  tkconfigure(console,cursor="arrow")
}

