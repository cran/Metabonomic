Met.Norm <-
function(datos)
{
  tkconfigure(console,cursor="watch")
  require("clusterSim")
  c.s<-datos$datos[,1]
  x<-as.data.frame(datos$datos[,-1])
  x<-as.matrix(x)
  a<-tclvalue(Met.RadioBox2("Normalization","Standardization ((x-mean)/sd)",
"Weber standardization ((x-Me)/MAD)", "Unitization ((x-mean)/range)","Unitization with zero minimum ((x-min)/range)",
"                 Normalization in range <-1,1> ((x-mean)/max(abs(x-mean))) ", "Quotient transformation (x/sd)" ,"Quotient transformation (x/range)",
"Quotient transformation (x/max)","Quotient transformation (x/mean)","Quotient transformation (x/sum)","Quotient transformation (x/sqrt(SSQ))",
"n1","n2","n3","n4","n5","n6","n7","n8","n9","n10","n11"))
  Try(data.Normalization (x,type=a))
  y<-data.Normalization (x,type=a)
  z<-cbind(c.s,y)
  if (a=="n10"){
  y<-y*1000
  z<-cbind(c.s,y)}
  datos$datos<<-z
  memory<<-memory+1
 
  memory.data[[memory]]<<-list(generation=memory,datos=datos$datos,
  info=datos$info)     #For undo function
  tkconfigure(console,cursor="arrow")
}

