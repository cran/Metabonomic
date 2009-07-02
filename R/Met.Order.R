Met.Order <-
function(datos)
{
  tkconfigure(console,cursor="watch")
  

  Require("hddplot")
  Try(orderFeatures(datos$datos[,-1],cl=datos$info[,categoria],FUN=aovFbyrow))
  orden=orderFeatures(datos$datos[,-1],cl=datos$info[,categoria],FUN=aovFbyrow)
 
  ordtotal.a<<-cbind(Metabolites=datos$datos[orden,1],Numeration=orden)
  showData2(ordtotal.a)
  tkconfigure(console,cursor="arrow")
}

