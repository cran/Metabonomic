`Met.Save.Data` <-
function()
{
  fileName<-tclvalue(tkgetSaveFile())
  write.table(datos$datos, file = fileName, append = FALSE, quote = FALSE, sep = "\t",
    eol = "\n", na = "NA", dec = ".", row.names = FALSE,
    col.names = FALSE, qmethod = c("escape", "double")) 
}

