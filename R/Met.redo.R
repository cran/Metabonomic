Met.redo <-
function()
{
  memory=memory+1
  Try(memory.data[[memory]])
  datos<<-memory.data[[memory]]
  showData2(datos$datos,title="Data")
  memory<<-memory
}

