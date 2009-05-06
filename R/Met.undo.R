Met.undo <-
function()
{
  if (memory<=1)
  {
tkmessageBox(title="Undo",message="You are in the first level",icon="info",type="ok")
  }
  if (memory>1)
  {
memory<<-memory-1
datos<<-memory.data[[memory]]
showData2(datos$datos,title="Data")
  }
}

