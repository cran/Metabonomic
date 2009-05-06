Try <-
function(expr)
{
  if (data.class(result<-try(expr,TRUE))=="try-error")
  {
  tkmessageBox(title="An error has occured!",message=as.character(result),icon="error",type="ok")
    tkconfigure(console,cursor="arrow")
  }
  else
  return (result)
}

