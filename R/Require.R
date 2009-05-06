Require <-
function(pkg)
{
  if (data.class(result<-try(.find.package(pkg),TRUE))=="try-error")
  {
        tkmessageBox(title="An error has occured!",message=paste("Cannot find package \"",pkg,"\"",sep=""),icon="error",type="ok")
        return (FALSE)
  }
  else
  {
        require(pkg,character.only=TRUE)
        return (TRUE)
  }
}

