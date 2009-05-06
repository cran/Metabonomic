Met.modalDialog2 <-
function(title,question,question2,entryInit,entryInit2,entryWidth=20,returnValOnCancel="ID_CANCEL")
{

  dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,title)
  textEntryVarTcl <- tclVar(paste(entryInit))
  textEntryVarTcl2 <- tclVar(paste(entryInit2))
  textEntryWidget <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl)
  textEntryWidget2 <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl2)

  tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text=question),textEntryWidget)
  tkgrid(tklabel(dlg,text=question2),textEntryWidget2)
  tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="       "))
  ReturnVal <- returnValOnCancel
  onOK <- function()
  {
    ReturnVal <<- list(a=as.numeric(tclvalue(textEntryVarTcl)),b=as.numeric(tclvalue(textEntryVarTcl2)))
    tkgrab.release(dlg)
    tkdestroy(dlg)
   
  }
  onCancel <- function()
  {
    ReturnVal <<- 0
    tkgrab.release(dlg)
    tkdestroy(dlg)
   
  }
  OK.but     <-tkbutton(dlg,text="   OK   ",command=onOK)
  Cancel.but <-tkbutton(dlg,text=" Cancel ",command=onCancel)
  tkgrid(OK.but,Cancel.but)
  tkgrid(tklabel(dlg,text="    "))
  tkraise(dlg)
  tkfocus(dlg)
  tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg);tkfocus(dlg)})
  tkbind(textEntryWidget, "<Return>", onOK)
  tkwait.window(dlg)

  return(ReturnVal)

}

