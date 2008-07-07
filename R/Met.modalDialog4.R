`Met.modalDialog4` <-
function(title,subtitle,question,question2,question3, entryInit,entryInit2,entryInit3, entryWidth=8,returnValOnCancel="ID_CANCEL")
{
  dlg <- tktoplevel()
  fontHeading <- tkfont.create(family="times",size=12,weight="bold")
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,title)
  textEntryVarTcl <- tclVar(paste(entryInit))
  textEntryVarTcl2 <- tclVar(paste(entryInit2))
  textEntryVarTcl3 <- tclVar(paste(entryInit3))
  textEntryWidget <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl)
  textEntryWidget2 <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl2)
  textEntryWidget3 <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl3)
  tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="       "),tklabel(dlg,text=subtitle,font=fontHeading),tklabel(dlg,text="       "),sticky="w")
  tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="       "),tklabel(dlg,text=question),textEntryWidget,tklabel(dlg,text="       "),sticky="w")
  tkgrid(tklabel(dlg,text="       "),tklabel(dlg,text=question2),textEntryWidget2,tklabel(dlg,text="       "),sticky="w")
  tkgrid(tklabel(dlg,text="       "),tklabel(dlg,text=question3),textEntryWidget3,tklabel(dlg,text="       "),sticky="w")
  tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="       "))
  ReturnVal <- returnValOnCancel
  onOK <- function()
  {
    ReturnVal <<- list(a=as.numeric(tclvalue(textEntryVarTcl)),b=as.numeric(tclvalue(textEntryVarTcl2)),d=as.numeric(tclvalue(textEntryVarTcl3)))
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
  tkgrid(tklabel(dlg,text="    "),OK.but,Cancel.but,sticky="e")
  tkgrid(tklabel(dlg,text="    "))
  tkraise(dlg)
  tkfocus(dlg)
  tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg);tkfocus(dlg)})
  tkbind(textEntryWidget, "<Return>", onOK)
  tkwait.window(dlg)
  return(ReturnVal)
}

