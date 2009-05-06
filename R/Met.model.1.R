Met.model.1 <-
function(title)
{
  Require("tcltk")
  tt <- tktoplevel()
  tkgrab.set(tt)
  tkwm.title(tt,title)
  rb1 <- tkradiobutton(tt)
  rb2 <- tkradiobutton(tt)
  rbValue<-tclVar("random")
  tkconfigure(rb1,variable=rbValue,value="manual")
  tkconfigure(rb2,variable=rbValue,value="random")
 
  tkgrid(tklabel(tt,text="                                                      "))
  tkgrid(tklabel(tt,text="Building the model:"),sticky="w")
  tkgrid(tklabel(tt,text="                                                      "))
  tkgrid(tklabel(tt,text="Manual selection."),rb1,sticky="e")
  tkgrid(tklabel(tt,text="Random selection."),rb2,sticky="e")
  tkgrid(tklabel(tt,text="                                                      "))
  OnOK <- function()
  {
   m.model <<- as.character(tclvalue(rbValue))
    tkdestroy(tt)
  }

  OK.but <- tkbutton(tt,text="OK",command=OnOK)
  tkgrid(OK.but)
  tkgrid(tklabel(tt,text="                                                      "))
  tkraise(tt)
  tkfocus(tt)
  tkwait.window(tt)
}

