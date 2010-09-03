Met.Checkbox1 <-
function(title,question1,question2,answer1,answer2,answer3,answer4)
{
  Require("tcltk")
  tt <- tktoplevel()
  tkgrab.set(tt)
  tkwm.title(tt,title)
  cb <- tkcheckbutton(tt)
  cb2 <-tkcheckbutton(tt)
  cbValue <-tclVar("0")
  cbValue2 <-tclVar("1")
  tkconfigure(cb,variable=cbValue)
  tkconfigure(cb2,variable=cbValue2)
  tkgrid(tklabel(tt,text="                                                      "))
  tkgrid(tklabel(tt,text="Selection of the PCA parameters:"),sticky="w")
  tkgrid(tklabel(tt,text="                                                      "))
  tkgrid(tklabel(tt,text=question1),cb,sticky="e")
  tkgrid(tklabel(tt,text=question2),cb2,sticky="e")
  tkgrid(tklabel(tt,text="                                                      "))
  OnOK <- function()
  {
tkgrab.release(tt)
cbVal <- as.character(tclvalue(cbValue))
cbVal2 <- as.character(tclvalue(cbValue2)) 
    tkdestroy(tt)
  }

  OK.but <- tkbutton(tt,text="OK",command=OnOK)
  tkgrid(OK.but)
  tkgrid(tklabel(tt,text="                                                      "))
  tkraise(tt)
  tkfocus(tt)
  tkwait.window(tt)
  return(c(as.character(cbValue),as.character(cbValue2)))
}

