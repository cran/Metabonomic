Met.RadioBox1 <-
function(title,question1,question2, question4, answer1,answer2, answer4)
{
  require(tcltk)
  tt <- tktoplevel()
  tkwm.title(tt,title)
  rb1 <- tkradiobutton(tt)
  rb2 <- tkradiobutton(tt)
  rb4 <- tkradiobutton(tt)
  rbValue<-tclVar(answer1)
  tkconfigure(rb1,variable=rbValue,value=answer1)
  tkconfigure(rb2,variable=rbValue,value=answer2)
  tkconfigure(rb4,variable=rbValue,value=answer4)
  tkgrid(tklabel(tt,text="                                                      "))
  tkgrid(tklabel(tt,text="Which method would you like to use?"))
  tkgrid(tklabel(tt,text="                                                      "))
  tkgrid(tklabel(tt,text=question1),rb1,sticky="e")
  tkgrid(tklabel(tt,text=question2),rb2,sticky="e")
  tkgrid(tklabel(tt,text=question4),rb4,sticky="e")
  
  OnOK <- function()
  {
    rbVal <- as.character(tclvalue(rbValue))
    tkdestroy(tt)
  }
  OK.but <- tkbutton(tt,text="OK",command=OnOK)
  tkgrid(tklabel(tt,text="                                                      "))
  tkgrid(OK.but)
  tkgrid(tklabel(tt,text="                                                      "))
  tkraise(tt)
  tkfocus(tt)
  tkwait.window(tt)
  return(as.character(rbValue))

}

