manual.model <-
function(title)
{
  require(tcltk)
  tt<-tktoplevel()
  tkwm.title(tt,title)
  scr <- tkscrollbar(tt, repeatinterval=5,
   command=function(...)tkyview(tl,...))
  tl<-tklistbox(tt,height=4,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
  scr2 <- tkscrollbar(tt, repeatinterval=5,
   command=function(...)tkyview(tl2,...))
  tl2<-tklistbox(tt,height=4,selectmode="single",yscrollcommand=function(...)tkset(scr2,...),background="white")
  tkgrid(tklabel(tt,text="Samples to build the model:"))
  tkgrid(tklabel(tt,text=" "))
  tkbind(tl,scr)
  tkgrid(tl,scr,columnspan=2)
  tkgrid.configure(scr,rowspan=4,sticky="nsw")
  tkgrid(tklabel(tt,text="  "))
  tkbind(tl2,scr2)
  tkgrid(tl2,scr2,columnspan=2)
  tkgrid.configure(scr2,rowspan=4,sticky="nsw")
  spectrum <-as.vector(datos$datos[,1]) 
  j=0
  Index<<-data.frame()
  for (i in (1:dim(datos$info)[1]))
  {
   tkinsert(tl,"end",paste(datos$info[i,1],datos$info[i,2]))
  }
  tkselection.set(tl,0)  # 
  onOK<-function()
  {
samp<<-as.vector(Index[,1])
tkdestroy(tt)
  }

  OK.but <-tkbutton(tt,text=" OK ",command=onOK)
  DeleteSelection <- function()
  {
  j<<-j+1
Index[j,1] <<-as.numeric(tkcurselection(tl))+1
In <- as.integer(tkcurselection(tl))
   tkinsert(tl2,"end",paste(datos$info[as.numeric(tkcurselection(tl))+1,1],
datos$info[as.numeric(tkcurselection(tl))+1,2]))
  }
  DeleteSelection.but <- tkbutton(tt,text=" ADD ",command=DeleteSelection)
  tkgrid(tklabel(tt,text="    "))
  tkgrid(DeleteSelection.but)
  tkgrid(tklabel(tt,text="    "))
  tkgrid(OK.but,sticky="e")
  tkfocus(tt)
  tkwait.window(tt)

}

