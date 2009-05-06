Met.Metabolites <-
function(datos, externa){
  tkconfigure(console,cursor="watch")
  list.spectrum<-function()
  {

require(tcltk)
tt<-tktoplevel()
scr <- tkscrollbar(tt, repeatinterval=5,
   command=function(...)tkyview(tl,...))
tl<-tklistbox(tt,height=4,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")

scr2 <- tkscrollbar(tt, repeatinterval=5,
   command=function(...)tkyview(tl2,...))
tl2<-tklistbox(tt,height=4,selectmode="single",yscrollcommand=function(...)tkset(scr2,...),background="white")

tkgrid(tklabel(tt,text="Metabolites selected:"))
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

for (i in (1:length(spectrum)))
{
   tkinsert(tl,"end",paste(spectrum[i],"ppm"))
}
tkselection.set(tl,0)  # 
onOK<-function()
{
a<-Index[,1]
datos<<-list(datos=datos$datos[a,],info=datos$info)

tkdestroy(tt)
}

OK.but <-tkbutton(tt,text=" OK ",command=onOK)
Cancel.but<-tkbutton(tt,text=" Cancel ",command= function() tkdestroy(tt)) 
DeleteSelection <- function()
{
    j<<-j+1

    Index[j,1] <<-as.numeric(tkcurselection(tl))+1
    Index[j,2] <<-spectrum[as.numeric(tkcurselection(tl))+1]
    In <- as.integer(tkcurselection(tl))
   tkinsert(tl2,"end",spectrum[as.numeric(tkcurselection(tl))+1])

}
DeleteSelection.but <- tkbutton(tt,text=" ADD ",command=DeleteSelection)
tkgrid(tklabel(tt,text="    "))
tkgrid(DeleteSelection.but)
tkgrid(tklabel(tt,text="    "))
tkgrid(OK.but,Cancel.but,sticky="e")
tkfocus(tt)
  tkwait.window(tt)

  }
  list.spectrum()  
  memory<<-memory
  memory.data[[memory]]<<-list(generation=memory,datos=datos$datos,
  info=datos$info)     #For undo function
  datos<<-datos
 
  tkconfigure(console,cursor="arrow")
}

