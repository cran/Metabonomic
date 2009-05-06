Met.B.STAT <-
function(datos)
{
  tkconfigure(console,cursor="watch")

  info<-datos$info
  Ma<-datos$datos[,-1]
  orden=1
  x<-as.numeric(Ma[orden,])
  mean(x)
  
  main<-paste("Metabolite = ",round(datos$datos[orden,1],3))
  etiqueta=info[,categoria]
  color="white"
  color2="black"
  color3="red"
  col=list(axis="black",lab="black",main="black",sub="black")
  subt=""
  size=list(cex=0.7,cex.axis=1,cex.lab=1,cex.main=1,cex.sub=1)
  angle=60

  f2a<-function()
  {
tt <- tktoplevel()
plotFunction <<- function(color,color2,etiqueta,xlab,ylab,main,subt)
{
 main<-paste("Metabolite = ",round(datos$datos[orden,1],3))
x<-as.numeric(Ma[orden,])
mean(x)
  par(bg=color)
plot(x,y=NULL,"n",col=color2,cex.axis=size$cex.axis,cex.lab=size$cex.lab,
cex.main=size$cex.main,cex.sub=size$cex.sub,cex=size$cex,
main=main, sub=subt,col.axis=col$axis,col.lab=col$lab,col.main=col$main,col.sub=col$sub,
pch=10,xlab="Index", ylab="Value")
text(x,labels=etiqueta,cex=size$cex,col=color2)
abline(h = mean(x), v = NULL, reg = NULL,coef = NULL,col=color3)

}
img <- tkrplot(tt,plotFunction(color,color2,etiqueta,xlab,ylab,main,subt),hscale=1.5,vscale=1.5)

change.color.bakground<-function(){
  Require("tcltk")
  tt <- tktoplevel()
  tkwm.title(tt,"Color Selection")

  ChangeColor1 <- function()
    {
     color <<- tclvalue(.Tcl(paste("tk_chooseColor",.Tcl.args(initialcolor=color,title="Choose a color"))))
     if (nchar(color)>0)
        tkconfigure(canvas1,bg=color)
     }
ChangeColor2 <- function()
    {
     color2 <<- tclvalue(.Tcl(paste("tk_chooseColor",.Tcl.args(initialcolor=color2,title="Choose a color"))))
     if (nchar(color2)>0)
        tkconfigure(canvas2,bg=color2)
     }
ChangeColor2b <- function()
    {
    color3 <<- tclvalue(.Tcl(paste("tk_chooseColor",.Tcl.args(initialcolor=color3,title="Choose a color"))))
     if (nchar(color3)>0)
        tkconfigure(canvas2b,bg=color3)
     }
ChangeColor3 <- function()
    {
     col$axis <<- tclvalue(.Tcl(paste("tk_chooseColor",.Tcl.args(initialcolor=col$axis,title="Choose a color"))))
     if (nchar(col$axis)>0)
        tkconfigure(canvas3,bg=col$axis)
     }
ChangeColor4 <- function()
    {
     col$lab <<- tclvalue(.Tcl(paste("tk_chooseColor",.Tcl.args(initialcolor=col$lab,title="Choose a color"))))
     if (nchar(col$lab)>0)
        tkconfigure(canvas4,bg=col$lab)
     }
ChangeColor5 <- function()
    {
     col$main <<- tclvalue(.Tcl(paste("tk_chooseColor",.Tcl.args(initialcolor=col$main,title="Choose a color"))))
     if (nchar(col$main)>0)
        tkconfigure(canvas5,bg=col$main)
     }
ChangeColor6 <- function()
    {
     col$sub <<- tclvalue(.Tcl(paste("tk_chooseColor",.Tcl.args(initialcolor=col$sub,title="Choose a color"))))
     if (nchar(col$sub)>0)
        tkconfigure(canvas6,bg=col$sub)
     }
  canvas1 <- tkcanvas(tt,width="80",height="25",bg=color)
    canvas2 <- tkcanvas(tt,width="80",height="25",bg=color2)
canvas2b <- tkcanvas(tt,width="80",height="25",bg=color3)
  canvas3 <- tkcanvas(tt,width="80",height="25",bg=col$axis)
canvas4 <- tkcanvas(tt,width="80",height="25",bg=col$lab)
canvas5 <- tkcanvas(tt,width="80",height="25",bg=col$main)
canvas6 <- tkcanvas(tt,width="80",height="25",bg=col$sub)
    ChangeColor.button1 <- tkbutton(tt,text="Change Color",command=function() ChangeColor1())
    ChangeColor.button2 <- tkbutton(tt,text="Change Color",command=function() ChangeColor2())
    ChangeColor.button2b <- tkbutton(tt,text="Change Color",command=function() ChangeColor2b())
ChangeColor.button3 <- tkbutton(tt,text="Change Color",command=function() ChangeColor3())
ChangeColor.button4 <- tkbutton(tt,text="Change Color",command=function() ChangeColor4())
ChangeColor.button5 <- tkbutton(tt,text="Change Color",command=function() ChangeColor5())
ChangeColor.button6 <- tkbutton(tt,text="Change Color",command=function() ChangeColor6())
onOK <- function()
  {
    tkrreplot(img,plotFunction(color,color2,etiqueta,xlab,ylab,main,subt))
    tkdestroy(tt)
     }
  onCancel <- function()
  {
    ReturnVal <<- 0
    tkdestroy(tt)
   }
  OK.but     <-tkbutton(tt,text="   OK   ",command=onOK)
  Cancel.but <-tkbutton(tt,text=" Cancel ",command=onCancel)
  tkgrid(tklabel(tt,text="    "))
    tkgrid(tklabel(tt,text="Background    "),canvas1,ChangeColor.button1)
tkgrid(tklabel(tt,text="    "))
tkgrid(tklabel(tt,text="Label    "),canvas2,ChangeColor.button2)
   tkgrid(tklabel(tt,text="    "))
tkgrid(tklabel(tt,text="Mean    "),canvas2b,ChangeColor.button2b)
   tkgrid(tklabel(tt,text="    "))
tkgrid(tklabel(tt,text="Axis    "),canvas3,ChangeColor.button3)
   tkgrid(tklabel(tt,text="    "))
tkgrid(tklabel(tt,text="Legend    "),canvas4,ChangeColor.button4)
   tkgrid(tklabel(tt,text="    "))
tkgrid(tklabel(tt,text="Title    "),canvas5,ChangeColor.button5)
   tkgrid(tklabel(tt,text="    "))
tkgrid(tklabel(tt,text="Subtitle    "),canvas6,ChangeColor.button6)
   tkgrid(tklabel(tt,text="    "))
tkgrid(OK.but,Cancel.but)
   }
met<-function()
{
require(tcltk)
tt<-tktoplevel()
scr <- tkscrollbar(tt, repeatinterval=5,
   command=function(...)tkyview(tl,...))
tl<-tklistbox(tt,height=10,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
tkgrid(tklabel(tt,text="Chemical shift region: "))
tkgrid(tl,scr)
tkgrid.configure(scr,rowspan=10,sticky="nsw")
C.S <- datos$datos[,1]
for (i in (1:dim(datos$datos)[1]))
{
tkinsert(tl,"end",C.S[i])
}
tkselection.set(tl,orden-1) 

OnOK <- function()
{
orden <<- as.numeric(tkcurselection(tl))+1
tkrreplot(img,plotFunction(color,color2,etiqueta,xlab,ylab,main,subt))
tkdestroy(tt)
}
OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
tkgrid(OK.but)
tkfocus(tt)
}
by.name<-function(info)
{
    etiqueta<<-info[,1]
tkrreplot(img,plotFunction(color,color2,etiqueta,xlab,ylab,main,subt))
  }

by.type<-function(info)
{
   etiqueta<<-info[,categoria] 
#plotFunction(color,color2,etiqueta,xlab,ylab,main,subt)
tkrreplot(img,plotFunction(color,color2,etiqueta,xlab,ylab,main,subt))}
modalDialog4 <- function(title,question,question2,question3,question4, entryInit,entryInit2,entryInit3,entryInit4, entryWidth=20,returnValOnCancel="ID_CANCEL")
  {
  dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,title)
  textEntryVarTcl <- tclVar(paste(entryInit))
  textEntryVarTcl2 <- tclVar(paste(entryInit2))
  textEntryVarTcl3 <- tclVar(paste(entryInit3))
  textEntryVarTcl4 <- tclVar(paste(entryInit4))
  textEntryWidget <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl)
  textEntryWidget2 <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl2)
  textEntryWidget3 <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl3)
  textEntryWidget4 <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl4)

  tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="Change the Legend"),sticky="w")
   tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text=question),textEntryWidget,sticky="e")
  tkgrid(tklabel(dlg,text=question2),textEntryWidget2,sticky="e")
  tkgrid(tklabel(dlg,text=question3),textEntryWidget3,sticky="e")
  tkgrid(tklabel(dlg,text=question4),textEntryWidget4,sticky="e")
  tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="       "))
  
ReturnVal <- returnValOnCancel
  onOK <- function()
  {
    xlab<<-as.character(tclvalue(textEntryVarTcl))
    ylab<<-as.character(tclvalue(textEntryVarTcl2))
    main<<-as.character(tclvalue(textEntryVarTcl3))
    subt<<-as.character(tclvalue(textEntryVarTcl4))
    tkdestroy(dlg)
tkrreplot(img,plotFunction(color,color2,etiqueta,xlab,ylab,main,subt))  
   }
  onCancel <- function()
  {
    ReturnVal <<- 0
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
Text.size<- function()
{
dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,"Text Size")
SliderValue1 <- tclVar("0.7")
SliderValue2 <- tclVar("1")
SliderValue3 <- tclVar("1")
SliderValue4 <- tclVar("2")
SliderValue5 <- tclVar("1")
SliderValueLabel1 <- tklabel(dlg,text=as.character(tclvalue(SliderValue1)))
SliderValueLabel2 <- tklabel(dlg,text=as.character(tclvalue(SliderValue2)))
SliderValueLabel3 <- tklabel(dlg,text=as.character(tclvalue(SliderValue3)))
SliderValueLabel4 <- tklabel(dlg,text=as.character(tclvalue(SliderValue4)))
SliderValueLabel5 <- tklabel(dlg,text=as.character(tclvalue(SliderValue5)))
tkconfigure(SliderValueLabel1,textvariable=SliderValue1)
tkconfigure(SliderValueLabel2,textvariable=SliderValue2)
tkconfigure(SliderValueLabel3,textvariable=SliderValue3)
tkconfigure(SliderValueLabel4,textvariable=SliderValue4)
tkconfigure(SliderValueLabel5,textvariable=SliderValue5)

slider1 <- tkscale(dlg, from=4, to=0,showvalue=F, variable=SliderValue1,
                   resolution=0.1, orient="horizontal")
slider2 <- tkscale(dlg, from=4, to=0,showvalue=F, variable=SliderValue2,
                   resolution=0.1, orient="horizontal")
slider3 <- tkscale(dlg, from=4, to=0,showvalue=F, variable=SliderValue3,
                   resolution=0.1, orient="horizontal")
slider4 <- tkscale(dlg, from=4, to=0,showvalue=F, variable=SliderValue4,
                   resolution=0.1, orient="horizontal")
slider5 <- tkscale(dlg, from=4, to=0,showvalue=F, variable=SliderValue5,
                   resolution=0.1, orient="horizontal")
tkgrid(tklabel(dlg,text="       "))
tkgrid(tklabel(dlg,text="Text Size"),sticky="w")
tkgrid(tklabel(dlg,text="       "))
tkgrid(tklabel(dlg,text="Labels : "),SliderValueLabel1,slider1)
tkgrid(tklabel(dlg,text="Axis : "),SliderValueLabel2,slider2)
tkgrid(tklabel(dlg,text="Legend : "),SliderValueLabel3,slider3)
tkgrid(tklabel(dlg,text="Title : "),SliderValueLabel4,slider4)
tkgrid(tklabel(dlg,text="Subtitle : "),SliderValueLabel5,slider5)
tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="       "))
  
onOK <- function()
  {
size<<-list(
    cex=as.numeric(tclvalue(SliderValue1)),cex.axis=as.numeric(tclvalue(SliderValue2)),
    cex.lab=as.numeric(tclvalue(SliderValue3)),cex.main=as.numeric(tclvalue(SliderValue4)),
cex.sub=as.numeric(tclvalue(SliderValue5)))
    tkdestroy(dlg)
tkrreplot(img,plotFunction(color,color2,etiqueta,xlab,ylab,main,subt))  
   }
  onCancel <- function()
  {
    ReturnVal <<- 0
    tkdestroy(dlg)
   }
  OK.but     <-tkbutton(dlg,text="   OK   ",command=onOK)
  Cancel.but <-tkbutton(dlg,text=" Cancel ",command=onCancel)
  tkgrid(OK.but,Cancel.but)
  tkgrid(tklabel(dlg,text="    "))
  tkraise(dlg)
  tkfocus(dlg)
  tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg);tkfocus(dlg)})
  tkwait.window(dlg)

  return(ReturnVal)
  }
CopyToClip <- function()
{
fileName<-tclvalue(tkgetSaveFile())
pdf(file = fileName)
plotFunction(color,color2,etiqueta, xlab,ylab,main,subt)
dev.off()
windows()
plotFunction(color,color2,etiqueta, xlab,ylab,main,subt)
tkdestroy(tt)
  }

tkwm.title(tt,"Interactive Plot")
Menu <- tkmenu(tt,borderwidth=40)
tkconfigure(tt, menu=Menu)
tkadd(Menu, "command", label="Background color",
      command=change.color.bakground)
labels <- tkmenu(Menu,borderwidth=40,tearoff=FALSE)
tkadd(labels, "command", label="Name",
      command=function() by.name(info))
tkadd(labels, "command", label="Type",
      command=function() by.type(info))
tkadd(Menu, "cascade", label="Labels",menu=labels)
Legend <- tkmenu(Menu,borderwidth=40,tearoff=FALSE)
tkadd(Legend, "command", label="Text",
      command=function() modalDialog4("Legend   ","Xlab   ","Ylab   ","Main Title   ","Subtitle", xlab,ylab,main,subt))
tkadd(Legend, "command", label="Size",
      command=function() Text.size())
tkadd(Menu, "cascade", label="Legend",menu=Legend)

tkadd(Menu, "command", label="Chemical Shift Region",
      command=function() met())

copy.but <- tkbutton(tt,text="Copy to Clipboard",command=CopyToClip)
tkgrid(img)
tkgrid(copy.but)
tkfocus(tt)
tkraise(tt)
tkwait.window(tt)
}
  f2a()
  tkconfigure(console,cursor="arrow")
}

