Met.spectrum.2 <-
function(xCoords)
{
  tkconfigure(console,cursor="watch")


  xCoords<-C.Shift
 require(tcltk)
  require(tkrplot)
  color="white"
  color2="black"
  col=list(axis="black",lab="black",main="black",sub="black")
  subt=""
  size=list(cex=0.7,cex.axis=1,cex.lab=1,cex.main=1,cex.sub=1)
  xlab="Chemical Shift (ppm)"
  ylab=""
  #xlim=c(10,0)
  xlim=c(xCoords[1,1],xCoords[dim(xCoords)[1],1])
  tipo="l"
  intensity=100
  espectro=list(a=2,b=0)
  main<-paste("Spectrum ")
  indexLabeled<-c()
  labeledPoints <- list()
 
  tt <- tktoplevel(background="white")
  tkwm.title(tt,"Click on a point to reference it")

  parPlotSize <- c()
  usrCoords <- c()
 
  plotFunction <- function()
  {
  params <- par(bg=color)
  plot(xCoords[,1],xCoords[,espectro$a],type=tipo,xlab=xlab,ylab=ylab,main=main,xlim=xlim,
sub=subt,cex.axis=size$cex.axis,cex.lab=size$cex.lab, xaxp=c(xCoords[dim(xCoords)[1],1],xCoords[1,1],20),
cex.main=size$cex.main,cex.sub=size$cex.sub,col.axis=col$axis,
col.lab=col$lab,col.main=col$main,col.sub=col$sub,cex=size$cex,
col=color2,ylim=range(xCoords[,espectro$a])/(0.01*intensity))
  if (espectro$b!=0)
{
  lines(xCoords[,1],xCoords[,espectro$b],type=tipo,col="red")
}
  if (length(indexLabeled)>0)
    for (i in (1:length(indexLabeled)))
    {
      indexClosest <- indexLabeled[i]
    }
  parPlotSize <<- par("plt")
  usrCoords   <<- par("usr")
  par(params)
  }

  img <- tkrplot(tt,fun=plotFunction,hscale=1.5,vscale=1.5)
  tkgrid(img)
  
  Met.reference<- function(entryWidth=5,returnValOnCancel="ID_CANCEL")
  {
  dlg <- tktoplevel()
  tkwm.title(dlg,"Reference")
  textEntryVarTcl <- tclVar(paste(" "))
  textEntryWidget <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl)
  textEntryVarTcl2 <- tclVar(paste(format(xPlotCoord,digits=3)))
  textEntryWidget2 <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl2,state="disabled")
  tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="Old Shift:   "),textEntryWidget2)
  tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="New Shift:   "),textEntryWidget)
  tkgrid(tklabel(dlg,text="       "))
  ReturnVal <- returnValOnCancel
  onOK <- function()
  {
    Reference <- as.numeric(tclvalue(textEntryVarTcl))-as.numeric(tclvalue(textEntryVarTcl2))
    xCoords[,1]<<-xCoords[,1]+Reference
tkrreplot(img,plotFunction())
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
  }

  labelClosestPoint <- function(xClick,yClick,imgXcoords,imgYcoords)
  {
  squared.Distance <- (xClick-imgXcoords)^2 + (yClick-imgYcoords)^2
  indexClosest <- which.min(squared.Distance)
  indexLabeled <<- c(indexLabeled,indexClosest)
  tkrreplot(img)
  }

  OnLeftClick <- function(x,y)
  {
  xClick <- x
yClick <- y
  require(tcltk)
  width  <- as.numeric(tclvalue(tkwinfo("reqwidth",img)))
  height <- as.numeric(tclvalue(tkwinfo("reqheight",img)))
xMin <- parPlotSize[1] * width
xMax <- parPlotSize[2] * width
yMin <- parPlotSize[3] * height
yMax <- parPlotSize[4] * height
rangeX <- usrCoords[2] - usrCoords[1]
rangeY <- usrCoords[4] - usrCoords[3]

  imgXcoords <- (xCoords[,1]-usrCoords[1])*(xMax-xMin)/rangeX + xMin
  imgYcoords <- (xCoords[,espectro$a]-usrCoords[3])*(yMax-yMin)/rangeY + yMin

  xClick <- as.numeric(xClick)+0.5
  yClick <- as.numeric(yClick)+0.5
  yClick <- height - yClick

  xPlotCoord <<- usrCoords[1]+(xClick-xMin)*rangeX/(xMax-xMin)
  yPlotCoord <- usrCoords[3]+(yClick-yMin)*rangeY/(yMax-yMin)

Met.reference()
  }

  tkbind(img, "<Button-1>",OnLeftClick)
  tkconfigure(img,cursor="crosshair")
 

  change.color.bakground<-function()
  {
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
  canvas3 <- tkcanvas(tt,width="80",height="25",bg=col$axis)
canvas4 <- tkcanvas(tt,width="80",height="25",bg=col$lab)
canvas5 <- tkcanvas(tt,width="80",height="25",bg=col$main)
canvas6 <- tkcanvas(tt,width="80",height="25",bg=col$sub)
    ChangeColor.button1 <- tkbutton(tt,text="Change Color",command=function() ChangeColor1())
    ChangeColor.button2 <- tkbutton(tt,text="Change Color",command=function() ChangeColor2())
ChangeColor.button3 <- tkbutton(tt,text="Change Color",command=function() ChangeColor3())
ChangeColor.button4 <- tkbutton(tt,text="Change Color",command=function() ChangeColor4())
ChangeColor.button5 <- tkbutton(tt,text="Change Color",command=function() ChangeColor5())
ChangeColor.button6 <- tkbutton(tt,text="Change Color",command=function() ChangeColor6())
onOK <- function()
  {
    tkrreplot(img,plotFunction())
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
tkrreplot(img,plotFunction())  
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

  Zoom<-function()
  {
dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,"Zoom")
SliderValue1 <- tclVar(round(xCoords[1,1],3))
SliderValue2 <- tclVar(round(xCoords[dim(xCoords)[1],1],3))
SliderValueLabel1 <- tklabel(dlg,text=as.character(tclvalue(SliderValue1)))
SliderValueLabel2 <- tklabel(dlg,text=as.character(tclvalue(SliderValue2)))
tkconfigure(SliderValueLabel1,textvariable=SliderValue1)
tkconfigure(SliderValueLabel2,textvariable=SliderValue2)
slider1 <- tkscale(dlg, from=xCoords[dim(xCoords)[1],1], to=xCoords[1,1],showvalue=F, variable=SliderValue1,
                   resolution=0.01, orient="horizontal",length=300)
slider2 <- tkscale(dlg,  from=xCoords[dim(xCoords)[1],1], to=xCoords[1,1],showvalue=F, variable=SliderValue2,
                   resolution=0.01, orient="horizontal",length=300)
tkgrid(tklabel(dlg,text="       "))
tkgrid(tklabel(dlg,text="Zoom"),sticky="w")
tkgrid(tklabel(dlg,text="       "))
tkgrid(tklabel(dlg,text="From : "),SliderValueLabel1,slider1)
tkgrid(tklabel(dlg,text="To : "),SliderValueLabel2,slider2)
tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="       "))
  
onOK <- function()
  {
if(as.numeric(tclvalue(SliderValue1))>as.numeric(tclvalue(SliderValue2)))
{
xlim<<-c(as.numeric(tclvalue(SliderValue1)),as.numeric(tclvalue(SliderValue2)))
}
if(as.numeric(tclvalue(SliderValue1))<as.numeric(tclvalue(SliderValue2)))
{
xlim<<-c(as.numeric(tclvalue(SliderValue2)),as.numeric(tclvalue(SliderValue1)))
}
    tkdestroy(dlg)
tkrreplot(img,plotFunction())
}
 onCancel <- function()
  {
    ReturnVal <<- 0
    tkdestroy(dlg)
   }
  OK.but     <-tkbutton(dlg,text="   OK   ",command=onOK)
  Cancel.but <-tkbutton(dlg,text=" Cancel ",command=onCancel)
  tkgrid(OK.but,Cancel.but,sticky="e")
  tkgrid(tklabel(dlg,text="    "))
  tkraise(dlg)
  tkfocus(dlg)
  tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg);tkfocus(dlg)})
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
tkrreplot(img,plotFunction())  
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

  histogram<-function()
  {
 tipo<<-"h"
tkrreplot(img,plotFunction())
  }
  line<-function() 
  { 
tipo<<-"l"
tkrreplot(img,plotFunction())
  }

  CopyToClip <- function()
  {
fileName<-tclvalue(tkgetSaveFile())
pdf(file = fileName)
plotFunction()
dev.off()
windows()
plotFunction()
  }
  onok <- function()
  {
C.Shift<<-xCoords
tkdestroy(tt)
  }
  c.spectrum<-function()
  {
espectro$b<<-0
change.espectrum()
  }
  tkwm.title(tt,"Click on a point to reference it")
  Menu <- tkmenu(tt,borderwidth=40)
  tkconfigure(tt, menu=Menu)
  tkadd(Menu, "command", label="Background color",
        command=change.color.bakground)
  legent <- tkmenu(Menu,borderwidth=40,tearoff=FALSE)
  tkadd(legent, "command", label="Text",
        command=function() modalDialog4("Legend   ","Xlab   ","Ylab   ","Main Title   ","Subtitle", xlab,ylab,main,subt))
  tkadd(legent, "command", label="Size",
        command=function() Text.size())
  tkadd(Menu, "cascade", label="Legend",menu=legent)
#  tkadd(Menu, "command", label="Spectrum",
#        command=function() c.spectrum())
  tkadd(Menu, "command", label="Zoom",
        command=function() Zoom())
  plot.type <- tkmenu(Menu,borderwidth=40,tearoff=FALSE)
  tkadd(plot.type, "command", label="Line",command=function() line())
  tkadd(plot.type, "command", label="Histogram",command=function() histogram())
  tkadd(Menu, "cascade", label="Type of plot ",menu=plot.type)
  copy.but <- tkbutton(tt,text="Copy to pdf",command=CopyToClip)
  ok.but <- tkbutton(tt,text="Ok",command=onok)
  SliderValue1 <- tclVar(100)
  SliderValueLabel1 <- tklabel(tt,text=as.character(tclvalue(SliderValue1)),background="white")
  tkconfigure(SliderValueLabel1,textvariable=SliderValue1)
  f.intensity<-function()
  {
  intensity<<-as.numeric(tclvalue(SliderValue1))
tkrreplot(img,plotFunction())
  }
  slider1 <- tkscale(tt, from=25, to=2000,showvalue=F, variable=SliderValue1,
             resolution=25, orient="horizontal",bigincrement=25)
  superimpose<-function()
  {
espectro$b<<-2
change.espectrum()
  }
  superimpose.b <- tkbutton(tt,text="Superimpose",command=superimpose)

  tkbind(slider1,"<ButtonRelease-1>" ,f.intensity)
  tkpack(img,side="top")
 #tkpack(superimpose.b,side="left",padx=5)
  tkpack(tklabel(tt,text="Intensity : ",background="white"),SliderValueLabel1,slider1)
  tkpack(ok.but,copy.but,padx=10,side="right",pady=10,ipadx=0)
  tkfocus(tt)
  tkraise(tt)
  tkwait.window(tt)
  tkconfigure(console,cursor="arrow")
  return(C.Shift)
}

