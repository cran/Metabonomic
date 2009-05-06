Baseline.Correction <-
function()
{
  #tkconfigure(console,cursor="watch")
  Require("FTICRMS")

  sp<-datos$datos# Data Imput
  SP2<-as.data.frame(sp[,1])# Data Output
  i=2

###### Graphic Parameters

 require(tcltk)
  require(tkrplot)
  color="white"
  color2="black"
  col=list(axis="black",lab="black",main="black",sub="black")
  subt=""
  size=list(cex=0.7,cex.axis=1,cex.lab=1,cex.main=1,cex.sub=1)
  xlab="Chemical Shift (ppm)"
  ylab=""
  xlim=c(10,0)
  tt <- tktoplevel(background="white")
  tkwm.title(tt,"Baseline Correction")
 
  a2<-sp[,i]# Spectrum to correct

###### Baseline Parameters

sm<-10^-10
frac<-0.001
max.it<-3000
binsiz<-1
neg.p<-6

###### Plot Function

  plotFunction <- function()
  {
   main=datos$info[i-1,1]
  params <- par(bg="white")
a2<-sp[,i]

############ Baseline function
baseline(a2, sm.par = sm, neg.pen =neg.p, max.iter = max.it,
 frac.changed = frac, k.biweight = binsiz,xvals =sp[,1])
a3<<-baseline(a2, sm.par = sm, neg.pen =neg.p, max.iter = max.it,
 frac.changed = frac, k.biweight = binsiz,xvals =sp[,1])

  plot(sp[,1],sp[,i],type="l",xlab=xlab,ylab=ylab,xlim=xlim,main=main,
sub=subt,cex.axis=size$cex.axis,cex.lab=size$cex.lab,xaxp=c(xlim[1],xlim[2],20),
cex.main=size$cex.main,cex.sub=size$cex.sub,col.axis=col$axis,
col.lab=col$lab,col.main=col$main,col.sub=col$sub,cex=size$cex,
col=color2)
   
lines(sp[,1],a3$baseline,type="l",col="red")

  }

  img <- tkrplot(tt,fun=plotFunction,hscale=1.5,vscale=1.5)
  tkgrid(img)

###### Change color function 

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

###### Legend Function
 
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

###### Zoom Function

  Zoom<-function()
  {
dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,"Zoom")
SliderValue1 <- tclVar("0")
SliderValue2 <- tclVar("10")
SliderValueLabel1 <- tklabel(dlg,text=as.character(tclvalue(SliderValue1)))
SliderValueLabel2 <- tklabel(dlg,text=as.character(tclvalue(SliderValue2)))
tkconfigure(SliderValueLabel1,textvariable=SliderValue1)
tkconfigure(SliderValueLabel2,textvariable=SliderValue2)
slider1 <- tkscale(dlg, from=10, to=0,showvalue=F, variable=SliderValue1,
                   resolution=0.01, orient="horizontal",length=300)
slider2 <- tkscale(dlg, from=10, to=0,showvalue=F, variable=SliderValue2,
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

###### Text Size function

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

###### Save as PDF

    CopyToClip <- function()
  {
fileName<-tclvalue(tkgetSaveFile())
pdf(file = fileName)
plotFunction()
dev.off()
windows()
plotFunction()
}

Refresh<-function()
{
tkrreplot(img,plotFunction())
}


###### OK Button function
 
onok <- function()
{
SP2[,i]<-sp[,i]-a3$baseline
SP2<<-SP2
i<<-i+1

if(i<(dim(sp)[2]+1))
tkrreplot(img,plotFunction())  
if(i>=(dim(sp)[2]+1))
{
datos$datos<<-SP2
 memory<<-memory+1
 memory.data[[memory]]<<-list(generation=memory,datos=datos$datos,
  info=datos$info)     #For undo function

tkdestroy(tt)
}
}
###### Baseline extra parameters

 extra.parameters <- function(title="Parameters",q1="Robust center and scale estimation ",q2="Max. iterations",
q3="Negativity Penalty",q4="Frac. Changed", e1=binsiz,e2=max.it,e3=neg.p, e4=frac,entryWidth=20,returnValOnCancel="ID_CANCEL")
  {
  dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,title)
  textVarTcl <- tclVar(paste(e1))
  textVarTcl2 <- tclVar(paste(e2))
  textVarTcl3 <- tclVar(paste(e3))
  textVarTcl4 <- tclVar(paste(e4))

  textWidget <- tkentry(dlg,width=paste(entryWidth),textvariable=textVarTcl)
  textWidget2 <- tkentry(dlg,width=paste(entryWidth),textvariable=textVarTcl2)
  textWidget3 <- tkentry(dlg,width=paste(entryWidth),textvariable=textVarTcl3)
  textWidget4 <- tkentry(dlg,width=paste(entryWidth),textvariable=textVarTcl4)

  tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="Change the Legend"),sticky="w")
   tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text=q1),textWidget,sticky="e")
  tkgrid(tklabel(dlg,text=q2),textWidget2,sticky="e")
  tkgrid(tklabel(dlg,text=q3),textWidget3,sticky="e")
  tkgrid(tklabel(dlg,text=q4),textWidget4,sticky="e")
  tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="       "))
  
ReturnVal <- returnValOnCancel
  onOK <- function()
  {
    binsiz<<-as.numeric(tclvalue(textVarTcl))
    max.it<<-as.numeric(tclvalue(textVarTcl2))
    neg.p<<-as.numeric(tclvalue(textVarTcl3))
frac<<-as.numeric(tclvalue(textVarTcl4))
    
tkrreplot(img,plotFunction()) 
tkdestroy(dlg)
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
  tkbind(textWidget, "<Return>", onOK)
  tkwait.window(dlg)
  return(ReturnVal)
  }

###### Main Window

  tkwm.title(tt,"Interactive Plot")
  Menu <- tkmenu(tt,borderwidth=40)
  tkconfigure(tt, menu=Menu)
  tkadd(Menu, "command", label="Background color",
        command=change.color.bakground)
  legend <- tkmenu(Menu,borderwidth=40,tearoff=FALSE)
  tkadd(legend, "command", label="Text",
        command=function() modalDialog4("Legend   ","Xlab   ","Ylab   ","Main Title   ","Subtitle", xlab,ylab,main,subt))
  tkadd(legend, "command", label="Size",
        command=function() Text.size())
  tkadd(Menu, "cascade", label="Legend",menu=legend)
  tkadd(Menu, "command", label="Zoom",
        command=function() Zoom())
  tkadd(Menu, "command", label="Extra Parameters",
        command=function() extra.parameters())
  copy.but <- tkbutton(tt,text="Copy to pdf",command=CopyToClip)
  ok.but <- tkbutton(tt,text="Ok",command=onok)
  refresh.but <- tkbutton(tt,text="Refresh",command=Refresh)
  SliderValue1 <- tclVar(20)
  SliderValueLabel1 <- tklabel(tt,text=as.character(tclvalue(SliderValue1)),background="white")
  tkconfigure(SliderValueLabel1,textvariable=SliderValue1)

###### Smooth function

  f.intensity<-function()
  {
  sm<<-10^(-0.5*(as.numeric(tclvalue(SliderValue1))))
tkrreplot(img,plotFunction())
  }
  slider1 <- tkscale(tt, from=1, to=30,showvalue=F, variable=SliderValue1,
             resolution=1, orient="horizontal",bigincrement=25)
 
  tkbind(slider1,"<ButtonRelease-1>" ,f.intensity)
  tkpack(img,side="top")

  tkpack(tklabel(tt,text="Smoothing Factor : ",background="white"),SliderValueLabel1,slider1,
tklabel(tt,text="           ",background="white"),
  ok.but,refresh.but,copy.but,padx=10,side="left",pady=10,ipadx=0)

  tkfocus(tt)
  tkraise(tt)
#  tkconfigure(console,cursor="arrow")
 tkwait.window(tt)

}

