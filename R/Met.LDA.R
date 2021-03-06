Met.LDA <-
function(datos,externa){
  tkconfigure(console,cursor="watch")

  Met.model<- function(title)
{
dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,title)
SliderValue<-list()
SliderValueLabel<-list()
slider<-list()
tkgrid(tklabel(dlg,text="       "))
tkgrid(tklabel(dlg,text="Building the model"),sticky="w")
tkgrid(tklabel(dlg,text="       "))
for (i in 1:length(clase))
{
SliderValue[[i]] <- tclVar(round(v[i]/2))
SliderValueLabel[[i]] <- tklabel(dlg,text=as.character(tclvalue(SliderValue[[i]])))
tkconfigure(SliderValueLabel[[i]],textvariable=SliderValue[[i]])
slider[[i]] <- tkscale(dlg, from=(v[i]-1), to=2,showvalue=F, variable=SliderValue[[i]],
                   resolution=1, orient="horizontal")
tkgrid(tklabel(dlg,text=paste("Number of samples of the",clase[[i]],
"group : ")),SliderValueLabel[[i]],slider[[i]])
}
tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="       "))
  
onOK <- function()
  {
d4o<-c()
for (i in 1:length(clase))
d4o[i]<-tclvalue(SliderValue[[i]])
    elementos <<- as.numeric(d4o)

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
  tkwait.window(dlg)
  }
  ################### Gráfica 2D

   f2<-function(a,b,Validation,main="",xlab="Index",ylab="LDA1",etiqueta=list(a=info[samp,1],b=info[-samp,1]),
color="white",color2="black",color3="blue",col=list(axis="black",lab="black",main="black",sub="black"),
subt="",size=list(cex=0.7,cex.axis=1,cex.lab=1,cex.main=1,cex.sub=1),PLSC=list(a=1,b=2))
  {
tt <- tktoplevel()
if (dim(a)[2]>1)
{ 
xlab="LDA1"
ylab="LDA2"
}
plotFunction <<- function(color,color2,etiqueta,xlab,ylab,main,subt)
{
if (dim(a)[2]>1)
{
par(bg=color)
plot(a[,PLSC$a],a[,PLSC$b],xlim=NULL,
ylim=NULL,xlab=xlab,ylab=ylab,main=main,sub=subt,type="n",cex.axis=size$cex.axis,
cex.lab=size$cex.lab,cex.main=size$cex.main,cex.sub=size$cex.sub,
col.axis=col$axis,col.lab=col$lab,col.main=col$main,col.sub=col$sub)
text(a[,PLSC$a],a[,PLSC$b],labels=abbreviate(etiqueta$a),cex=size$cex,col=color2)
if (Validation==TRUE)
text(b[,PLSC$a],b[,PLSC$b],labels=abbreviate(etiqueta$b),cex=size$cex,col=color3)

}
if (dim(a)[2]==1)
{
par(bg=color)
plot(a[,PLSC$a],ylim=range(a[,PLSC$a]),xlab=xlab,ylab=ylab,main=main,sub=subt,type="n",cex.axis=size$cex.axis,
cex.lab=size$cex.lab,cex.main=size$cex.main,cex.sub=size$cex.sub,
col.axis=col$axis,col.lab=col$lab,col.main=col$main,col.sub=col$sub)
text(a[,PLSC$a],labels=abbreviate(etiqueta$a),cex=size$cex,col=color2)
if (Validation==TRUE)
text(b[,PLSC$a],labels=abbreviate(etiqueta$b),cex=size$cex,col=color3)


}
}
img <- tkrplot(tt,plotFunction(color,color2,etiqueta,xlab,ylab,main,subt),hscale=1.5,vscale=1.5)

change.color.bakground<-function()
{
require(tcltk)
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
ChangeColor7 <- function()
    {
     color3 <<- tclvalue(.Tcl(paste("tk_chooseColor",.Tcl.args(initialcolor=color3,title="Choose a color"))))
     if (nchar(color3)>0)
        tkconfigure(canvas7,bg=color3)
     }
 canvas1 <- tkcanvas(tt,width="80",height="25",bg=color)
 canvas2 <- tkcanvas(tt,width="80",height="25",bg=color2)
 canvas3 <- tkcanvas(tt,width="80",height="25",bg=col$axis)
canvas4 <- tkcanvas(tt,width="80",height="25",bg=col$lab)
canvas5 <- tkcanvas(tt,width="80",height="25",bg=col$main)
canvas6 <- tkcanvas(tt,width="80",height="25",bg=col$sub)
canvas7 <- tkcanvas(tt,width="80",height="25",bg=color3)
ChangeColor.button1 <- tkbutton(tt,text="Change Color",command=function() ChangeColor1())
ChangeColor.button2 <- tkbutton(tt,text="Change Color",command=function() ChangeColor2())
ChangeColor.button3 <- tkbutton(tt,text="Change Color",command=function() ChangeColor3())
ChangeColor.button4 <- tkbutton(tt,text="Change Color",command=function() ChangeColor4())
ChangeColor.button5 <- tkbutton(tt,text="Change Color",command=function() ChangeColor5())
ChangeColor.button6 <- tkbutton(tt,text="Change Color",command=function() ChangeColor6())
ChangeColor.button7 <- tkbutton(tt,text="Change Color",command=function() ChangeColor7())
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
tkgrid(tklabel(tt,text="Label1    "),canvas2,ChangeColor.button2)
   tkgrid(tklabel(tt,text="    "))
tkgrid(tklabel(tt,text="Label2    "),canvas7,ChangeColor.button7)
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


by.name<-function(info)
{
etiqueta<<-list(a=info[samp,1],b=info[-samp,1])
tkrreplot(img,plotFunction(color,color2,etiqueta,xlab,ylab,main,subt))
  }

by.type<-function(info)
{
   etiqueta<<-list(a=info[samp,categoria],b=info[-samp,categoria]) 
tkrreplot(img,plotFunction(color,color2,etiqueta,xlab,ylab,main,subt))
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
}
Comp <- function()
{
ncomp<-dim(a)[2]
dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,"LDA Components")
SliderValue1 <- tclVar(PLSC$a)
SliderValue2 <- tclVar(PLSC$b)
SliderValueLabel1 <- tklabel(dlg,text=as.character(tclvalue(SliderValue1)))
SliderValueLabel2 <- tklabel(dlg,text=as.character(tclvalue(SliderValue2)))
tkconfigure(SliderValueLabel1,textvariable=SliderValue1)
tkconfigure(SliderValueLabel2,textvariable=SliderValue2)
slider1 <- tkscale(dlg, from=1, to=ncomp,showvalue=F, variable=SliderValue1,
                   resolution=1, orient="horizontal")
slider2 <- tkscale(dlg, from=1, to=ncomp,showvalue=F, variable=SliderValue2,
                   resolution=1, orient="horizontal")
tkgrid(tklabel(dlg,text="       "))
tkgrid(tklabel(dlg,text="X axis : "),SliderValueLabel1,slider1)
tkgrid(tklabel(dlg,text="       "))
tkgrid(tklabel(dlg,text="Y axis : "),SliderValueLabel2,slider2)
tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="       "))
  
onOK <- function()
  {
PLSC<<-list(a=as.numeric(tclvalue(SliderValue1)),b=as.numeric(tclvalue(SliderValue2))) 
  xlab<-paste("LDA",PLSC$a)
  ylab<-paste("LDA",PLSC$b)
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

  }

onok <- function(){tkdestroy(tt)}

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
tkadd(Menu, "command", label="Components",
      command=function() Comp())
copy.but <- tkbutton(tt,text="Copy to pdf",command=CopyToClip)
ok.but <- tkbutton(tt,text="Ok",command=onok)
tkpack(img,side="top")
tkpack(ok.but,side="right",padx="120")
tkpack(copy.but,side="left",padx="120")
tkfocus(tt)
tkraise(tt)
tkwait.window(tt)
   }
   ##########################################

  
   ######################################################  3D PLOT 


   f3<-function(a,b,Validation,main="",xlab="LDA1",ylab="LDA2",zlab="LDA3",etiqueta=list(a=info[samp,1],b=info[-samp,1]),
color="white",color2="black",color3="blue",col=list(axis="black",lab="black",main="black",sub="black"),
subt="",size=list(cex=0.7,cex.axis=1,cex.lab=1,cex.main=1,cex.sub=1),  angle=60, PLSC=list(a=1,b=2,d=3))
   {
require(scatterplot3d)
tt <- tktoplevel()
plotFunction <<- function(color,color2,etiqueta,xlab,ylab,main,subt)
{
  par(bg=color)
s3d <- scatterplot3d(a[,PLSC$a],a[,PLSC$b],a[,PLSC$d],type="p",
color=color2, pch=10,box=TRUE,cex.axis=size$cex.axis,cex.lab=size$cex.lab,
cex.main=size$cex.main,cex.sub=size$cex.sub,cex=size$cex,
    main=main, sub=subt, xlim=NULL, ylim=NULL, zlim=NULL,
col.axis=col$axis,col.lab=col$lab,col.main=col$main,col.sub=col$sub,
    xlab=xlab, ylab=ylab, zlab=zlab, scale.y=1, angle=angle,
    axis=TRUE, tick.marks=TRUE, label.tick.marks=TRUE, grid=TRUE)

text(s3d$xyz.convert(a[,PLSC$a],a[,PLSC$b],a[,PLSC$d]), labels=etiqueta$a,
cex=size$cex,lwd=2,col=color2, pos=1)
if (Validation==TRUE)
{
points(s3d$xyz.convert(b[,PLSC$a],b[,PLSC$b],b[,PLSC$d]),cex=size$cex,lwd=2,pos=3,col=color3)
text(s3d$xyz.convert(b[,PLSC$a],b[,PLSC$b],b[,PLSC$d]),labels=etiqueta$b,cex=size$cex,lwd=2,pos=3,col=color3)
}
}
img <- tkrplot(tt,plotFunction(color,color2,etiqueta,xlab,ylab,main,subt),hscale=1.5,vscale=1.5)

change.color.bakground<-function(){
  require(tcltk)
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
ChangeColor7 <- function()
    {
     color3 <<- tclvalue(.Tcl(paste("tk_chooseColor",.Tcl.args(initialcolor=color3,title="Choose a color"))))
     if (nchar(color3)>0)
        tkconfigure(canvas7,bg=color3)
     }
  canvas1 <- tkcanvas(tt,width="80",height="25",bg=color)
    canvas2 <- tkcanvas(tt,width="80",height="25",bg=color2)
  canvas3 <- tkcanvas(tt,width="80",height="25",bg=col$axis)
canvas4 <- tkcanvas(tt,width="80",height="25",bg=col$lab)
canvas5 <- tkcanvas(tt,width="80",height="25",bg=col$main)
canvas6 <- tkcanvas(tt,width="80",height="25",bg=col$sub)
canvas7 <- tkcanvas(tt,width="80",height="25",bg=color3)
    ChangeColor.button1 <- tkbutton(tt,text="Change Color",command=function() ChangeColor1())
    ChangeColor.button2 <- tkbutton(tt,text="Change Color",command=function() ChangeColor2())
ChangeColor.button3 <- tkbutton(tt,text="Change Color",command=function() ChangeColor3())
ChangeColor.button4 <- tkbutton(tt,text="Change Color",command=function() ChangeColor4())
ChangeColor.button5 <- tkbutton(tt,text="Change Color",command=function() ChangeColor5())
ChangeColor.button6 <- tkbutton(tt,text="Change Color",command=function() ChangeColor6())
ChangeColor.button7 <- tkbutton(tt,text="Change Color",command=function() ChangeColor7())
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
tkgrid(tklabel(tt,text="Label2    "),canvas7,ChangeColor.button7)
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

by.name<-function(info)
{
    etiqueta<<-list(a=info[samp,1],b=info[-samp,1])
tkrreplot(img,plotFunction(color,color2,etiqueta,xlab,ylab,main,subt))
}

by.type<-function(info)
{
   etiqueta<<-list(a=info[samp,categoria],b=info[-samp,categoria]) 
tkrreplot(img,plotFunction(color,color2,etiqueta,xlab,ylab,main,subt))
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
  }

onok <- function()
{
tkdestroy(tt)
}

Angle <- function()
{
dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,"Angle")
SliderValue1 <- tclVar(angle)
SliderValueLabel1 <- tklabel(dlg,text=as.character(tclvalue(SliderValue1)))
tkconfigure(SliderValueLabel1,textvariable=SliderValue1)
slider1 <- tkscale(dlg, from=0, to=180,showvalue=F, variable=SliderValue1,
                   resolution=1, orient="horizontal")
tkgrid(tklabel(dlg,text="       "))
tkgrid(tklabel(dlg,text="Angle : "),SliderValueLabel1,slider1)
tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="       "))
  
onOK <- function()
  {
angle<<-as.numeric(tclvalue(SliderValue1))
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
}

Comp <- function()
{
dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,"PLS Components")
SliderValue1 <- tclVar(PLSC$a)
SliderValue2 <- tclVar(PLSC$b)
SliderValue3 <- tclVar(PLSC$d)
SliderValueLabel1 <- tklabel(dlg,text=as.character(tclvalue(SliderValue1)))
SliderValueLabel2 <- tklabel(dlg,text=as.character(tclvalue(SliderValue2)))
SliderValueLabel3 <- tklabel(dlg,text=as.character(tclvalue(SliderValue3)))
tkconfigure(SliderValueLabel1,textvariable=SliderValue1)
tkconfigure(SliderValueLabel2,textvariable=SliderValue2)
tkconfigure(SliderValueLabel3,textvariable=SliderValue3)
slider1 <- tkscale(dlg, from=1, to=ncomp,showvalue=F, variable=SliderValue1,
                  resolution=1, orient="horizontal")
slider2 <- tkscale(dlg, from=1, to=ncomp,showvalue=F, variable=SliderValue2,
                  resolution=1, orient="horizontal")
slider3 <- tkscale(dlg, from=1, to=ncomp,showvalue=F, variable=SliderValue3,
                  resolution=1, orient="horizontal")
tkgrid(tklabel(dlg,text="       "))
tkgrid(tklabel(dlg,text="X axis : "),SliderValueLabel1,slider1)
tkgrid(tklabel(dlg,text="       "))
tkgrid(tklabel(dlg,text="Y axis : "),SliderValueLabel2,slider2)
tkgrid(tklabel(dlg,text="       "))
tkgrid(tklabel(dlg,text="Z axis : "),SliderValueLabel3,slider3)
tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="       "))
  onOK <- function()
  {
PLSC<<-list(a=as.numeric(tclvalue(SliderValue1)),b=as.numeric(tclvalue(SliderValue2)),
 d=as.numeric(tclvalue(SliderValue3))) 
  xlab<<-paste("LDA  ",PLSC$a)
  ylab<<-paste("LDA  ",PLSC$b)
zlab<<-paste("LDA  ",PLSC$d)

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
tkadd(Menu, "command", label="Angle",
      command=function() Angle())
tkadd(Menu, "command", label="Components",
      command=function() Comp())
copy.but <- tkbutton(tt,text="Copy to pdf",command=CopyToClip)
ok.but <- tkbutton(tt,text="Ok",command=onok)
tkpack(img,side="top")
tkpack(ok.but,side="right",padx="120")
tkpack(copy.but,side="left",padx="120")
tkfocus(tt)
tkraise(tt)
tkwait.window(tt)
  }
  ##########################

  info=datos$info
  datos=datos$datos
  attach(info)


  dimnames(datos)[[1]]=as.character(datos[,1])
  datos=datos[,-1] #eliminamos la primera columna
  colnames(datos)=info$Nombre
  datos<-t(datos)
  v4<-dim(info)[1]
  Selection<-list()
  v<-c()  
  for (i in 1: length(clase))
  {
Selection[[i]]<-which(info[,categoria]==clase[[i]])
  v[i]<-length(Selection[[i]])
  }
  Met.model.1("LDA")
  if (m.model=="random")  #Random Selection
  {
Met.model("LDA")
samp.list<-list()
for (i in 1:length(v))
samp.list[[i]]<-sample(Selection[[i]],elementos[i])

d4m<-c()
  lista1<-c()
  d4m[1]<-length(samp.list[[1]])
  samp<-samp.list[[1]]

  for (i in 2:length(samp.list))
  {
d4m[i]<-length(samp.list[[i]])
  samp[(length(samp)+1):(length(samp)+d4m[i])]<-samp.list[[i]]
  }
  }

  if (m.model=="manual")  #Manual Selection
  {
manual.model("LDA")
  }
  valor<-((Met.RadioBox1("LDA","MLEs","Moment","t distribution","mle","moment","t")))
  metodo<-tclvalue(valor)
  if (metodo=="t")
  {
 grados<- Met.modalDialog("LDA","Choose degrees of freedom in the t-distribution.   (>2)","")
  }
  Try(dfB.ordtotal.lda<-lda((datos),(grouping=info[,categoria]),tol = 1.0e-6,
  method=metodo,nu=grados, CV = FALSE, subset=samp))
  scores.total<-predict(dfB.ordtotal.lda)$x
  group.total<-predict(dfB.ordtotal.lda)$class 
  f2(a=scores.total, Validation=FALSE)
  if (dim(scores.total)[2]>=3)
  f3(a=scores.total, Validation=FALSE)

  ######  ValidatiOn
  Validacion.Cruzada<-group.total
  print.table(table(info[samp,categoria],Validacion.Cruzada),justify = "centre")
  TABLE(info[samp,categoria],Validacion.Cruzada,title="Cross validation results.")
  info.validacion=info[-samp,]
  datos.validacion=datos[-samp,]
  nuevos.datos<- predict(dfB.ordtotal.lda,(datos.validacion))$x
  Datos.Validacion<- predict(dfB.ordtotal.lda,datos.validacion)$class

  
  f2(a=scores.total, b=nuevos.datos, Validation=TRUE)
  if (dim(scores.total)[2]>=3)
  f3(a=scores.total, b=nuevos.datos, Validation=TRUE)
  print(table(info.validacion[,categoria],Datos.Validacion))
  TABLE(info.validacion[,categoria],Datos.Validacion,title="Internal validation results.")
  tkconfigure(console,cursor="arrow")

  }

