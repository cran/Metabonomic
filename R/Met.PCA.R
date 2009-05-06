Met.PCA <-
function(datos){

  tkconfigure(console,cursor="watch")

######################################Data Load 
  info=datos$info
  datos<-datos$datos

  dimnames(datos)[[1]]=as.character(datos[,1])
  datos=datos[,-1] #eliminamos la primera columna
  colnames(datos)=info$nombres
  datos<-t(datos)
######################################Selection of Parameters
  valor<-((Met.Checkbox1("PCA","Scale","Center","Scale = True","Scale = False","Center = True","Center = False")))
  escalado<-tclvalue(valor[1])
  if (escalado == "1") 
  {
escalado=TRUE
  }
  else 
escalado=FALSE
  centro<-tclvalue(valor[2])
  if (centro == "1") 
  centro=TRUE
  else 
centro=FALSE

######################################PCA
  Try(prcomp(datos,scale=escalado,
center=centro,tol = sqrt(.Machine$double.eps)))
  dat.pc<-prcomp(datos,scale=escalado,
center=centro,tol = sqrt(.Machine$double.eps))
  varianza<-summary(dat.pc,loadings=TRUE,digits=2)
  Principales<-predict(dat.pc)
  varianza<-round(varianza$importance[2,],4)*100 
  ncomp<-length(varianza)
  
######################################2D PLOT
  f2<-function(){
  etiqueta=info[,1]
  color="white"
  color2="black"
  col=list(axis="black",lab="black",main="black",sub="black")
  main<-" "
 subt<-" "  
  size=list(cex=0.7,cex.axis=1,cex.lab=1,cex.main=1,cex.sub=1)
  xlab<-paste("PC1 (",varianza[1],"%)")
  ylab<-paste("PC2 (",varianza[2],"%)")
  PLSC=list(a=1,b=2)
tt <- tktoplevel()
plotFunction <- function(color,color2,etiqueta,xlab,ylab,main,subt)
{
  par(bg=color)
plot(Principales[,PLSC$a],Principales[,PLSC$b],
ylim=range(Principales[,PLSC$b]),xlim=range(Principales[,PLSC$a]),
xlab=xlab,ylab=ylab,main=main,sub=subt,type="n",cex.axis=size$cex.axis,
cex.lab=size$cex.lab,cex.main=size$cex.main,cex.sub=size$cex.sub,
col.axis=col$axis,col.lab=col$lab,col.main=col$main,col.sub=col$sub)
text(Principales[,PLSC$a],Principales[,PLSC$b],
labels=abbreviate(etiqueta),cex=size$cex,col=color2)
}
img <- tkrplot(tt,plotFunction(color,color2,etiqueta,xlab,ylab,main,subt),hscale=1.5,vscale=1.5)

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
    etiqueta<<-info[,1]
tkrreplot(img,plotFunction(color,color2,etiqueta,xlab,ylab,main,subt))
  }

by.type<-function(info)
{
   etiqueta<<-info[,categoria] 
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
#windows()
  }
onok <- function()
{
tkdestroy(tt)}
Comp <- function()
{
dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,"PCA Components")
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
  xlab<-paste("PC",PLSC$a,"(",varianza[PLSC$a],"%)")
  ylab<-paste("PC",PLSC$b,"(",varianza[PLSC$b],"%)")
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
tkadd(Menu, "command", label="Components",
      command=function() Comp())

copy.but <- tkbutton(tt,text="Copy to pdf",command=CopyToClip)
ok.but <- tkbutton(tt,text="Ok",command=onok)
tkpack(img,side="top")
tkpack(ok.but,side="right",padx="120")
tkpack(copy.but,side="left",padx="120")
tkfocus(tt)
tkraise(tt)
#tkwait.window(tt)
}
  f2()

########################################################Loadings

  f2<-function(){
  main<-"Loading"
  xlab<-paste("PC1 (",varianza[1],"%)")
  ylab<-paste("PC2 (",varianza[2],"%)")
  PLSC=list(a=1,b=2) 
  etiqueta=info[,1]
  color="white"
  color2="black"
  col=list(axis="black",lab="black",main="black",sub="black")
  subt=""
  size=list(cex=0.7,cex.axis=1,cex.lab=1,cex.main=1,cex.sub=1)
tt <- tktoplevel()
plotFunction <- function(color,color2,etiqueta,xlab,ylab,main,subt)
{
par(bg=color)
plot(dat.pc$rotation[,PLSC$a],dat.pc$rotation[,PLSC$b],
ylim=range(dat.pc$rotation[,PLSC$b]),xlim=range(dat.pc$rotation[,PLSC$a]),
xlab=xlab,ylab=ylab,main=main,sub=subt,type="n",cex.axis=size$cex.axis,
cex.lab=size$cex.lab,cex.main=size$cex.main,cex.sub=size$cex.sub,
col.axis=col$axis,col.lab=col$lab,col.main=col$main,col.sub=col$sub)
text(dat.pc$rotation[,PLSC$a],dat.pc$rotation[,PLSC$b],
labels=abbreviate(row.names(dat.pc$rotation)),cex=size$cex,col=color2)}

img <- tkrplot(tt,plotFunction(color,color2,etiqueta,xlab,ylab,main,subt),hscale=1.5,vscale=1.5)

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
#windows()
}
onok <- function()
{
tkdestroy(tt)
}
Comp <- function()
{
dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,"PCA Components")
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
  xlab<-paste("PC",PLSC$a,"(",varianza[PLSC$a],"%)")
  ylab<-paste("PC",PLSC$b,"(",varianza[PLSC$b],"%)")
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
#tkwait.window(tt)

  }
  f2()
  tkconfigure(console,cursor="arrow")
}

