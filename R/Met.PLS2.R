Met.PLS2 <-
function(datos,externa)
{
  tkconfigure(console,cursor="watch")
  
  Require("splines")
  Require("pls")
  Require("scatterplot3d")
  Met.check.radio <- function(entryWidth=20,returnValOnCancel="ID_CANCEL")
  {
  tt <- tktoplevel()
  fontHeading <- tkfont.create(family="times",size=12,weight="bold")
  fontTextLabel <- tkfont.create(family="times",size=12)
  tkwm.deiconify(tt)
  tkgrab.set(tt)
  tkfocus(tt)
  tkwm.title(tt,"PLS")
 tkgrid(tklabel(tt,text="    "))
  tkgrid(tklabel(tt,text="      Selection of the PLS parameters:",font=fontHeading))
 tkgrid(tklabel(tt,text="    "))
 cb <- tkcheckbutton(tt)
 cbValue <-tclVar(1)
 tkconfigure(cb,variable=cbValue)
 tkgrid(tklabel(tt,text="Scale"),cb,sticky="w")
 tkgrid(tklabel(tt,text="    "))
 rb1 <- tkradiobutton(tt)
 rb2 <- tkradiobutton(tt)
 rb3 <- tkradiobutton(tt)
 rb4 <- tkradiobutton(tt)
 rbValue<-tclVar("kernelpls")
 tkconfigure(rb1,variable=rbValue,value="kernelpls")
 tkconfigure(rb2,variable=rbValue,value="widekernwlpls")
 tkconfigure(rb3,variable=rbValue,value="simpls")
 tkconfigure(rb4,variable=rbValue,value="oscorespls")
 tkgrid(tklabel(tt,text="Which method would you like to use?"),sticky="w")
 tkgrid(tklabel(tt,text="Kernel algorithm "),rb1,sticky="e")
 tkgrid(tklabel(tt,text="Wide Kernel algorithm "),rb2,sticky="e")
 tkgrid(tklabel(tt,text="SIMPLS"),rb3,sticky="e")
 tkgrid(tklabel(tt,text="Classical orthogonal scores algorithm"),rb4,sticky="e")
 
 rb5 <- tkradiobutton(tt)
 rb6 <- tkradiobutton(tt)
 rb7 <- tkradiobutton(tt)
 rbValue2<-tclVar("CV")
 tkconfigure(rb5,variable=rbValue2,value="none")
 tkconfigure(rb6,variable=rbValue2,value="CV")
 tkconfigure(rb7,variable=rbValue2,value="LOO")
 tkgrid(tklabel(tt,text="    "))
 tkgrid(tklabel(tt,text="Validation method"),sticky="w")
 tkgrid(tklabel(tt,text="None"),rb5,sticky="e")
 tkgrid(tklabel(tt,text="Cross-validation"),rb6,sticky="e")
 tkgrid(tklabel(tt,text="Leave-one-out cross-validation "),rb7,sticky="e")
  
 
 onOK <- function()
 {
 ReturnVal <<- list(a=as.numeric(tclvalue(cbValue)),b=as.character(tclvalue(rbValue)),d=as.character(tclvalue(rbValue2)))
 tkgrab.release(tt)
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
    tkgrid(tklabel(tt,text="    "))
  tkgrid(tklabel(tt,text="    "),OK.but,Cancel.but,tklabel(tt,text="    "),sticky="w")
  tkgrid(tklabel(tt,text="    "))
  tkfocus(tt)
  tkraise(tt)  
 
  tkwait.window(tt)
return(ReturnVal)

  }


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
  
  f1<-function(a,ylab,main=" ",xlab="Number of Components",model=1,
color="white",color2="black",col=list(axis="black",lab="black",main="black",sub="black"),
subt=" ",size=list(cex=0.7,cex.axis=1,cex.lab=1,cex.main=1,cex.sub=1))
  {
tt <- tktoplevel()
tkwm.title(tt,"PLS Model")
l1<-ylab
m1<-main
plotFunction <- function()
{
  par(bg=color)
  plot(a[[model]], xlab=xlab, ylab=ylab[model],
xaxp=c(0,100,100),pch=19,main=main[model],sub=subt,
cex.axis=size$cex.axis,cex.lab=size$cex.lab,cex.main=size$cex.main,
cex.sub=size$cex.sub,col.axis=col$axis,col.lab=col$lab,
col.main=col$main,col.sub=col$sub,col=color2)
  }

img <- tkrplot(tt,plotFunction(),hscale=1.5,vscale=1.5)

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
tkdestroy(tt)
  }
Met.radio_seleccion1 <- function(etiquetas,s,entryWidth=7,returnValOnCancel="ID_CANCEL")
 {
entryWidth=7
  tt <- tktoplevel()
  fontHeading <- tkfont.create(family="times",size=12,weight="bold")
  fontTextLabel <- tkfont.create(family="times",size=12)
  tkwm.deiconify(tt)
  tkgrab.set(tt)
  tkfocus(tt)
  tkwm.title(tt,"PLS")
  tkgrid(tklabel(tt,text="    "))
  tkgrid(tklabel(tt,text="Model Information",font=fontHeading),sticky="e")
  tkgrid(tklabel(tt,text="    "))
rbuton<-list()
for (i in 1:length(l1))
rbuton[[i]]<-tkradiobutton(tt)

  rbValue<-tclVar(model)
for (i in 1:length(l1))
  tkconfigure(rbuton[[i]],variable=rbValue,value=i)
  
for (i in 1:length(l1))
tkgrid(tklabel(tt,text=paste(l1[i]," (",m1[i]," )")),rbuton[[i]],sticky="e")

   tkgrid(tklabel(tt,text="       "))
    onOK <- function()
  {
    model <<- as.numeric((tclvalue(rbValue)))
    tkgrab.release(tt)
    tkdestroy(tt)
tkrreplot(img,plotFunction())  

     }
  onCancel <- function()
  {
    categoria <<- 0
    tkdestroy(tt)
   }
  OK.but     <-tkbutton(tt,text="   OK   ",command=onOK)
  Cancel.but <-tkbutton(tt,text=" Cancel ",command=onCancel)
    tkgrid(tklabel(tt,text="    "))
  tkgrid(tklabel(tt,text="    "),OK.but,Cancel.but,tklabel(tt,text="    "),sticky="w")
  tkgrid(tklabel(tt,text="    "))
  tkfocus(tt)
  tkraise(tt)
  tkwait.window(tt)
  }

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
tkadd(Menu, "command", label="Model Information",
      command=function() Met.radio_seleccion1())

copy.but <- tkbutton(tt,text="Copy to pdf",command=CopyToClip)
ok.but <- tkbutton(tt,text="Ok",command=onok)
tkpack(img,side="top")
tkpack(ok.but,side="right",padx="120")
tkpack(copy.but,side="left",padx="120")

tkfocus(tt)
tkraise(tt)
  }
  ################### 2D PLOT
   f2<-function(a,b,Validation,main="",xlab=paste("PLS 1 : ",round(explvar(analisis.pls)[1],2),"%"),
ylab=paste("PLS 2:",round(explvar(analisis.pls)[2],2),"%"),etiqueta=list(a=info[samp,1],b=info[-samp,1]),
color="white",color2="black",color3="blue",col=list(axis="black",lab="black",main="black",sub="black"),
subt="",size=list(cex=0.7,cex.axis=1,cex.lab=1,cex.main=1,cex.sub=1),PLSC=list(a=1,b=2))
  {
tt <- tktoplevel()

plotFunction <<- function()
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

img <- tkrplot(tt,plotFunction(),hscale=1.5,vscale=1.5)

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
tkrreplot(img,plotFunction())
  }

by.type<-function(info)
{
   etiqueta<<-list(a=info[samp,categoria],b=info[-samp,categoria]) 
tkrreplot(img,plotFunction())
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


CopyToClip <- function()
{
fileName<-tclvalue(tkgetSaveFile())
pdf(file = fileName)
plotFunction()
dev.off()
windows()
plotFunction()
}
Comp <- function()
{
ncomp<-dim(a)[2]
dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,"PLS Components")
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
  xlab<<-paste("PLS",PLSC$a,round(explvar(analisis.pls)[PLSC$a],2),"%")
  ylab<<-paste("PLS",PLSC$b,round(explvar(analisis.pls)[PLSC$b],2),"%")

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

  ################
   f2b<-function(a,type,main="",xlab=paste("PLS 1 : ",round(explvar(analisis.pls)[1],2),"%"),
ylab=paste("PLS 2:",round(explvar(analisis.pls)[2],2),"%"),etiqueta=list(a=info[samp,1],b=info[-samp,1]),
color="white",color2="black",color3="blue",col=list(axis="black",lab="black",main="black",sub="black"),
subt="",size=list(cex=0.7,cex.axis=1,cex.lab=1,cex.main=1,cex.sub=1),PLSC=list(a=1,b=2))
  {
tt <- tktoplevel()
plsplottype <- c("biplot","coefficients","loadings","correlation")
plotFunction <<- function()
{
par(bg=color)

                     
plot(a,plottype =plsplottype[type], comps = c(PLSC$a,PLSC$b),
    xlabs=abbreviate(etiqueta[[1]]),cex=size$cex,col=c(color2,color3),var.axes = TRUE,lwd=2,
col.axis=col$axis,col.lab=col$lab,col.main=col$main,col.sub=col$sub,
cex.lab=size$cex.lab,cex.main=size$cex.main,cex.sub=size$cex.sub,
cex.axis=size$cex.axis,main=main,sub=subt)

}

img <- tkrplot(tt,plotFunction(),hscale=1.5,vscale=1.5)

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
tkrreplot(img,plotFunction())
  }

by.type<-function(info)
{
   etiqueta<<-list(a=info[samp,categoria],b=info[-samp,categoria]) 
tkrreplot(img,plotFunction())
}

modalDialog4 <- function(title,question,question2,question3,question4, entryInit,entryInit2,entryInit3,entryInit4, entryWidth=20,returnValOnCancel="ID_CANCEL")
  {
  dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,title)
  #textEntryVarTcl <- tclVar(paste(entryInit))
  #textEntryVarTcl2 <- tclVar(paste(entryInit2))
  textEntryVarTcl3 <- tclVar(paste(entryInit3))
  textEntryVarTcl4 <- tclVar(paste(entryInit4))
  #textEntryWidget <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl)
  #textEntryWidget2 <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl2)
  textEntryWidget3 <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl3)
  textEntryWidget4 <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl4)
tkgrid(tklabel(dlg,text="       "))
tkgrid(tklabel(dlg,text="Change the Legend"),sticky="w")
  tkgrid(tklabel(dlg,text="       "))
  #tkgrid(tklabel(dlg,text=question),textEntryWidget,sticky="e")
  #tkgrid(tklabel(dlg,text=question2),textEntryWidget2,sticky="e")
  tkgrid(tklabel(dlg,text=question3),textEntryWidget3,sticky="e")
  tkgrid(tklabel(dlg,text=question4),textEntryWidget4,sticky="e")
tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="       "))
 
ReturnVal <- returnValOnCancel
  onOK <- function()
{
#xlab<<-as.character(tclvalue(textEntryVarTcl))
#ylab<<-as.character(tclvalue(textEntryVarTcl2))
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


CopyToClip <- function()
{
fileName<-tclvalue(tkgetSaveFile())
pdf(file = fileName)
plotFunction()
dev.off()
windows()
plotFunction()
}
Comp <- function()
{
ncomp<-dim(a$scores)[2]
dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,"PLS Components")
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
  xlab<<-paste("PLS",PLSC$a,round(explvar(analisis.pls)[PLSC$a],2),"%")
  ylab<<-paste("PLS",PLSC$b,round(explvar(analisis.pls)[PLSC$b],2),"%")

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

  }
pls.mode<-function()
 {
entryWidth=7
  tt <- tktoplevel()
  fontHeading <- tkfont.create(family="times",size=12,weight="bold")
  fontTextLabel <- tkfont.create(family="times",size=12)
  tkwm.deiconify(tt)
  tkgrab.set(tt)
  tkfocus(tt)
  tkwm.title(tt,"PLS")
  tkgrid(tklabel(tt,text="    "))
  tkgrid(tklabel(tt,text="Mode",font=fontHeading),sticky="e")
  tkgrid(tklabel(tt,text="    "))
rbuton<-list()
for (i in 1:length(plsplottype))
rbuton[[i]]<-tkradiobutton(tt)

  rbValue<-tclVar(type)
for (i in 1:length(plsplottype))
  tkconfigure(rbuton[[i]],variable=rbValue,value=i)
  
for (i in 1:length(plsplottype))
tkgrid(tklabel(tt,text=plsplottype [i]),rbuton[[i]],sticky="e")

   tkgrid(tklabel(tt,text="       "))
    onOK <- function()
  {
    type <<- as.numeric((tclvalue(rbValue)))
    tkgrab.release(tt)
    tkdestroy(tt)
tkrreplot(img,plotFunction())  

     }
  onCancel <- function()
  {
    categoria <<- 0
    tkdestroy(tt)
   }
  OK.but     <-tkbutton(tt,text="   OK   ",command=onOK)
  Cancel.but <-tkbutton(tt,text=" Cancel ",command=onCancel)
    tkgrid(tklabel(tt,text="    "))
  tkgrid(tklabel(tt,text="    "),OK.but,Cancel.but,tklabel(tt,text="    "),sticky="w")
  tkgrid(tklabel(tt,text="    "))
  tkfocus(tt)
  tkraise(tt)
  tkwait.window(tt)
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
tkadd(Menu, "command", label="Mode",
      command=function() pls.mode())
copy.but <- tkbutton(tt,text="Copy to pdf",command=CopyToClip)
ok.but <- tkbutton(tt,text="Ok",command=onok)
tkpack(img,side="top")
tkpack(ok.but,side="right",padx="120")
tkpack(copy.but,side="left",padx="120")
tkfocus(tt)
tkraise(tt)
tkwait.window(tt)
   }
  ############# 3D PLOT

   f3<-function(a,b,Validation,main="",xlab=paste("PLS 1 : ",round(explvar(analisis.pls)[1],2),"%"),
ylab=paste("PLS 2:",round(explvar(analisis.pls)[2],2),"%"),zlab=paste("PLS 3: ",round(explvar(analisis.pls)[3],2),"%"),
etiqueta=list(a=info[samp,1],b=info[-samp,1]),angle=60,PLSC=list(a=1,b=2,d=3),
color="white",color2="black",color3="blue",col=list(axis="black",lab="black",main="black",sub="black"),
subt="",size=list(cex=0.7,cex.axis=1,cex.lab=1,cex.main=1,cex.sub=1))
  {
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
if( Validation==TRUE)
{
text(s3d$xyz.convert(b[,PLSC$a],b[,PLSC$b],b[,PLSC$d]),labels=etiqueta$b,
cex=size$cex,lwd=2,pos=3,col=color3)
}
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
  tkwait.window(dlg)}
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
xlab<<-paste("PLS  ",PLSC$a,"  ",round(explvar(analisis.pls)[PLSC$a],2),"%")
  ylab<<-paste("PLS  ",PLSC$b,"  ",round(explvar(analisis.pls)[PLSC$b],2),"%")
           zlab<<-paste("PLS  ",PLSC$d,"  ",round(explvar(analisis.pls)[PLSC$d],2),"%")
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

  #######################
  info=datos$info #Load data
  datos=datos$datos
  
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
  Met.model.1("PLS")
  if (m.model=="random")  #Random Selection
  {
Met.model("PLS")
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
manual.model("PLS")
  }
  sample.class<-vector()
 
  for (i in 1: length(clase))
  {
sample.class[which(info[,categoria]==clase[[i]])]<-i
  }

  dat4=data.frame(enfermedad=sample.class)
  dat4$espectros=(datos)
  Met.check.radio()  #Parameters
  escala=FALSE
  if (ReturnVal$a==1)escala=TRUE
  
  Try(analisis.pls<-plsr(enfermedad ~ espectros,data = dat4,method=ReturnVal$b, scale=escala, validation =ReturnVal$d,subset=samp))
  scores.pls<-plsr(enfermedad ~ espectros,data = dat4, scale=TRUE, validation ="CV",subset=samp)$scores
  rownames(scores.pls)<-info[,categoria][samp]
  g.R2<-R2(analisis.pls, estimate="train",newdata=dat4$espectros[-samp,], ncomp = 1:analisis.pls$ncomp,
   intercept = FALSE, se = FALSE)
  g.MSEP.CV<-MSEP(analisis.pls, estimate="CV", ncomp = 1:analisis.pls$ncomp,
   intercept = FALSE, se = FALSE)
  g.MSEP.BYAS<-MSEP(analisis.pls, estimate="adjCV", ncomp = 1:analisis.pls$ncomp,
   intercept = FALSE, se = FALSE)
  g.MSEP.Test<-MSEP(analisis.pls, estimate="test",newdata=as.data.frame(dat4[-samp,]), ncomp = 1:analisis.pls$ncomp,
   intercept = FALSE, se = FALSE)
  g.RMSEP.CV<-RMSEP(analisis.pls, estimate="CV",newdata=dat4$espectros[-samp,], ncomp = 1:analisis.pls$ncomp,
   intercept = FALSE, se = FALSE)
  g.RMSEP.CV.BYAS<-RMSEP(analisis.pls, estimate="adjCV",newdata=dat4$espectros[-samp,], ncomp = 1:analisis.pls$ncomp,
   intercept = FALSE, se = FALSE)
  g.RMSEP.Test<-RMSEP(analisis.pls, estimate="test",newdata=as.data.frame(dat4[-samp,]), ncomp = 1:analisis.pls$ncomp,
   intercept = FALSE, se = FALSE)

  sum.var<-c()
  for (i in 1:analisis.pls$ncomp)
sum.var[i]<-sum(explvar(analisis.pls)[1:i])
  Model.validation<-list(g.R2,sum.var,g.MSEP.CV,g.MSEP.BYAS,g.MSEP.BYAS,g.MSEP.Test,
g.RMSEP.CV,g.RMSEP.CV.BYAS,g.RMSEP.Test)
  f1(a=Model.validation,ylab=c("R2","Variance (%)","Mean Squared Error of Prediction (MSEP)",
"Mean Squared Error of Prediction (MSEP)","Mean Squared Error of Prediction (MSEP)",
"Root Mean Squared Error of Prediction (RMSEP)","Root Mean Squared Error of Prediction (RMSEP)",
"Root Mean Squared Error of Prediction (MSEP)"), main=c("","Cumulative","Cross-Validation",
"Bias-corrected Cross-Validation","Test-Validation","Cross-Validation","Bias-corrected Cross-Validation",
"Test-Validation"))
  ncomp<-Met.modalDialog("PLS","Number of PLS Components :   ","",entryWidth=4)

  analisis.pls<-plsr(enfermedad ~ espectros,ncomp,data = dat4,method=ReturnVal$b, scale=escala, validation =ReturnVal$d,subset=samp)
  scores.pls<-plsr(enfermedad ~ espectros,ncomp,data = dat4, scale=TRUE, validation ="CV",subset=samp)$scores
  rownames(scores.pls)<-info[,categoria][samp]
  f2(a=scores.pls,Validation=F)
  f2b(analisis.pls,type=c(1))
  if (ncomp>2)
f3(a=scores.pls,Validation=F)
  validacion.pls<-predict(analisis.pls, dat4$espectros[-samp,], type="scores")
  Valores.pls<-predict(analisis.pls, dat4$espectros[-samp,], type="response")
  rownames(validacion.pls)<-info[,categoria][-samp]
  rownames(Valores.pls)<-info[,categoria][-samp]
  


a<-analisis.pls$validation$pred[,1,]
Resultados<-c()
for(i in 1:dim(a)[1]){
Resultados[i]<-round(mean(a[i,]))}

Resultado=c()

  for (i in 1: length(clase))
  {
Resultado[which(Resultados==i)]<-clase[[i]]
  }  
Cross.Validation<-Resultado
print(table(info[,categoria][samp],Cross.Validation))
TABLE(info[,categoria][samp],Cross.Validation,title="Cross Validation")

      f2(a=scores.pls,b=validacion.pls,Validation=T)
if (ncomp>2)
f3(a=scores.pls,b=validacion.pls,Validation=T)
# Validation
Resultados<-c()
b<-Valores.pls[,1,]
for(i in 1:dim(b)[1]){

Resultados[i]<-round(mean(b[i,]))}


Resultado=c()
for (i in 1: length(clase))
  {
Resultado[which(Resultados==i)]<-clase[[i]]
  }  

Validacion.Interna<-Resultado
print(table(info[,categoria][-samp],Validacion.Interna))
TABLE(info[,categoria][-samp],Validacion.Interna,title="Internal validation results.")
  pls.model<-analisis.pls
  tkconfigure(console,cursor="arrow")
  ReturnVal <- tkmessageBox(title="PLS",message="Save PLS model for a later validation?",icon="question",type="yesno")
  if (tclvalue(ReturnVal)=="yes")
  {
fileName<-tclvalue(tkgetSaveFile())
save(pls.model, file=fileName)
  }
}

