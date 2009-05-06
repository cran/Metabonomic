Met.PLS3D <-
function(datos,externa,angle){
  Require("splines")
  Require("pls")
  info=datos$info #Load data
  datos=datos$datos
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
SliderValue1 <- tclVar(round(v1/2))
SliderValue2 <- tclVar(round(v2/2))
SliderValueLabel1 <- tklabel(dlg,text=as.character(tclvalue(SliderValue1)))
SliderValueLabel2 <- tklabel(dlg,text=as.character(tclvalue(SliderValue2)))
tkconfigure(SliderValueLabel1,textvariable=SliderValue1)
tkconfigure(SliderValueLabel2,textvariable=SliderValue2)


slider1 <- tkscale(dlg, from=(v1-1), to=2,showvalue=F, variable=SliderValue1,
                   resolution=1, orient="horizontal")
slider2 <- tkscale(dlg, from=(v2-1), to=2,showvalue=F, variable=SliderValue2,
                   resolution=1, orient="horizontal")
tkgrid(tklabel(dlg,text="       "))
tkgrid(tklabel(dlg,text="Building the model"),sticky="w")
tkgrid(tklabel(dlg,text="       "))

tkgrid(tklabel(dlg,text="Number of samples of the 'A' group : "),SliderValueLabel1,slider1)
tkgrid(tklabel(dlg,text="Number of samples of the 'B' group : "),SliderValueLabel2,slider2)
tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="       "))
  
onOK <- function()
  {
elementos<<-list(a=as.numeric(tclvalue(SliderValue1)),b=as.numeric(tclvalue(SliderValue2)))
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
  dimnames(datos)[[1]]=as.character(datos[,1])
  datos=datos[,-1] #eliminamos la primera columna
  colnames(datos)=info$Nombre
  datos<-t(datos)
  attach(info)
  dat4=data.frame(enfermedad=info[,categoria])
  dat4$espectros=(datos)
  v4<-dim(info)[1]
  v1=0
  v2=0
  if(info[,categoria][1]==clase2){
for(i in 1:v4){
if(info[,categoria][i]==clase2){
v1=v1+1
}

if(info[,categoria][i]==clase1){
v2=v2+1
}
}}
if(info[,categoria][1]==clase1){
for(i in 1:v4){
if(info[,categoria][i]==clase2){
v2=v2+1
}

if(info[,categoria][i]==clase1){
v1=v1+1
}
}}
Met.model("PLS")
v3 <-elementos$a
v5 <- elementos$b
v6<-v1+1
samp <- c(sample(1:v1,v3), sample(v6:v4,v5))  #Random Selection
clase<-vector()
for(i in 1:v4){  #Change to numeric clasification
if(info[,categoria][i]==clase2){clase[i]<-1}
if(info[,categoria][i]==clase1){clase[i]<-0}
}
dat4=data.frame(enfermedad=clase)
dat4$espectros=(datos)
Met.check.radio()  #Parameters
escala=FALSE
if (ReturnVal$a==1){escala=TRUE}
analisis.pls<-plsr(enfermedad ~ espectros,6,data = dat4,method=ReturnVal$b, scale=escala, validation =ReturnVal$d,subset=samp)
scores.pls<-plsr(enfermedad ~ espectros,6,data = dat4, scale=TRUE, validation ="CV",subset=samp)$scores
 rownames(scores.pls)<-info[,categoria][samp]

kmedias<-kmeans(scores.pls[,1],2,iter.max = 1000, nstart = 1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
library(scatterplot3d)
#scores.pls<-scores.pls[-c(1,7),]
  main<-"PLS Model"
  xlab<-"PLS 1"
  ylab<-"PLS 2"
  zlab="PLS3"
  etiqueta=info[samp,categoria]
  color="white"
  color2="red"
  col=list(axis="black",lab="black",main="black",sub="black")
  subt=""
  size=list(cex=0.7,cex.axis=1,cex.lab=1,cex.main=1,cex.sub=1)
  angle=60

f2a<-function(){
tt <- tktoplevel()


plotFunction <<- function(color,color2,etiqueta,xlab,ylab,main,subt)
{
  par(bg=color)
s3d <- scatterplot3d(scores.pls[,1],scores.pls[,2],scores.pls[,3],type="p",
color=color2, pch=10,box=TRUE,cex.axis=size$cex.axis,cex.lab=size$cex.lab,
cex.main=size$cex.main,cex.sub=size$cex.sub,cex=size$cex,
    main=main, sub=subt, xlim=NULL, ylim=NULL, zlim=NULL,
col.axis=col$axis,col.lab=col$lab,col.main=col$main,col.sub=col$sub,
    xlab=xlab, ylab=ylab, zlab="PLS3", scale.y=1, angle=angle,
    axis=TRUE, tick.marks=TRUE, label.tick.marks=TRUE, grid=TRUE)

text(s3d$xyz.convert(scores.pls[,1],scores.pls[,2],scores.pls[,3]), labels=etiqueta,
cex=size$cex,lwd=2,col=color2, pos=1)

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

by.name<-function(info){
    etiqueta<<-info[samp,1]
tkrreplot(img,plotFunction(color,color2,etiqueta,xlab,ylab,main,subt))
  }

by.type<-function(info){
   etiqueta<<-info[samp,categoria] 
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

copy.but <- tkbutton(tt,text="Copy to Clipboard",command=CopyToClip)
tkgrid(img)
tkgrid(copy.but)
tkfocus(tt)
tkraise(tt)
tkwait.window(tt)
}
f2a()
}

