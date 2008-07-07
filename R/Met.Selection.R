`Met.Selection` <-
function(datos,externa.inicial){
  tkconfigure(console,cursor="watch")
  Met.radio_seleccion1 <- function(etiquetas,s,entryWidth=7,returnValOnCancel="ID_CANCEL")
  {

entryWidth=7
  tt <- tktoplevel()
  fontHeading <- tkfont.create(family="times",size=12,weight="bold")
  fontTextLabel <- tkfont.create(family="times",size=12)
  tkwm.deiconify(tt)
  tkgrab.set(tt)
  tkfocus(tt)
  tkwm.title(tt,"Category Selection")
  tkgrid(tklabel(tt,text="    "))
  tkgrid(tklabel(tt,text="       Parameters for the data clasification",font=fontHeading),sticky="e")
  tkgrid(tklabel(tt,text="    "))

  rb1 <- tkradiobutton(tt)
  rb2 <- tkradiobutton(tt)
  rb3 <- tkradiobutton(tt)
  rb4 <- tkradiobutton(tt)
  rb5 <- tkradiobutton(tt)
 
  rbValue<-tclVar(etiquetas[2])
  tkconfigure(rb1,variable=rbValue,value=2)
  tkconfigure(rb2,variable=rbValue,value=3)
  tkconfigure(rb3,variable=rbValue,value=4)
  tkconfigure(rb4,variable=rbValue,value=5)
  tkconfigure(rb5,variable=rbValue,value=6)


  tkgrid(tklabel(tt,text="How would you like to class the data?"),sticky="w")
    if (s==2) 
{
  tkgrid(tklabel(tt,text=paste("By", etiquetas[2])),rb1,sticky="e")
}
  if (s==3) 
{
  tkgrid(tklabel(tt,text=paste("By", etiquetas[2])),rb1,sticky="e")
  tkgrid(tklabel(tt,text=paste("By", etiquetas[3])),rb2,sticky="e")
}
if (s==4) 
{
  tkgrid(tklabel(tt,text=paste("By", etiquetas[2])),rb1,sticky="e")
  tkgrid(tklabel(tt,text=paste("By", etiquetas[3])),rb2,sticky="e")
  tkgrid(tklabel(tt,text=paste("By", etiquetas[4])),rb3,sticky="e")
}

if (s==5) 
{
  tkgrid(tklabel(tt,text=paste("By", etiquetas[2])),rb1,sticky="e")
  tkgrid(tklabel(tt,text=paste("By", etiquetas[3])),rb2,sticky="e")
  tkgrid(tklabel(tt,text=paste("By", etiquetas[4])),rb3,sticky="e")
  tkgrid(tklabel(tt,text=paste("By", etiquetas[5])),rb4,sticky="e")
}

if (s==6) 
{
  tkgrid(tklabel(tt,text=paste("By", etiquetas[2])),rb1,sticky="e")
  tkgrid(tklabel(tt,text=paste("By", etiquetas[3])),rb2,sticky="e")
  tkgrid(tklabel(tt,text=paste("By", etiquetas[4])),rb3,sticky="e")
  tkgrid(tklabel(tt,text=paste("By", etiquetas[5])),rb4,sticky="e")
  tkgrid(tklabel(tt,text=paste("By", etiquetas[6])),rb5,sticky="e")
}

   tkgrid(tklabel(tt,text="       "))
    onOK <- function()
  {
    categoria <<- (tclvalue(rbValue))
    tkgrab.release(tt)
    tkdestroy(tt)
     }
  onCancel <- function()
  {
    categoria <<- 0
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
  }

  Met.radio_seleccion2 <- function(info,categoria,entryWidth=7,returnValOnCancel="ID_CANCEL")
  {
categoria<-as.numeric(categoria)
  tt <- tktoplevel()
  fontHeading <- tkfont.create(family="times",size=12,weight="bold")
  fontTextLabel <- tkfont.create(family="times",size=12)
  tkwm.deiconify(tt)
  tkgrab.set(tt)
  tkfocus(tt)
  tkwm.title(tt,"Category Selection")
  tkgrid(tklabel(tt,text="    "))
  tkgrid(tklabel(tt,text="       Select two class of samples ",font=fontHeading),sticky="e")
  tkgrid(tklabel(tt,text="    "))
  niveles<-(levels(info[,categoria])  )
    for (i in 1:dim(as.data.frame(niveles))[1])
{ 
if(is.na(niveles[i])==FALSE)
{
CX=i
}
}
if (CX>5)
{
tkmessageBox(message="To many inputs!",icon="warning")
 stop("To many inputs!")
}
  cb1 <- tkcheckbutton(tt)
  cbValue <- tclVar("0")
  cb2 <- tkcheckbutton(tt)
  cbValue2 <- tclVar("0")
  cb3 <- tkcheckbutton(tt)
  cbValue3 <- tclVar("0")
  cb4 <- tkcheckbutton(tt)
  cbValue4 <- tclVar("0")
  cb5 <- tkcheckbutton(tt)
  cbValue5 <- tclVar("0")
  cb6 <- tkcheckbutton(tt)
  cbValue6 <- tclVar("0")
  cb7 <- tkcheckbutton(tt)
  cbValue7 <- tclVar("0")
 
categoria
tkconfigure(cb1,variable=cbValue)
tkconfigure(cb2,variable=cbValue2)
tkconfigure(cb3,variable=cbValue3)
tkconfigure(cb4,variable=cbValue4)
tkconfigure(cb5,variable=cbValue5)
tkconfigure(cb6,variable=cbValue6)
tkconfigure(cb7,variable=cbValue7)

  if (CX==2) 
{
  tkgrid(tklabel(tt,text=paste(niveles[1])),cb1,sticky="e")
  tkgrid(tklabel(tt,text=paste(niveles[2])),cb2,sticky="e")
}
  
if (CX==3) 
{
  tkgrid(tklabel(tt,text=paste(niveles[1])),cb1,sticky="e")
  tkgrid(tklabel(tt,text=paste(niveles[2])),cb2,sticky="e")
  tkgrid(tklabel(tt,text=paste(niveles[3])),cb3,sticky="e")
}

if (CX==4) 
{
  tkgrid(tklabel(tt,text=paste(niveles[1])),cb1,sticky="e")
  tkgrid(tklabel(tt,text=paste(niveles[2])),cb2,sticky="e")
  tkgrid(tklabel(tt,text=paste(niveles[3])),cb3,sticky="e")
  tkgrid(tklabel(tt,text=paste(niveles[4])),cb4,sticky="e")
}

if (CX==5) 
{
  tkgrid(tklabel(tt,text=paste(niveles[1])),cb1,sticky="e")
  tkgrid(tklabel(tt,text=paste(niveles[2])),cb2,sticky="e")
  tkgrid(tklabel(tt,text=paste(niveles[3])),cb3,sticky="e")
  tkgrid(tklabel(tt,text=paste(niveles[4])),cb4,sticky="e")
  tkgrid(tklabel(tt,text=paste(niveles[5])),cb5,sticky="e")
}

   tkgrid(tklabel(tt,text="       "))
  
  onOK <- function()
  {
    categoria2 <<- as.numeric(c((tclvalue(cbValue)),(tclvalue(cbValue2)),
(tclvalue(cbValue3)),(tclvalue(cbValue4)),(tclvalue(cbValue5))))
    j=0
    categoria3=vector()
    for (i in 1:CX)
{ 
    if(categoria2[i]=="1")
{
j=j+1
categoria3[j]<-niveles[i]
}
}
categoria3<<-categoria3
tkgrab.release(tt)
    tkdestroy(tt)
   }
  onCancel <- function()
  {
    categoria2 <<- 0
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
  }

  Met.display.info<-function(t1)
  {
  t1<-as.matrix(t1)
  tclArray1 = tclArray()
  Require("tcltk")
  tclArray1 = tclArray()
  for (i in (1:dim(t1)[1]))
{
  for (j in (1:dim(t1)[2]))
{
      tclArray1[[i,j]] <- t1[i,j]
  tclArray1[[i,0]] <- i
tclArray1[[0,j]] <- as.character(colnames(t1)[j])
   }
}
Require("tcltk2")
  tt <- tktoplevel()
  fonttable <- tkfont.create(family="times",size=12)
  tkwm.title(tt,"Info file")
  table1 <- tk2table(tt,rows=dim(t1)[1]+1,cols=dim(t1)[2]+1,titlerows=1,titlecols=0,
    xscrollcommand=function(...) tkset(xscr,...),yscrollcommand=function(...) tkset(yscr,...))
  xscr <-tkscrollbar(tt,orient="horizontal", command=function(...)tkxview(table1,...))
  yscr <- tkscrollbar(tt,command=function(...)tkyview(table1,...))

  tkgrid(table1,yscr)
  tkgrid.configure(yscr,sticky="nsw")
  tkgrid(xscr,sticky="new")
  tkconfigure(table1,variable=tclArray1,background="white",foreground="black",font=fonttable,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
  return (table1)
  #tkwait.window(tt)
  }
  
  info=datos$info
  datos2=datos$datos
  v5<-dim(info)[1]
  s<-dim(info)[2]
  v1=0
  v2=0
  v3=0
  v4=0
  C1=vector()
  C2=vector()
  C3=vector()
  C4=vector()
  clase1<<-NULL
  clase2<<-NULL
  clase3<<-NULL
  clase4<<-NULL
  etiquetas<-colnames(info)
  levels(info$Categoria)[2]
  Met.radio_seleccion1(etiquetas,s,entryWidth=7,returnValOnCancel="ID_CANCEL")
  if(categoria==0)
  {
  tkmessageBox(message="No file selected!",icon="warning")
tkconfigure(console,cursor="arrow")
  stop("no category selected")
  }

  Met.radio_seleccion2(info,categoria,entryWidth=7,returnValOnCancel="ID_CANCEL")
  if(dim(as.data.frame(categoria2))[1]==1)
  {
  tkmessageBox(message="No file selected!",icon="warning")
tkconfigure(console,cursor="arrow")
 stop("no category selected")
  }
  categoria<-as.numeric(categoria)
  categoria3
  select<-info[,categoria]

#########################################3 class inputs selected

  if(sum(categoria2)==3)
  {
  for(i in 1:v5)
 {
if(select[i]==categoria3[1])
{
v1=v1+1
C1[v1]=i
}
if(select[i]==categoria3[2])
{
v2=v2+1
C2[v2]=i
}
if(select[i]==categoria3[3])
{
v3=v3+1
C3[v3]=i
}

  }

   lista1<-c(C1,C2,C3)
   lista2<-c(1,C1+1,C2+1,C3+1)
  info<-info[lista1,]
  datos2<-datos2[,lista2]
  datos<<-(list(datos=datos2,info=info))
  clase1<<-categoria3[1]
  clase2<<-categoria3[2]
  clase3<<-categoria3[3]
  categoria<<-categoria
  #v<-as.matrix(info)
  Met.display.info(info)
  }

####################################################### 2 class input selected

  if(sum(categoria2)==2)
  {
for(i in 1:v5)
{
if(select[i]==categoria3[1])
{
v1=v1+1
C1[v1]=i
}
if(select[i]==categoria3[2])
{
v2=v2+1
C2[v2]=i
}
}
  if (C1[1]>=C2[1])
{
lista1<-c(C2,C1)
lista2<-c(1,C2+1,C1+1)
  }
  if (C1[1]<=C2[1])
{
lista1<-c(C1,C2)
lista2<-c(1,C1+1,C2+1)
  }
  info<-info[lista1,]
  datos2<-datos2[,lista2]
  datos<<-(list(datos=datos2,info=info))
  clase1<<-categoria3[1]
  clase2<<-categoria3[2]
  clase3<<-0
  categoria<<-categoria
  #v<-as.matrix(info)
  Met.display.info(info)
  #eleccion<-tclvalue(tkmessageBox(title="Selection",
#message="Will you use a external validation file?",icon="question",
#type="yesno",default="yes"))
  eleccion<-"no"
  if (eleccion=="yes") # External Validation
{
info.externa=externa.inicial$info
datos.externa=externa.inicial$datos
v5<-dim(info.externa)[1]
v1=0
v2=0
C4=vector()
C5=vector()
select2<-info.externa[,categoria]
for(i in 1:v5){
if(select2[i]==categoria3[1]){
v1=v1+1
C4[v1]=i
}
if(select2[i]==categoria3[2]){
v2=v2+1
C5[v2]=i
}
}
if (C4[1]>=C5[1]){
lista3<-c(C4,C3)
lista4<-c(1,C4+1,C3+1)
}
if (C3[1]<=C4[1]){
lista3<-c(C3,C4)
lista4<-c(1,C3+1,C4+1)
}
info.externa<-info.externa[lista3,]
datos.externa<-datos.externa[,lista4]
externa<<-(list(datos=datos.externa,info=info.externa))
  }
}
####################################################### 4 class input selected

  if(sum(categoria2)==4)
  {
  for(i in 1:v5)
{
if(select[i]==categoria3[1])
{
v1=v1+1
C1[v1]=i
}
if(select[i]==categoria3[2])
{
v2=v2+1
C2[v2]=i
}
if(select[i]==categoria3[3])
{
v3=v3+1
C3[v3]=i
}
if(select[i]==categoria3[4])
{
v4=v4+1
C4[v4]=i
}

  }
   lista1<-c(C1,C2,C3,C4)
   lista2<-c(1,C1+1,C2+1,C3+1,C4+1)
  info<-info[lista1,]
  datos2<-datos2[,lista2]
  datos<<-(list(datos=datos2,info=info))
  clase1<<-categoria3[1]
  clase2<<-categoria3[2]
  clase3<<-categoria3[3]
  clase4<<-categoria3[4]
  categoria<<-categoria
  #v<-as.matrix(info)
  Met.display.info(info)
  }

####################################################### No enought input selected

  if(sum(categoria2)==1 )
  {
tclvalue(tkmessageBox(title="Selection",
message="You have to select 2 o more inputs",icon="warning",
type="ok",default="ok"))
tkconfigure(console,cursor="arrow")
stop("no category selected")
  }

  if(sum(categoria2)>4 )
  {
tclvalue(tkmessageBox(title="Selection",
 message="No implemented yet",icon="warning",
 type="ok",default="ok"))
 tkconfigure(console,cursor="arrow")

stop("no category selected")
  }

  memory<<-memory+1
  memory.data[[memory]]<<-list(generation=memory,datos=datos$datos,
  info=datos$info)     #For undo function
  tkconfigure(console,cursor="arrow")


}

