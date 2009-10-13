Met.Selection <-
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
rbuton<-list()
for (i in 1:length(etiquetas))
rbuton[[i]]<-tkradiobutton(tt)

  rbValue<-tclVar(etiquetas[2])
for (i in 1:length(etiquetas))
  tkconfigure(rbuton[[i]],variable=rbValue,value=i)
  
  tkgrid(tklabel(tt,text="How would you like to class the data?"),sticky="w")
for (i in 1:length(etiquetas))
tkgrid(tklabel(tt,text=paste("By", etiquetas[i])),rbuton[[i]],sticky="e")

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
  ######################
  
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
  tkgrid(tklabel(tt,text="       Class selection of samples ",font=fontHeading),sticky="e")
  tkgrid(tklabel(tt,text="    "))
  niveles<-(levels(info[,categoria])  )
    for (i in 1:dim(as.data.frame(niveles))[1])
{ 
if(is.na(niveles[i])==FALSE)
{
CX=i
}
}
cb<-list()
cbValue<-list()
for (i in 1:length(niveles))
{
  cb[[i]] <- tkcheckbutton(tt)
  cbValue[[i]] <- tclVar("0")
}
for (i in 1:length(niveles))
tkconfigure(cb[[i]],variable=cbValue[[i]])
for (i in 1:length(niveles))
tkgrid(tklabel(tt,text=paste(niveles[i])),cb[[i]],sticky="e")
   tkgrid(tklabel(tt,text="       "))
  
  onOK <- function()
  {
d3o<-c()
for (i in 1:length(niveles))
d3o[i]<-tclvalue(cbValue[[i]])
    categoria2 <<- as.numeric(d3o)
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
  ###################
  
  
  ###################  
  
  info=datos$info
  datos2=datos$datos
  v5<-dim(info)[1]
  s<-dim(info)[2]

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


  Selection<-list()  
  for (i in 1: length(categoria3))
Selection[[i]]<-which(select==categoria3[i])
  
  d4m<-c()
  lista1<-c()
  d4m[1]<-length(Selection[[1]])
  lista1<-Selection[[1]]
  for (i in 2:length(Selection))
  {
d4m[i]<-length(Selection[[i]])
  lista1[(length(lista1)+1):(length(lista1)+d4m[i])]<-Selection[[i]]
  }

   lista2<-c(1,lista1+1)
  info<-info[lista1,]
  datos2<-datos2[,lista2]
  datos<<-(list(datos=datos2,info=info))
  clase<<-list()
for (i in 1:length(categoria3))
clase[[i]]<<-categoria3[i]
  categoria<<-categoria
  #v<-as.matrix(info)
  showData2(info)
  
  #### No enought input selected

  if(sum(categoria2)==1 )
  {
tclvalue(tkmessageBox(title="Selection",
message="You have to select 2 o more inputs",icon="warning",
type="ok",default="ok"))
tkconfigure(console,cursor="arrow")
stop("no category selected")
  }



  memory<<-memory+1
  memory.data[[memory]]<<-list(generation=memory,datos=datos$datos,
  info=datos$info)     #For undo function
  tkconfigure(console,cursor="arrow")


}

