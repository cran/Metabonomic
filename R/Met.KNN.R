Met.KNN <-
function(datos,externa){
  require(class)
  tkconfigure(console,cursor="watch")

  info=datos$info#Load Data
  datos=datos$datos
  attach(info)
  Met.text.check <- function(entryWidth=3,returnValOnCancel="ID_CANCEL")
{
  tt <- tktoplevel()
  fontHeading <- tkfont.create(family="times",size=12,weight="bold")
  fontTextLabel <- tkfont.create(family="times",size=12)
  tkwm.deiconify(tt)
  tkgrab.set(tt)
  tkfocus(tt)
  tkwm.title(tt,"KNN")
  tkgrid(tklabel(tt,text="    "))
  tkgrid(tklabel(tt,text="       Selection of the KNN parameters: ",
font=fontHeading),sticky="e")
  tkgrid(tklabel(tt,text="    "))
  
  textEntryVarTcl <- tclVar(paste("1"))
  textEntryVarTcl2 <- tclVar(paste("0"))
  textEntryWidget <- tkentry(tt,width=paste(entryWidth),textvariable=textEntryVarTcl)
  textEntryWidget2 <- tkentry(tt,width=paste(entryWidth),textvariable=textEntryVarTcl2)

  tkgrid(tklabel(tt,text="       "))
  tkgrid(tklabel(tt,text="Number of neighbours considered"),textEntryWidget,tklabel(tt,text="       "),sticky="e")
  tkgrid(tklabel(tt,text="Minimum vote for definite decision"),textEntryWidget2,tklabel(tt,text="       "),sticky="e")
  tkgrid(tklabel(tt,text="       "))
  tkgrid(tklabel(tt,text="       "))
  
  cb <- tkcheckbutton(tt)
  cbValue <-tclVar(0)
  tkconfigure(cb,variable=cbValue)
  tkgrid(tklabel(tt,text="Use all the neighbours?"),cb,sticky="e")
  tkgrid(tklabel(tt,text="    "))
  
 
  onOK <- function()
  {
    ReturnVal <<- list(a=as.numeric(tclvalue(textEntryVarTcl)),
b=as.numeric(tclvalue(textEntryVarTcl2)),
d=as.numeric(tclvalue(cbValue)))
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
  
  Met.model.1("K Nearest Neighbor")

  if (m.model=="random")  #Random Selection
  {
Met.model("KNN") #Build the model
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
manual.model("K Nearest Neighbor")
  }

  dat4=data.frame(enfermedad=info[,categoria])
  dat4$espectros=(datos)
  Met.text.check() 
  ReturnVal
  todos=FALSE
  if(ReturnVal$d==1){ todos=TRUE}
 
  Try(knn.cv((dat4$espectros[samp,]),
cl=info[,categoria][samp], k = ReturnVal$a, l = ReturnVal$b,
prob = FALSE, use.all =todos)) 

  KNN.Autovalidacion<-knn.cv((dat4$espectros[samp,]),
cl=info[,categoria][samp], k = ReturnVal$a, l = ReturnVal$b,
prob = FALSE, use.all =todos)  #Cross Validation
  print(table(info[,categoria][samp],KNN.Autovalidacion))
  TABLE(info[,categoria][samp],KNN.Autovalidacion,title="Cross validation results.")

  Try(knn((dat4$espectros[samp,]), (dat4$espectros[-samp,]),
 cl=info[,categoria][samp], k =ReturnVal$a , l = ReturnVal$b,
 prob = FALSE, use.all = todos))
  KNN.Validacion<-knn((dat4$espectros[samp,]), (dat4$espectros[-samp,]),
 cl=info[,categoria][samp], k =ReturnVal$a , l = ReturnVal$b,
 prob = FALSE, use.all = todos)  #Internal Validation
  print(table(info[,categoria][-samp],KNN.Validacion))
  TABLE(info[,categoria][-samp],KNN.Validacion,title="Internal validation results.")

  tkconfigure(console,cursor="arrow")

}

