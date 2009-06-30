Met.NN1 <-
function(datos,externa){
  Require("class")
  Require("nnet")
  info=datos$info
  datos=datos$datos
  attach(info)
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
  ##############################

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
  
  Met.model.1("Neural Network")
  if (m.model=="random")  #Random Selection
  {
 Met.model("Neural Network") #Select class
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
manual.model("Neural Network")
  }

  dat=data.frame((datos),species= factor(info[,categoria]))
  valores.red <- Met.modalDialog4("Neural Network","A single-hidden-layer neural network is going to be built.","Number of units in the hidden layer","Initial random weights","Weight decay","","0.5","5e-4")
  Try(ir.nn <- nnet(species ~ ., data = dat, subset = samp, size = valores.red$a, rang =valores.red$b ,
      decay = valores.red$d, maxit = 400))
  print(table(dat$species[-samp], predict(ir.nn, dat[-samp,], type = "class")))
  TABLE(dat$species[-samp], predict(ir.nn, dat[-samp,], type = "class"),title="Internal validation results.")
  tkconfigure(console,cursor="arrow")
}

