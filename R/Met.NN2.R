Met.NN2 <-
function(datos,externa){
  require(class)
  require(AMORE)
  #Require("neural")
  tkconfigure(console,cursor="watch")

  info=datos$info
  datos=datos$datos
  attach(info)
  Met.text.radio <- function(entryWidth=7,returnValOnCancel="ID_CANCEL")
  {
entryWidth=7
tt <- tktoplevel()
  fontHeading <- tkfont.create(family="times",size=12,weight="bold")
  fontTextLabel <- tkfont.create(family="times",size=12)
  tkwm.deiconify(tt)
  tkgrab.set(tt)
  tkfocus(tt)
  tkwm.title(tt,"Neural Network")
  tkgrid(tklabel(tt,text="    "))
  tkgrid(tklabel(tt,text="       Selection of the Neural Network parameters: ",font=fontHeading),sticky="e")
  tkgrid(tklabel(tt,text="    "))
  
  textEntryVarTcl <- tclVar(paste("1e-3"))
  textEntryVarTcl2 <- tclVar(paste("0.5"))
  textEntryWidget <- tkentry(tt,width=paste(entryWidth),textvariable=textEntryVarTcl)
  textEntryWidget2 <- tkentry(tt,width=paste(entryWidth),textvariable=textEntryVarTcl2)

  tkgrid(tklabel(tt,text="       "))
  tkgrid(tklabel(tt,text="Learning rate at which every neuron is trained."),textEntryWidget,tklabel(tt,text="       "),sticky="e")
  tkgrid(tklabel(tt,text="Momentum for every neuron."),textEntryWidget2,tklabel(tt,text="       "),sticky="e")
  tkgrid(tklabel(tt,text="       "))
  tkgrid(tklabel(tt,text="       "))
  
  rb1 <- tkradiobutton(tt)
  rb2 <- tkradiobutton(tt)
  rbValue<-tclVar("LMLS")
  tkconfigure(rb1,variable=rbValue,value="LMS")
  tkconfigure(rb2,variable=rbValue,value="LMLS")
  tkgrid(tklabel(tt,text="Error criterium:"),sticky="w")
  tkgrid(tklabel(tt,text="Least Mean Squares"),rb1,sticky="e")
  tkgrid(tklabel(tt,text="Least Mean Logarithm Squared"),rb2,sticky="e")

  
  rb3 <- tkradiobutton(tt)
  rb4 <- tkradiobutton(tt)
  rb5 <- tkradiobutton(tt)
  rb6 <- tkradiobutton(tt)
  rbValue2<-tclVar("tansig")
  tkconfigure(rb3,variable=rbValue2,value="purelin")
  tkconfigure(rb4,variable=rbValue2,value="tansig")
  tkconfigure(rb5,variable=rbValue2,value="sigmoid")
  tkconfigure(rb6,variable=rbValue2,value="hardlim")
  tkgrid(tklabel(tt,text="    "))
  tkgrid(tklabel(tt,text="Activation function of the hidden layer neurons:"),sticky="w")
  tkgrid(tklabel(tt,text="Purelin"),rb3,sticky="e")
  tkgrid(tklabel(tt,text="Tansig"),rb4,sticky="e")
  tkgrid(tklabel(tt,text="Sigmoide"),rb5,sticky="e")
  tkgrid(tklabel(tt,text="Hardlim"),rb6,sticky="e")
  
  rb7 <- tkradiobutton(tt)
  rb8 <- tkradiobutton(tt)
  rb9 <- tkradiobutton(tt)
  rb10 <- tkradiobutton(tt)
  rbValue3<-tclVar("purelin")
  tkconfigure(rb7,variable=rbValue3,value="purelin")
  tkconfigure(rb8,variable=rbValue3,value="tansig")
  tkconfigure(rb9,variable=rbValue3,value="sigmoid")
  tkconfigure(rb10,variable=rbValue3,value="hardlim")
  tkgrid(tklabel(tt,text="    "))
  tkgrid(tklabel(tt,text="Activation function of the output layer"),sticky="w")
  tkgrid(tklabel(tt,text="Purelin"),rb7,sticky="e")
  tkgrid(tklabel(tt,text="Tansig"),rb8,sticky="e")
  tkgrid(tklabel(tt,text="Sigmoide"),rb9,sticky="e")
  tkgrid(tklabel(tt,text="Hardlim"),rb10,sticky="e") 
 
  rb11 <- tkradiobutton(tt)
  rb12 <- tkradiobutton(tt)
  rb13 <- tkradiobutton(tt)
  rb14 <- tkradiobutton(tt)
  rbValue4<-tclVar("ADAPTgdwm")
  tkconfigure(rb11,variable=rbValue4,value="ADAPTgd")
  tkconfigure(rb12,variable=rbValue4,value="ADAPTgdwm")
  tkconfigure(rb13,variable=rbValue4,value="BATCHgd")
  tkconfigure(rb14,variable=rbValue4,value="BATCHgdwm")
  tkgrid(tklabel(tt,text="    "))
  tkgrid(tklabel(tt,text="Training method"),sticky="w")
  tkgrid(tklabel(tt,text="Adaptative gradient descend"),rb11,sticky="e")
  tkgrid(tklabel(tt,text="Adaptative gradient descend with momentum"),rb12,sticky="e")
  tkgrid(tklabel(tt,text="BATCH gradient descend"),rb13,sticky="e")
  tkgrid(tklabel(tt,text="BATCH gradient descend with momentum"),rb14,sticky="e")
   
  onOK <- function()
  {
    ReturnVal <<- list(a=as.numeric(tclvalue(textEntryVarTcl)),
b=as.numeric(tclvalue(textEntryVarTcl2)),error=as.character(tclvalue(rbValue)),
    h.layer=as.character(tclvalue(rbValue2)),o.layer=as.character(tclvalue(rbValue3)),
metodo=as.character(tclvalue(rbValue4)))
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
  ######################

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
  
  sample.class<-vector()
 
  for (i in 1: length(clase))
  {
sample.class[which(info[,categoria]==clase[[i]])]<-i
  }
  i=1
  decision<-"yes"
  neurona<-vector()
  neurona[1]=dim(datos)[2] #First layer (entrance)
  while (decision!="no")
  {
i=i+1
require(tcltk)
 neurona[i] <- Met.modalDialog("Neural Network",
paste("Number of units in the layer",i-1),"")
decision<-tclvalue(tkmessageBox(message="Add another layer?",
icon="question",type="yesno",default="yes"))
  }
  i=i+1
  neurona[i]<-1  #Last layer (exit)
  neurona<-as.numeric(neurona[0:i])
  Met.text.radio() #NN Parameters
  ReturnVal
  Try(net <- newff(n.neurons=neurona, learning.rate.global=ReturnVal$a,
   momentum.global=ReturnVal$b,error.criterium=ReturnVal$error, Stao=NA,
 hidden.layer=ReturnVal$h.layer,output.layer=ReturnVal$o.layer,
 method=ReturnVal$metodo))

  if(ReturnVal$metodo=="ADAPTgdwm")
  {

Try(net2<-ADAPTgdwm.MLPnet(net,datos[samp,], as.numeric(sample.class[samp]),n.epochs=25000))
  }
  if(ReturnVal$metodo=="ADAPTgd")
  {
Try(net2<-ADAPTgd.MLPnet(net,datos[samp,], as.numeric(sample.class[samp]),n.epochs=25000))
  }

  if(ReturnVal$metodo=="BATCHgd")
  {
Try(net2<-BATCHgd.MLPnet(net,datos[samp,], as.numeric(sample.class[samp]),n.epochs=25000))
  }
  else{
Try(net2<-ADAPTgd.MLPnet(net,datos[samp,], as.numeric(sample.class[samp]),n.epochs=25000))
  }

  z<-sim.MLPnet(net2,datos[-samp,])
  Valores2<-round(z)
  Resultado<-c(mode="character")
  v7<-length(Valores2)
  for (i in 1: length(clase))
  {
Resultado[which(Valores2==i)]<-clase[[i]]
  }  

  print(table(info[,categoria][-samp],Resultado))
  TABLE(info[,categoria][-samp],Resultado,title="Internal validation results.")
 
  tkconfigure(console,cursor="arrow")
}

