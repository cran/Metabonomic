`Met.NN2` <-
function(datos,externa){
  Require("class")
  Require("AMORE")
  Require("neural")
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

tkgrid(tklabel(dlg,text=paste("Number of samples of the",clase1,"group : ")),SliderValueLabel1,slider1)
tkgrid(tklabel(dlg,text=paste("Number of samples of the",clase2,"group : ")),SliderValueLabel2,slider2)
if (is.character(clase3)==TRUE & is.character(clase4)==FALSE)
{
SliderValue3 <- tclVar(round(v1b/2))
SliderValueLabel3 <- tklabel(dlg,text=as.character(tclvalue(SliderValue3)))
tkconfigure(SliderValueLabel3,textvariable=SliderValue3)
slider3 <- tkscale(dlg, from=(v1b-1), to=2,showvalue=F, variable=SliderValue3,
                   resolution=1, orient="horizontal")
tkgrid(tklabel(dlg,text=paste("Number of samples of the",clase3,"group : ")),SliderValueLabel3,slider3)
}
 if (is.character(clase3)==TRUE & is.character(clase4)==TRUE)
{
SliderValue3 <- tclVar(round(v1b/2))
SliderValueLabel3 <- tklabel(dlg,text=as.character(tclvalue(SliderValue3)))
tkconfigure(SliderValueLabel3,textvariable=SliderValue3)
slider3 <- tkscale(dlg, from=(v1b-1), to=2,showvalue=F, variable=SliderValue3,
                   resolution=1, orient="horizontal")

SliderValue4 <- tclVar(round(v1c/2))
SliderValueLabel4 <- tklabel(dlg,text=as.character(tclvalue(SliderValue4)))
tkconfigure(SliderValueLabel4,textvariable=SliderValue4)
slider4 <- tkscale(dlg, from=(v1c-1), to=2,showvalue=F, variable=SliderValue4,
                   resolution=1, orient="horizontal")
tkgrid(tklabel(dlg,text=paste("Number of samples of the",clase3,"group : ")),SliderValueLabel3,slider3)
tkgrid(tklabel(dlg,text=paste("Number of samples of the",clase4,"group : ")),SliderValueLabel4,slider4)

}
tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="       "))
  
onOK <- function()
  {
if (is.character(clase3)==FALSE)
{
elementos<<-list(a=as.numeric(tclvalue(SliderValue1)),
b=as.numeric(tclvalue(SliderValue2)))
}
if (is.character(clase3)==TRUE & is.character(clase4)==FALSE)
{
elementos<<-list(a=as.numeric(tclvalue(SliderValue1)),
b=as.numeric(tclvalue(SliderValue2)),d=as.numeric(tclvalue(SliderValue3)))
}
if (is.character(clase3)==TRUE & is.character(clase4)==TRUE)
{
elementos<<-list(a=as.numeric(tclvalue(SliderValue1)),
b=as.numeric(tclvalue(SliderValue2)),
d=as.numeric(tclvalue(SliderValue3)),e=as.numeric(tclvalue(SliderValue3)))

}
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
  j=k=l=m=0
  p=q=r=s=c()
  for(i in 1:v4)
  {
if(info[,categoria][i]==clase1)
{
j=j+1
p[j]=i
}
if(info[,categoria][i]==clase2)
{
k=k+1
q[k]=i
}
if (is.character(clase3)==TRUE & is.character(clase4)==FALSE)
{
if(info[,categoria][i]==clase3)
{
l=l+1
r[l]=i
}
}
if (is.character(clase3)==TRUE & is.character(clase4)==TRUE)
{
if(info[,categoria][i]==clase3)
{
l=l+1
r[l]=i
}
if(info[,categoria][i]==clase4)
{
m=m+1
s[m]=i
}
}
  }
  rownames(info)
  v1=p[length(p)]-p[1]+1
  v2=q[length(q)]-q[1]+1
  if (is.character(clase3)==TRUE & is.character(clase4)==FALSE)
  {
v1b=r[length(r)]-r[1]+1
  }
  if (is.character(clase3)==TRUE & is.character(clase4)==TRUE)
  {
v1b=r[length(r)]-r[1]+1
v1c=s[length(s)]-s[1]+1
  }
  Met.model.1("Neural Network")

  if (m.model=="random")  #Random Selection
  {
Met.model("Neural Network") #Select class
v3 <-elementos$a
v5 <- elementos$b
v6<-v1+1
if (is.character(clase3)==FALSE)
{
samp <- c(sample(p[1]:p[length(p)],v3), sample(q[1]:q[length(q)],v5))
}
if (is.character(clase3)==TRUE & is.character(clase4)==FALSE)
{
v5b<-elementos$d
samp <- c(sample(p[1]:p[length(p)],v3), sample(q[1]:q[length(q)],v5),
 sample(r[1]:r[length(r)],v5b))
}
  if (is.character(clase3)==TRUE & is.character(clase4)==TRUE)
{
v5b<-elementos$d
v5c <-elementos$e
samp <- c(sample(p[1]:p[length(p)],v3), sample(q[1]:q[length(q)],v5),
 sample(r[1]:r[length(r)],v5b),sample(s[1]:s[length(s)],v5c))
}
  }
  if (m.model=="manual")  #Manual Selection
  {
manual.model("Neural Network")
  }

  clase<-vector()
  for(i in 1:v4)  #Change to numeric clasification
  {  
if (is.character(clase3)==TRUE & is.character(clase4)==TRUE)
{
if(info[,categoria][i]==clase4){clase[i]<-3}
if(info[,categoria][i]==clase3){clase[i]<-2}
}
  if (is.character(clase3)==TRUE & is.character(clase4)==FALSE)
{
if(info[,categoria][i]==clase3){clase[i]<-2}
}
if(info[,categoria][i]==clase2){clase[i]<-1}
if(info[,categoria][i]==clase1){clase[i]<-0}
  }
  i=1
  decision<-"yes"
  neurona<-vector()
  neurona[1]=dim(datos)[2] #First layer (entrance)
  while (decision!="no")
  {
i=i+1
Require("tcltk")
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
  Try(newff(n.neurons=neurona, learning.rate.global=ReturnVal$a,
 momentum.global=ReturnVal$b,error.criterium=ReturnVal$error, Stao=NA,
 hidden.layer=ReturnVal$h.layer,output.layer=ReturnVal$o.layer,
 method=ReturnVal$metodo))
  net <- newff(n.neurons=neurona, learning.rate.global=ReturnVal$a,
   momentum.global=ReturnVal$b,error.criterium=ReturnVal$error, Stao=NA,
 hidden.layer=ReturnVal$h.layer,output.layer=ReturnVal$o.layer,
 method=ReturnVal$metodo)

  if(ReturnVal$metodo=="ADAPTgdwm")
  {
Try(ADAPTgdwm.MLPnet(net,datos[samp,], clase[samp],n.epochs=25000))
net2<-ADAPTgdwm.MLPnet(net,datos[samp,], clase[samp],n.epochs=25000)
  }
  if(ReturnVal$metodo=="ADAPTgd")
  {
Try(ADAPTgd.MLPnet(net,datos[samp,], clase[samp],n.epochs=25000))
net2<-ADAPTgd.MLPnet(net,datos[samp,], clase[samp],n.epochs=25000)
  }

  if(ReturnVal$metodo=="BATCHgd")
  {
Try(BATCHgd.MLPnet(net,datos[samp,], clase[samp],n.epochs=25000))
net2<-BATCHgd.MLPnet(net,datos[samp,], clase[samp],n.epochs=25000)
  }
  else{
Try(ADAPTgd.MLPnet(net,datos[samp,], clase[samp],n.epochs=25000))
net2<-ADAPTgd.MLPnet(net,datos[samp,], clase[samp],n.epochs=25000)
  }

  z<-sim.MLPnet(net2,datos[-samp,])
  Valores2<-round(z)
  Resultado<-c(mode="character")
  v7<-length(Valores2)
  for(i in 1:v7)
  {
  if (is.character(clase3)==TRUE & is.character(clase4)==TRUE)
{
if(round(Valores2)[i]>=3)
{
Resultado[i]<-clase4
}
if(round(Valores2)[i]==2)
{
Resultado[i]<-clase3
}
}
if (is.character(clase3)==TRUE & is.character(clase4)==FALSE)
{
if(round(Valores2)[i]==2)
{
Resultado[i]<-clase3
}
}
if(round(Valores2)[i]==1)
{
Resultado[i]<-clase2
}
if(round(Valores2)[i]<=0)
{
Resultado[i]<-clase1
}
  }
  print(table(info[,categoria][-samp],Resultado))
  TABLE(info[,categoria][-samp],Resultado,title="Internal validation results.")
 
  tkconfigure(console,cursor="arrow")
}

