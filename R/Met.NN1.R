`Met.NN1` <-
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

  dat=data.frame((datos),species= factor(info[,categoria]))
  valores.red <- Met.modalDialog4("Neural Network","A single-hidden-layer neural network is going to be built.","Number of units in the hidden layer","Initial random weights","Weight decay","","0.5","5e-4")
  Try(nnet(species ~ ., data = dat, subset = samp, size = valores.red$a, rang =valores.red$b ,
      decay = valores.red$d, maxit = 400))
  ir.nn <- nnet(species ~ ., data = dat, subset = samp, size = valores.red$a, rang =valores.red$b ,
      decay = valores.red$d, maxit = 400)
  print(table(dat$species[-samp], predict(ir.nn, dat[-samp,], type = "class")))
  TABLE(dat$species[-samp], predict(ir.nn, dat[-samp,], type = "class"),title="Internal validation results.")
  tkconfigure(console,cursor="arrow")
}

