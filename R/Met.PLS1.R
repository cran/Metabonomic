Met.PLS1 <-
function(datos,externa)
{

  tkconfigure(console,cursor="watch")
  info=datos$info#Load Data
  datos=datos$datos

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
  attach(info)
  Require("MASS")
  Require("gpls")
  Require("pls")
  dimnames(datos)[[1]]=as.character(datos[,1]) #Data Pretratament
  datos=datos[,-1] 
  colnames(datos)=info$Nombre
  datos<-t(datos)
  dat4=data.frame(enfermedad=as.vector(info[,categoria]))
  dat4$espectros=(datos)
  v4<-dim(info)[1]
  v1=0
  v2=0
  if(info[,categoria][1]==clase2)#Samples Clasification
  {     
for(i in 1:v4)
{
if(info[,categoria][i]==clase2)
{
v1=v1+1
}
if(info[,categoria][i]==clase1)
{
v2=v2+1
}
}
  }
  if(info[,categoria][1]==clase1)
  {
for(i in 1:v4)
  {
if(info[,categoria][i]==clase2)
{
v2=v2+1
}
if(info[,categoria][i]==clase1)
{
v1=v1+1
}
}
  }
  Met.model.1("PLS")
  if (m.model=="random")  #Random Selection
  {
Met.model("PLS")  #Number of samples to build the model
v3 <-elementos$a
v5 <- elementos$b
v6<-v1+1
samp <- c(sample(1:v1,v3), sample(v6:v4,v5)) #Random selection
  }
  if (m.model=="manual")  #Manual Selection
  {
 manual.model("PLS")
  }

  valores.pls <- Met.modalDialog3("PLS","Tolerance for convergence",
"Maximum number of iteration allowed","Number of PLS components",
"1e-3","100","")  # PLS Parameters

  Try(gpls( enfermedad~.,dat4,eps=valores.pls$a,lmax=valores.pls$b,
K=valores.pls$d, subset=samp))
dat.gpls<-gpls( enfermedad~.,dat4,eps=valores.pls$a,lmax=valores.pls$b,
K=valores.pls$d, subset=samp)

  t1=data.frame(enfermedad=info[,categoria][-samp]) #Validation
  t1$espectros=datos[-samp,]
  validacion.gpls<-predict(dat.gpls,t1)$class
  print(table(t1[,1],validacion.gpls))
  print(cbind(info[-samp,],validacion.gpls))
  TABLE(t1[,1],validacion.gpls,title="Internal validation results.")
  tkconfigure(console,cursor="arrow")


}

