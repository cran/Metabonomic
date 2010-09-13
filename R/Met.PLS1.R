Met.PLS1 <-
function(datos,externa)
{

  tkconfigure(console,cursor="watch")
  if (length(clase)>2)
  {
ReturnVal <- tkmessageBox(title="PLS",message="Only two class PLS model. Continue?",icon="warning",type="yesno")
if (tclvalue(ReturnVal)=="no")
{
tkconfigure(console,cursor="arrow")
 stop("Cancel by the user")
}
  }
  info=datos$info#Load Data
  datos=datos$datos
  attach(info)
  require(gpls)
  require(pls)

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

  dimnames(datos)[[1]]=as.character(datos[,1]) #Data Pretratament
  datos=datos[,-1] 
  colnames(datos)=info$Nombre
  datos<-t(datos)
  dat4=data.frame(enfermedad=as.vector(info[,categoria]))
  dat4$espectros=(datos)
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
  samp[(d4m[i-1]+1):(d4m[i-1]+d4m[i])]<-samp.list[[i]]
  }
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

