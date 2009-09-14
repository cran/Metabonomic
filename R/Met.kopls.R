Met.kopls <-
function(datos)
{
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

KOPL.MODEL<-function()#Graphical Interface
{
require(tcltk)#Packages
  require(tkrplot)
tt <- tktoplevel(background="white")#Main windows
  tkwm.title(tt,"KOPLS Model")

#######################################################

######Plot function

plotFunction1 <- function()
  {
  params <- par(bg="white")
  koplsPlotCVDiagnostics(modelCV)
     }
plotFunction2 <- function()
  {
  params <- par(bg="white")
  koplsPlotSensSpec(modelCV)
     
}
########################################################
img1 <- tkrplot(tt,fun=plotFunction1,hscale=1.7,vscale=1)
img2 <- tkrplot(tt,fun=plotFunction2,hscale=1,vscale=0.8)
  tkgrid(img1,columnspan=10,row=1)
tkgrid(img2,columnspan=4,rowspan=20)
######Kernel function
rb1 <- tkradiobutton(tt,background="white")
  rb2 <- tkradiobutton(tt,background="white")
  rbValue<-tclVar(kfunction)
  tkconfigure(rb1,variable=rbValue,value="g")
  tkconfigure(rb2,variable=rbValue,value="p")
  tkgrid(tklabel(tt,text="Kernel Function:",background="white"),row=2,column=4,sticky="w")
  tkgrid(tklabel(tt,text="Gaussian",background="white"),row=2,column=5,sticky="e")
tkgrid(rb1,sticky="w",row=2,column=6,sticky="w")
  tkgrid(tklabel(tt,text="Polynomial",background="white"),row=3,column=5,sticky="e")
tkgrid(rb2,sticky="w",row=3,column=6,sticky="w")

text1 <- tclVar(v.sigma)
  textEntryWidget <- tkentry(tt,width=6,textvariable=text1,background="white")
tkgrid(tklabel(tt,text="Kernel Parameter (Sigma/Order)",background="white"),row=4,column=4,sticky="e")
tkgrid(textEntryWidget,row=4,column=5,sticky="e")

######Pre-processing parameters for the K matrix:
rb3 <- tkradiobutton(tt,background="white")
  rb4 <- tkradiobutton(tt,background="white")
  rbValue2<-tclVar(preK)
  tkconfigure(rb3,variable=rbValue2,value="mc")
  tkconfigure(rb4,variable=rbValue2,value="no")
  tkgrid(tklabel(tt,text="K matrix Pre-processing:",background="white"),row=5,column=4,sticky="w")
  tkgrid(tklabel(tt,text="mean-centering",background="white"),row=5,column=5,sticky="e")
tkgrid(rb3,sticky="w",row=5,column=6,sticky="w")
  tkgrid(tklabel(tt,text="no centering",background="white"),row=6,column=5,sticky="e")
tkgrid(rb4,sticky="w",row=6,column=6,sticky="w")

######Pre-processing parameters for the Y matrix:
rb5 <- tkradiobutton(tt,background="white")
  rb6 <- tkradiobutton(tt,background="white")
rb7 <- tkradiobutton(tt,background="white")
  rb8 <- tkradiobutton(tt,background="white")
  rbValue3<-tclVar(preY)
  tkconfigure(rb5,variable=rbValue3,value="mc")
  tkconfigure(rb6,variable=rbValue3,value="uv")
tkconfigure(rb7,variable=rbValue3,value="pa")
  tkconfigure(rb8,variable=rbValue3,value="no")
  tkgrid(tklabel(tt,text="Y matrix Pre-processing:",background="white"),row=7,column=4,sticky="w")
  tkgrid(tklabel(tt,text="mean-centering",background="white"),row=7,column=5,sticky="e")
tkgrid(rb5,sticky="w",row=7,column=6,sticky="w")
tkgrid(tklabel(tt,text="mc(unit variance)",background="white"),row=8,column=5,sticky="e")
tkgrid(rb6,sticky="w",row=8,column=6,sticky="w")
tkgrid(tklabel(tt,text="mc(pareto)",background="white"),row=9,column=5,sticky="e")
tkgrid(rb7,sticky="w",row=9,column=6,sticky="w")
  tkgrid(tklabel(tt,text="no centering",background="white"),row=10,column=5,sticky="e")
tkgrid(rb8,sticky="w",row=10,column=6,sticky="w")

text2 <- tclVar(nox)
  textEntryWidget2 <- tkentry(tt,width=6,textvariable=text2,background="white")
tkgrid(tklabel(tt,text=" Orthogonal components",background="white"),row=11,column=4,sticky="W")
tkgrid(textEntryWidget2,row=11,column=5,sticky="e")

text3 <- tclVar(nrcv)
  textEntryWidget3 <- tkentry(tt,width=6,textvariable=text3,background="white")
tkgrid(tklabel(tt,text=" Cross Validations",background="white"),row=12,column=4,sticky="w")
tkgrid(textEntryWidget3,row=12,column=5,sticky="e")

######cross-validation Type

 rb9 <- tkradiobutton(tt,background="white")
  rb10 <- tkradiobutton(tt,background="white")
rb11 <- tkradiobutton(tt,background="white")
   rbValue4<-tclVar(cvType)
  tkconfigure(rb9,variable=rbValue4,value="nfold")
  tkconfigure(rb10,variable=rbValue4,value="mccv")
tkconfigure(rb11,variable=rbValue4,value="mccvb")
  
  tkgrid(tklabel(tt,text="Cross-validation Type:",background="white"),row=13,column=4,sticky="w")
  tkgrid(tklabel(tt,text="n-fold",background="white"),row=13,column=5,sticky="e")
tkgrid(rb9,sticky="w",row=13,column=6,sticky="w")
tkgrid(tklabel(tt,text="Monte Carlo",background="white"),row=14,column=5,sticky="e")
tkgrid(rb10,sticky="w",row=14,column=6,sticky="w")
tkgrid(tklabel(tt,text="MC class-balanced",background="white"),row=15,column=5,sticky="e")
tkgrid(rb11,sticky="w",row=15,column=6,sticky="w")

######PDF

  CopyToClip <- function()
  {
fileName<-tclvalue(tkgetSaveFile())
pdf(file = paste(fileName,".pdf"))
plotFunction1()
dev.off()
pdf(file = paste(fileName,"-b.pdf"))
plotFunction2()
dev.off()
  }
#########################################################

###### onOk & onCancel

 onok <- function()
 {
model<<-koplsModel(Ktr,Ytr.mod,1,nox,preProcK=preK,preProcY=preY)
tkdestroy(tt)
  }
onCancel<-function()
{
tkdestroy(tt)
stop("Stopped by the user")
}
CV.function<-function()
{
    kfunction <<- as.character(tclvalue(rbValue))
v.sigma<<- as.numeric(tclvalue(text1))
preK<<- as.character(tclvalue(rbValue2))
preY<<- as.character(tclvalue(rbValue3))
nox<<- as.numeric(tclvalue(text2))
nrcv<<- as.numeric(tclvalue(text3))
cvType<<-as.character(tclvalue(rbValue4))
Ktr<<-koplsKernel(data.mod,NULL,kfunction,v.sigma)
modelCV<<-koplsCV(Ktr,Ytr.mod,1,nox,nrcv,cvType,preProcK=preK,preProcY=preY,modelType='da')
tkrreplot(img1,plotFunction1())
tkrreplot(img2,plotFunction2())
}
#########################################################

copy.but <- tkbutton(tt,text="Copy to pdf",command=CopyToClip)
ok.but <- tkbutton(tt,text="Ok",command=onok)
cancel.but <- tkbutton(tt,text="Cancel",command=onCancel)

tkgrid(copy.but,row=15,column=1,sticky="e")
tkgrid(ok.but,row=15,column=2,sticky="e")
tkgrid(cancel.but,row=15,column=3,sticky="w")

tkbind(rb1,"<Button-1>" ,CV.function)
tkbind(rb2,"<Button-1>" ,CV.function)
tkbind(rb3,"<Button-1>" ,CV.function)
tkbind(rb4,"<Button-1>" ,CV.function)
tkbind(rb5,"<Button-1>" ,CV.function)
tkbind(rb6,"<Button-1>" ,CV.function)
tkbind(rb7,"<Button-1>" ,CV.function)
tkbind(rb8,"<Button-1>" ,CV.function)
tkbind(rb9,"<Button-1>" ,CV.function)
tkbind(rb10,"<Button-1>" ,CV.function)
tkbind(rb11,"<Button-1>" ,CV.function)
tkbind(textEntryWidget,"<Return>" ,CV.function)
tkbind(textEntryWidget2,"<Return>",CV.function)
tkbind(textEntryWidget3,"<Return>",CV.function)


tkwait.window(tt)
}
#############Second Graphical display


KOPL.MODEL.2<-function(Xs,Ys,density)#Graphical Interface
{
require(tcltk)#Packages
  require(tkrplot)
tt <- tktoplevel(background="white")#Main windows
  tkwm.title(tt,"KOPLS Model")

#######################################################

######Plot function
plotFunction3 <- function()
  {
params <- par(bg="white")
if (density==0)
{
koplsPlotScores(model, x=1, xsub="p", y=Ys, ysub="o",
 col= c("red","blue","green","black")[unclass(ycol)],main="Predictive Component")
}
if (density==1)
{
 plot(density(model$T[, 1]), ylab = "Density",sub= "tp,1", main="Predictive Component")
}
}

plotFunction1 <- function()
  {
params <- par(bg="white")
if (density==0)
{
koplsPlotScores(model, x=Xs, xsub="o", y=Ys, ysub="o", 
col= c("red","blue","green","black")[unclass(ycol)], main="Y-orthogonal component")
}
if (density==1)
{
 plot(density(model$To[, Xs]), ylab = "Density",sub= paste("to,",Xs), main="Y-orthogonal component")
}
}


########################################################
img2 <- tkrplot(tt,fun=plotFunction3,hscale=1.2,vscale=1.2)
img1 <- tkrplot(tt,fun=plotFunction1,hscale=1.2,vscale=1.2)
tkgrid(img2,columnspan=10,column=1,row=1)
  tkgrid(img1,columnspan=10,column=11,row=1)

SliderValue1 <- tclVar(Xs)
SliderValue2 <- tclVar(Ys)

rbValue <- tclVar(density)


plot.function2<-function()
{
Xs<<-as.numeric(tclvalue(SliderValue1))
Ys<<-as.numeric(tclvalue(SliderValue2))
density<<-as.numeric(tclvalue(rbValue))
tkrreplot(img1,plotFunction1())
tkrreplot(img2,plotFunction3())
}

plot.function2()

######PDF

  CopyToClip <- function()
  {
fileName<-tclvalue(tkgetSaveFile())
pdf(file = paste(fileName,".pdf"))
plotFunction1()
dev.off()
pdf(file = paste(fileName,"-b.pdf"))
plotFunction3()
dev.off()
  }
#########################################################

###### onOk & onCancel

 onok <- function()
 {
tkdestroy(tt)
  }
onCancel<-function()
{
tkdestroy(tt)
stop("Stopped by the user")
}

#########################################################
SliderValueLabel1 <- tklabel(tt,text=as.character(tclvalue(SliderValue1)),background="white")
slider1 <- tkscale(tt, from=1, to=nox,showvalue=F, variable=SliderValue1,
             resolution=1, orient="horizontal",bigincrement=1)

  SliderValueLabel2 <- tklabel(tt,text=as.character(tclvalue(SliderValue2)),background="white")
slider2 <- tkscale(tt, from=1, to=nox,showvalue=F, variable=SliderValue2,
             resolution=1, orient="horizontal",bigincrement=1)
tkconfigure(SliderValueLabel1,textvariable=SliderValue1)
tkconfigure(SliderValueLabel2,textvariable=SliderValue2)

rb1 <- tkradiobutton(tt,background="white")
  rb2 <- tkradiobutton(tt,background="white")
  rbValue<-tclVar(density)
  tkconfigure(rb1,variable=rbValue,value=0)
  tkconfigure(rb2,variable=rbValue,value=1)
  tkgrid(tklabel(tt,text="Score Plot",background="white"),row=2,column=5,sticky="e")
tkgrid(rb1,sticky="w",row=2,column=6,sticky="w")
  tkgrid(tklabel(tt,text="Density Plot",background="white"),row=3,column=5,sticky="e")
tkgrid(rb2,sticky="w",row=3,column=6,sticky="w")



tkgrid(tklabel(tt,text="X component", bg="white"),row=2,column=10,sticky="e")
tkgrid(tklabel(tt,text="Y component", bg="white"),row=3,column=10,sticky="e")
tkgrid(slider1,row=2,column=11,sticky="w")
tkgrid(slider2,row=3,column=11,sticky="w")
tkgrid(SliderValueLabel1,row=2,sticky="w",column=12)
tkgrid(SliderValueLabel2,row=3,sticky="w",column=12)
copy.but <- tkbutton(tt,text="Copy to pdf",command=CopyToClip)
ok.but <- tkbutton(tt,text="Ok",command=onok)
cancel.but <- tkbutton(tt,text="Cancel",command=onCancel)

tkgrid(copy.but,row=4,column=16,sticky="e")
tkgrid(ok.but,row=4,column=19,sticky="e")
tkgrid(cancel.but,row=4,column=20,sticky="w")

tkbind(slider1,"<ButtonRelease-1>" ,plot.function2)
tkbind(slider2,"<ButtonRelease-1>" ,plot.function2)
tkbind(rb1,"<Button-1>" ,plot.function2)
tkbind(rb2,"<Button-1>" ,plot.function2)



tkwait.window(tt)
}

require(kopls)
#data(koplsExample)
  info=datos$info#Load Data Set
  datos=datos$datos
  
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
  Met.model.1("PLS")
  if (m.model=="random")  #Random Selection
  {
Met.model("PLS")
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
manual.model("PLS")
  }
  sample.class<-vector()
 
  for (i in 1: length(clase))
  {
sample.class[which(info[,categoria]==clase[[i]])]<-i
  }
  Ytr<-matrix(0,ncol=length(clase),nrow=length(info[,categoria]))
 
  for (i in 1: length(clase))
  {
Ytr[which(info[,categoria]==clase[[i]]),i]<-1
  }
  ycol<-rep(0,length(info[,categoria]))
  for (i in 1: length(clase))
  {
ycol<-ycol+i*Ytr[,i]
  }
data<-as.data.frame(datos)
data<-as.matrix(data)

data.mod<-data[samp,]
data.val<-data[-samp,]
colnames(Ytr)<-letters[1:dim(Ytr)[2]]
Ytr.mod<-Ytr[samp,]
Yte<-Ytr[-samp,]
ycol<-ycol[samp]

kfunction <- "p"
v.sigma<- 3
preK<- "mc"
preY<- "mc"
nox<- dim(data.mod)[1]
nrcv<- 3
cvType<-"nfold"
Ktr<-koplsKernel(data.mod,NULL,kfunction,v.sigma)# Kernel
modelCV<-koplsCV(Ktr,Ytr.mod,1,nox,nrcv,cvType,preProcK=preK,preProcY=preY,modelType='da')
KOPL.MODEL()
KOPL.MODEL.2(1,1,0)

Predictive<-modelCV$koplsModel$T
colnames(Predictive)<-"Predictive"
Ortogonal<-modelCV$koplsModel$To
colnames(Ortogonal)<-paste("Ortogonal",c(1:dim(Ortogonal)[2]))
CVScores<-cbind(info[samp,],Predictive,Ortogonal)
showData2(CVScores,title="Scores")
TrueClass<-modelCV$da$trueClass
CrossValidationClass<-letters[modelCV$da$predClass]
CVobserved<-Ytr.mod
colnames(CVobserved)<-paste("observed",letters[1:dim(CVobserved)[2]])

CVpredicted<-modelCV$cv$Yhat
colnames(CVpredicted)<-paste("CVpredicted",letters[1:dim(CVpredicted)[2]])
CV.results<-cbind(info[samp,],TrueClass,CrossValidationClass,CVobserved,CVpredicted)
showData2(CV.results, title="KOPLS Cross Validation")
#koplsSensSpec(TrueClass,modelCV$da$predClass)
#Validacion
#v.sigma=160
#Ktr<-koplsKernel(data.mod,NULL,kfunction,v.sigma)
KteTr<-koplsKernel(data.val,data.mod,kfunction,v.sigma)
KteTe<-koplsKernel(data.val,NULL,kfunction,v.sigma)
modelPred<-koplsPredict(KteTr,KteTe,Ktr,model,rescaleY=TRUE)

## Visualize
#plot(modelPred$Yhat, Yte, xlab="Predicted", ylab="Observed")
#abline(v=0.5, col="Red", lty=2) ## Approximate decision bounda
observed<-Yte
colnames(observed)<-paste("observed",colnames(observed))
predicted<-modelPred$Yhat
colnames(predicted)<-paste("predicted",letters[1:dim(predicted)[2]])

Validation.results<-cbind(info[-samp,],observed,predicted)
      showData2(Validation.results, title="KOPLS Validation Results")

}

