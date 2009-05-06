Met.peak.detection <-
function(datos){
  tkconfigure(console,cursor="watch")

  picos<-datos$datos
  picos3<-as.matrix(picos)
  rownames(picos3)<-picos[,1]
  picos3<-picos3[,-1]
  Require("caMassClass")
  Met.model<- function()
{
dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,"Peak Detection")
SliderValue1 <- tclVar(2)
SliderValue2 <- tclVar(-0.95)
SliderValueLabel1 <- tklabel(dlg,text=as.numeric(tclvalue(SliderValue1)))
SliderValueLabel2 <- tklabel(dlg,text=as.numeric(tclvalue(SliderValue2)))
tkconfigure(SliderValueLabel1,textvariable=SliderValue1)
tkconfigure(SliderValueLabel2,textvariable=SliderValue2)


slider1 <- tkscale(dlg, from=0, to=6,showvalue=F, variable=SliderValue1,
                   resolution=0.2, orient="horizontal")
slider2 <- tkscale(dlg, from=-0.99, to=0.99,showvalue=F, variable=SliderValue2,
                   resolution=0.01, orient="horizontal")
tkgrid(tklabel(dlg,text="       "))
tkgrid(tklabel(dlg,text="Peak detection parameters"),sticky="w")
tkgrid(tklabel(dlg,text="       "))

tkgrid(tklabel(dlg,text="Signal-Noise Ratio : "),SliderValueLabel1,slider1)
tkgrid(tklabel(dlg,text="Threshold : "),SliderValueLabel2,slider2)
tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="       "))
  
onOK <- function()
  {
peak.parameters<<-list(a=as.numeric(tclvalue(SliderValue1)),b=as.numeric(tclvalue(SliderValue2)))
    tkdestroy(dlg)
}
  onCancel <- function()
  {
    ReturnVal <<- 0
    tkdestroy(dlg)
   }
  OK.but     <-tkbutton(dlg,text="   OK   ",command=onOK)
  Cancel.but <-tkbutton(dlg,text=" Cancel ",command=onCancel)
  tkgrid(tklabel(dlg,text="    "),OK.but,Cancel.but)
  tkgrid(tklabel(dlg,text="    "))
  tkraise(dlg)
  tkfocus(dlg)
  tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg);tkfocus(dlg)})
  tkwait.window(dlg)


  }
  Met.model()
  Try(msc.peaks.find(picos3,
SNR=peak.parameters$a,span=c(31,3), zerothresh = peak.parameters$b))

  Peaks <<-msc.peaks.find(picos3,
SNR=peak.parameters$a,span=c(31,3), zerothresh = peak.parameters$b)
  Peaks2<-Peaks
  colnames(Peaks2)[5]<-"Chemical Shift"
  showData2(Peaks2,title="Peaks detected")
  tkconfigure(console,cursor="arrow")


}

