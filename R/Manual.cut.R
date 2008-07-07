`Manual.cut` <-
function()
{
  tkconfigure(console,cursor="watch")

  memory<<-memory+1
  dat<-datos$datos

  pos<-list()

  cut<-function()
  {
dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,"Manual Cut")
SliderValue1 <- tclVar("0")
SliderValue2 <- tclVar("10")
SliderValueLabel1 <- tklabel(dlg,text=as.character(tclvalue(SliderValue1)))
SliderValueLabel2 <- tklabel(dlg,text=as.character(tclvalue(SliderValue2)))
tkconfigure(SliderValueLabel1,textvariable=SliderValue1)
tkconfigure(SliderValueLabel2,textvariable=SliderValue2)
SliderValue3 <- tclVar("5")
SliderValue4 <- tclVar("5.20")
SliderValueLabel3 <- tklabel(dlg,text=as.character(tclvalue(SliderValue3)))
SliderValueLabel4 <- tklabel(dlg,text=as.character(tclvalue(SliderValue4)))
tkconfigure(SliderValueLabel3,textvariable=SliderValue3)
tkconfigure(SliderValueLabel4,textvariable=SliderValue4)


slider1 <- tkscale(dlg, from=round(dat[1,1],3), to=round(dat[dim(dat)[1],1],3),showvalue=F, variable=SliderValue1,
                   resolution=0.001, orient="horizontal",length=300,bigincrement=0.001)
slider2 <- tkscale(dlg, from=round(dat[1,1],3), to=round(dat[dim(dat)[1],1],3),showvalue=F, variable=SliderValue2,
                   resolution=0.001, orient="horizontal",length=300)
slider3 <- tkscale(dlg, from=round(dat[1,1],3), to=round(dat[dim(dat)[1],1],3),showvalue=F, variable=SliderValue3,
                   resolution=0.001, orient="horizontal",length=300,bigincrement=0.001)
slider4 <- tkscale(dlg, from=round(dat[1,1],3), to=round(dat[dim(dat)[1],1],3),showvalue=F, variable=SliderValue4,
                   resolution=0.001, orient="horizontal",length=300)
cb <- tkcheckbutton(dlg)
cbValue <- tclVar("0")
tkconfigure(cb,variable=cbValue)

tkgrid(tklabel(dlg,text="       "))
tkgrid(tklabel(dlg,text="Select the spectral region to analyze"),sticky="w")
tkgrid(tklabel(dlg,text="       "))
tkgrid(tklabel(dlg,text="                     From : "),SliderValueLabel1,slider1)
tkgrid(tklabel(dlg,text="                     To : "),SliderValueLabel2,slider2)
tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="Delete any spectral region "),cb)
tkgrid(tklabel(dlg,text="       "))
 tkgrid(tklabel(dlg,text="                     From : "),SliderValueLabel3,slider3)
tkgrid(tklabel(dlg,text="                     To : "),SliderValueLabel4,slider4)

onOK <- function()
  {
         if(as.numeric(tclvalue(SliderValue1))>as.numeric(tclvalue(SliderValue2))){
xlim<<-c(as.numeric(tclvalue(SliderValue1)),as.numeric(tclvalue(SliderValue2)))
   }
   if(as.numeric(tclvalue(SliderValue1))<as.numeric(tclvalue(SliderValue2))){
xlim<<-c(as.numeric(tclvalue(SliderValue2)),as.numeric(tclvalue(SliderValue1)))
    }
cbVal <- as.character(tclvalue(cbValue))
   if (cbVal=="1")
{
if(as.numeric(tclvalue(SliderValue3))>as.numeric(tclvalue(SliderValue4))){
xlim2<<-c(as.numeric(tclvalue(SliderValue3)),
as.numeric(tclvalue(SliderValue4)))
   }
   if(as.numeric(tclvalue(SliderValue3))<as.numeric(tclvalue(SliderValue4))){
xlim2<<-c(as.numeric(tclvalue(SliderValue4)),
as.numeric(tclvalue(SliderValue3)))
    }
}
if (cbVal=="0")
{
xlim2<<-0
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
  tkgrid(OK.but,Cancel.but,sticky="e")
  tkgrid(tklabel(dlg,text="    "))
  tkraise(dlg)
  tkfocus(dlg)
tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg);tkfocus(dlg)})
  tkwait.window(dlg)
  }
  cut()
  pos$A<-which(round(dat,3) == round(xlim,3)[2])[1] 
  pos$B<-which(round(dat,3) == round(xlim,3)[1])[1]
  dat2<-dat[-c(1:pos$A,pos$B:dim(dat)[1]),]
  if (xlim2[1]!="0")
  {
pos$C<-which(round(dat,3) == round(xlim2,3)[2])[1]
pos$D<-which(round(dat,3) == round(xlim2,3)[1])[1]
dat2<-dat[-c(1:pos$A,pos$C:pos$D,pos$B:dim(dat)[1]),]
  }
  showData2(dat2,title="Import Data")
  datos<<-list(datos=dat2,info=datos$info)
  memory.data[[memory]]<<-list(generation=memory,datos=dat2,info=datos$info)
  tkconfigure(console,cursor="arrow")

}

