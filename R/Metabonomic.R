Metabonomic <-
function() {
  closeAllConnections()
  Require("tcltk")
  Require ("tkrplot")
  memory.data<<-list()
  
  wfile <- ""
  R.base.dir <- system.file()
   Met.Console<- paste(R.base.dir,"/../../library/Metabonomic/Metabonomic_Console_O",sep="")
   Met.Console2<-paste(R.base.dir,"/../../library/Metabonomic/Metabonomic_Console_W",sep="")
 
  .exit<-function()
  {
closeAllConnections()
tkdestroy(console)
  }
  z<-file(Met.Console,"w+b")
  z2<-file(Met.Console2,"w+b")
  sink(file = z, append = TRUE, type = c("output", "message"),
     split = FALSE)
  sink(file = z2, append = TRUE, type ="message",
     split = FALSE)
  .write<-function()
  {
tkconfigure(txt2, state="normal")
tkconfigure(txt3, state="normal")
  chn <- tclopen(Met.Console, "r")
chn2 <- tclopen(Met.Console2, "r")
 tkdelete(txt2,"0.0","100000.0")
tkdelete(txt3,"0.0","100000.0")
  tkinsert(txt2, "end", tclvalue(tclread(chn)))
  tkinsert(txt3, "end", tclvalue(tclread(chn2)))
tkconfigure(txt2, state="disabled")
tkconfigure(txt3, state="disabled")
   }
  save <- function() 
  {
    file <- tclvalue(tkgetSaveFile(
      initialfile=tclvalue(tclfile.tail(wfile)),
      initialdir=tclvalue(tclfile.dir(wfile))))
  if (!length(file)) return()
  chn <- tkopen(file, "w")
  tkputs(chn, tclvalue(tkget(txt,"0.0","end")))
  tclclose(chn)
  wfile <<- file
  }

  Met.Help1<-function()
  {
 tkmessageBox(title="Information",message="Metabonomic version 3.1.3 (2009-04-14)
Copyright(C)2009 Instituto de Estudios Biofuncionales (UCM)",icon="info",type="ok")
  }

  Met.Help2<-function()
  {
   .write()
   print(eval(?Metabonomic))
   .write()
  }
  Require("tkrplot")
  load <- function() 
  {
  file <- tclvalue(tkgetOpenFile())
  if (!length(file)) return()
  chn <- tclopen(file, "r")
 
  tkinsert(txt, "0.0", tclvalue(tclread(chn)))
  tclclose(chn)
  wfile <<- file
  }
  run <- function() 
  {
  code <- tclvalue(tkget(txt,"0.0","end"))
  e <- try(parse(text=code))
  if (inherits(e, "try-error")) 
{
   tkmessageBox(message="Syntax error",icon="error")
  return()
  }
   #cat("Executing from script window:",
   #   "-----", code, "result:", sep="\n")
   .write()
   print(eval(e))
   .write()
  }
  Met.stop <- function()
  {
tkconfigure(console,cursor="arrow")
stop("  Stop  ")
  }
  console <<- tktoplevel()
  
  ico.undo<- paste(R.base.dir,"/../../library/Metabonomic/51.gif",sep="")
  ico.redo<- paste(R.base.dir,"/../../library/Metabonomic/52.gif",sep="")
  ico.show<- paste(R.base.dir,"/../../library/Metabonomic/62.gif",sep="")
  ico.launch<- paste(R.base.dir,"/../../library/Metabonomic/42.gif",sep="")
  ico.exit<-paste(R.base.dir,"/../../library/Metabonomic/493.gif",sep="")
  ico.erease<- paste(R.base.dir,"/../../library/Metabonomic/43.gif",sep="")
  ico.stop<- paste(R.base.dir,"/../../library/Metabonomic/44.gif",sep="")


  image1<-tkimage.create("photo",file=ico.undo,height=50, width=50)
  image2<-tkimage.create("photo",file=ico.redo,height=50, width=50)
  image3<-tkimage.create("photo",file=ico.show,height=50, width=50)
  image4<-tkimage.create("photo",file=ico.launch,height=50, width=50)
  image5<-tkimage.create("photo",file=ico.exit,height=50, width=50)
  image6<-tkimage.create("photo",file=ico.erease,height=50, width=50)
  image7<-tkimage.create("photo",file=ico.stop,height=50, width=50)

  Undo.but<-tkbutton(console,image=image1,command=Met.undo)
  Redo.but<-tkbutton(console,image=image2,command=Met.redo)
  Show.but<-tkbutton(console,image=image3,command=Met.Show) 
  Data.but<-tkbutton(console,image=image5,command=.exit)
  Launch.but<-tkbutton(console,image=image4,command=run)
  Erease.but<-tkbutton(console,image=image6,command=function() tkdelete(txt,"0.0","100000.0"))
  Stop.but<-tkbutton(console,image=image7,command=Met.stop)

  tkgrid(Undo.but, Redo.but, Show.but,Launch.but,Erease.but,Stop.but,Data.but,columnspan=1,sticky="w")


  font.txt <- tkfont.create(family="times",size=12)
  scr <- tkscrollbar(console, repeatinterval=5,
         command=function(...)tkyview(txt,...))
  txt <- tktext(console, width=130,height=15,font=font.txt)
  tkgrid(txt,scr,columnspan=100)
  tkgrid.configure(scr,sticky="ns")

  tkgrid(tklabel(console,text="    "))
  scr2 <- tkscrollbar(console, repeatinterval=5,
         command=function(...)tkyview(txt2,...))
  scr3 <- tkscrollbar(console, repeatinterval=5,
         command=function(...)tkyview(txt3,...))
  txt2 <- tktext(console,bg="#d8d8d8", width=65,height=15,font=font.txt,fg="blue")
  txt3 <- tktext(console,bg="#d8d8d8", width=65,height=15,font=font.txt,fg="red")
  tkgrid(tklabel(console,text="  Output  "),columnspan=50)
  tkgrid(tklabel(console,text="  Warning  "),row=3,column=51,columnspan=50)

  tkgrid(txt2,columnspan=50)
  tkgrid(scr2,column=51,row=4)
  tkgrid(txt3,column=52,row=4,columnspan=48)
  tkgrid(scr3,column=101,row=4)
  tkgrid.configure(scr2,sticky="ns")
  tkgrid.configure(scr3,sticky="ns")


  font.menu <- tkfont.create(family="times",size=48)

  topMenu <- tkmenu(console)
  tkconfigure(console, menu=topMenu)

  FileMenu <- tkmenu(topMenu, tearoff=FALSE)
  tkadd(FileMenu,"command",label="Load Data File",
command=function() Met.Load.Data())
  tkadd(FileMenu,"command",label="Import Bruker File",
    command=function() Import.data())
  tkadd(FileMenu,"command",label="Save Data File",
    command=function() Met.Save.Data())
  tkadd(FileMenu,"command",label="Delete Spectrum",
    command=function() Met.delete())
  tkadd(FileMenu,"command",label="Manual Cut",
    command=function() Manual.cut())
  tkadd(FileMenu,"command",label="Category Selection",
command=function() Met.Selection(datos,externa))
  tkadd(FileMenu,"command",label="Exit",command=.exit)      
  tkadd(topMenu, "cascade", label="File",
      menu=FileMenu)

  ScriptMenu <- tkmenu(topMenu, tearoff=FALSE)
  tkadd(ScriptMenu, "command", label="Load Script",
      command=load)
  tkadd(ScriptMenu, "command", label="Save Script",
      command=save)
  tkadd(ScriptMenu, "command",label="Launch the Script",
      command=run)   
  tkadd(topMenu, "cascade", label="Script",
      menu=ScriptMenu)

  EditMenu <- tkmenu(topMenu, tearoff=FALSE)
  tkadd(EditMenu, "command", label="Show Data",
      command=Met.Show)
  tkadd(EditMenu, "command", label="Undo",
      command=Met.undo)    
  tkadd(EditMenu, "command", label="Redo",
      command=Met.redo)   
  tkadd(topMenu, "cascade", label="Edit",
      menu=EditMenu)    

  Pretratamiento <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(Pretratamiento,"command",label="Peak Detection",
    command=function() Met.peak.detection(datos))
  tkadd(Pretratamiento,"command",label="Peak Aligment. Biomarkers",
    command=function() Met.Aligm(datos,Peaks))
  tkadd(Pretratamiento,"command",label="Binning",
    command=function() Met.Binning(datos))
  tkadd(Pretratamiento,"command",label="Normalization",
    command=function() Met.Norm(datos))
  tkadd(Pretratamiento,"command",label="Baseline (FTICRMS)",
    command=function() Baseline.Correction())
  tkadd(Pretratamiento,"command",label="Metabolites order by discriminantion quality",
    command=function() Met.Order(datos))
  tkadd(Pretratamiento,"command",label="Metabolites Selection",
    command=function()Met.Metabolites(datos, externa))
  tkadd(Pretratamiento,"command",label="Outliers",
command=function() Met.Outliers(datos))
  tkadd(topMenu,"cascade",label="Preprocessing",menu=Pretratamiento) 

  Analisis <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(Analisis,"command",label="Chemical shift Region display",
    command=function() Met.B.STAT(datos))
  tkadd(Analisis,"command",label="PCA",
    command=function() Met.PCA(datos))
  tkadd(Analisis,"command",label="LDA",
command=function() Met.LDA(datos,externa))
  PLSMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(PLSMenu,"command",label="PLS",
command=function() Met.PLS1(datos,externa))
  tkadd(PLSMenu,"command",label="PLS with graphics",
command=function() Met.PLS2(datos,externa))
  #tkadd(PLSMenu,"command",label="PLS with 3D graphics ",
  #command=function() Met.PLS3D(datos,externa))
  tkadd(Analisis,"cascade",label="Partial Least Squares",menu=PLSMenu)
  tkadd(Analisis,"command",label="KNN",
command=function() Met.KNN(datos,externa))
  NNMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(NNMenu,"command",label="Neural Network(Single hidden layer)",
command=function() Met.NN1(datos,externa))
  tkadd(NNMenu,"command",label="Neural Network(Multiple hidden layer)",
command=function() Met.NN2(datos,externa))
  tkadd(Analisis,"cascade",label="Neural Network",menu=NNMenu)
  tkadd(topMenu,"cascade",label="Metabonomic Analysis",menu=Analisis)   

  SpectMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(SpectMenu,"command",label="Original Spectrum",
command=function() Met.spectrum(xCoords=memory.data[[1]]))
  tkadd(SpectMenu,"command",label="Current Spectrum",
command=function() Met.spectrum(xCoords=datos))
  tkadd(topMenu,"cascade",label="Spectrum",menu=SpectMenu)

  HelpMenu <- tkmenu(topMenu,tearoff=FALSE)
 tkadd(HelpMenu,"command",label="Help",
command=function() Met.Help2())
  tkadd(HelpMenu,"command",label="About",
command=function() Met.Help1())
 tkadd(topMenu,"cascade",label="Help",menu=HelpMenu)
  
 
  tkwm.title(console,"Metabonomic")     
  tkfocus(console)   
  tkbind(txt, "<Motion>",.write)
  tkbind(Undo.but, "<Motion>",.write)
  tkbind(Redo.but, "<Motion>",.write)
  tkbind(Show.but, "<Motion>",.write)
  tkbind(Data.but, "<Motion>",.write)
  tkbind(Launch.but, "<Motion>",.write)
  tkbind(Erease.but, "<Motion>",.write)

  tkbind(topMenu, "<Motion>",.write)

  tkbind(txt, "<Control-Return>",run)
}

