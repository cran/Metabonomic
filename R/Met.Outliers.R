`Met.Outliers` <-
function(datos){
tkconfigure(console,cursor="watch")
ReturnVal <- tkmessageBox(title="Outliers",message="Outliers identification by Mehalanobis distance. Launched in the R console.",icon="info",type="ok")
info=datos$info  #Data tratament
info2=datos$info
datos2=datos$datos
datos=datos$datos
dimnames(datos)[[1]]=as.character(datos[,1])
datos=datos[,-1] 
colnames(datos)=info$Nombres
datos<-t(datos)
dat.pc<-prcomp(datos,scale=FALSE,center=TRUE,tol = sqrt(.Machine$double.eps))
summary(dat.pc,loadings=TRUE)
Principales<-predict(dat.pc)  #PCA
Require("robustbase")
Require("mvoutlier")
windows()
tolEllipsePlot(Principales[,1:2], m.cov = covMcd(Principales[,1:2]), 
   cutoff = NULL, id.n = NULL,
               classic = TRUE, tol = 1e-07,
               xlab = "PC1", ylab = "PC2",
               main = "Tolerance ellipse (97.5%)",
               txt.leg = c("robust", "classical"),
               col.leg = c("red", "blue"),
               lty.leg = c("solid","dashed"))
windows()
covPlot(Principales[,1:2],
      which = c("all", "dd", "distance", "qqchi2",
               "tolEllipsePlot", "screeplot"),classic = FALSE,
     labels.id = rownames(Principales), cex.id = 0.75,
     label.pos = c(4,2), tol = 1e-7,)
decision<-"yes"
i=0
ReturnVal<-vector()
while (decision!="no")
{
i=i+1
Require("tcltk")
ReturnVal[i] <- Met.modalDialog("Outliers","Would you like to delete any sample? Introduce number of sample.","")
ReturnVal[i]=as.numeric(ReturnVal[i])+1
decision<-tclvalue(tkmessageBox(message="Delete another sample??",icon="question",type="yesno",default="yes"))
}
lista<-(ReturnVal[0:i])
if (lista[1]!=0)
{
datos<-datos2[,-lista]
info2<-info2[-lista,]
}
datos<<-(list(datos=datos,info=info2))
memory<<-memory+1
memory.data[[memory]]<<-list(generation=memory,datos=datos$datos,
  info=datos$info)     #For undo function
tkconfigure(console,cursor="arrow")


}

