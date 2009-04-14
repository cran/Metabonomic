`Import.data` <-
function()
{
require(tcltk)#Packages
  require(tkrplot)
require(waved)
memory<<-1
Fid<-function(){
k<<-k+1
require(tcltk)
 file<-tclvalue(tkgetOpenFile())
a<-unlist(strsplit(file,"/"))
  fin<-length(a)-1
  b<-paste(a[1],sep="/")
  for(i in 2:fin)
  {
b<-paste(b,a[i],sep="/")
  }
setwd(b)
  characteristic<-paste(b,"acqu",sep="/") #Load the information
  ficha<-readLines(con = characteristic, n = -1, ok = TRUE, warn = TRUE,
          encoding = "Latin-1")
is(ficha)
ficha2<-(strsplit(ficha," "))
ficha2<-unlist(ficha2)
n1<-which(ficha2=="##$BF1=")+1
SF<-as.numeric(ficha2[n1])
  n2<-which(ficha2== "##$SW_h=")+1
 SWHz<-as.numeric(ficha2[n2])

S<-readBin(file, what="int",70000, size = 4, signed = T,
        endian ="swap")
td<-length(S)
si=2*td
rawR<-S[seq(from=1, to=td,by=2)]
fidRaw<-rawR
rawI<-S[seq(from=2, to=td,by=2)]
#plot(c(1:(td/2)),Re(rawR),"l")
mediar<-mean(as.integer(rawR[c((3*length(rawR)/4):length(rawR))]),na.rm = TRUE)
mediai<--mean(as.integer(rawI[c((3*length(rawR)/4):length(rawR))]),na.rm = TRUE)
rawR<-rawR-mediar
rawI<-rawI-mediai
raw<-complex(real=rawR,imaginary=rawI)
raw2<-raw
raw2[c(length(raw):si)]<-0
#plot(c(1:si),Re(raw2),"l")

t<-seq(from=1, to=td/2, length.out=si)
XScaleHz<-(SWHz*t/(td/2))
PPM<-XScaleHz/SF


TF<-fftshift(fft((raw2))) #Transformada de Fourier
#plot(fid.out$PPM,Re(TF),"l", xlim=c(fid.out$PPM[length(x$PPM)],0),xlab="PPM", ylab="")

fid.out[[k]]<-list(fidRaw=fidRaw,fid=raw2,SF=SF, SWHz=SWHz,XScaleHZ=XScaleHz,
 PPM=PPM,spectrum=TF,t=t, si=si )
return(fid.out)
}
#Import Bruker FID
Bruker<-tclvalue(tkmessageBox(message="Import Bruker FID?",icon="question",type="yesno",default="yes"))
if (Bruker=="yes")
{
fid.out<<-list()
k<-0
answer<-"yes"
while (answer=="yes")
{
fid.out<-Fid()
answer<-tclvalue(tkmessageBox(message="Do you want to import another spectrum?",icon="question",type="yesno",default="yes"))
}
file.out<<-fid.out
tclvalue(tkmessageBox(message="Bruker spectra loaded",icon="question",type="ok",default="ok"))
}

Phase.correction()


}

