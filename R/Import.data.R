Import.data <-
function()
{
  require(tcltk)#Packages
  require(tkrplot)
  require(waved)
  memory<<-1
  Fid<-function()
  {
k<<-k+1

file<-tclvalue(tkgetOpenFile())
a<-unlist(strsplit(file,"/"))
fin<-length(a)-1
b<-paste(a[1],sep="/")
for(i in 2:fin)
  {
b<-paste(b,a[i],sep="/")
  }
setwd(b)
  characteristic<-paste(b,"acqus",sep="/") #Load the information
  ficha<-readLines(con = characteristic, n = -1, ok = TRUE, warn = TRUE,
          encoding = "Latin-1")
#is(ficha)
ficha2<-(strsplit(ficha,"="))
ficha2<-unlist(ficha2)
n1<-which(ficha2=="##$BF1")+1
SF<-as.numeric(ficha2[n1])
n2<-which(ficha2== "##$SW_h")+1
 SWHz<-as.numeric(ficha2[n2])
n3<-which(ficha2=="##$AQ_mod")+1
Dig<-as.numeric(ficha2[n3])
n4<-which(ficha2=="##$BYTORDA")+1
bytorda<-as.numeric(ficha2[n4])
n5<-which(ficha2=="##$DSPFVS")+1
DSPFVS<-as.numeric(ficha2[n5])
n6<-which(ficha2=="##$DECIM")+1
DECIM<-as.numeric(ficha2[n6])

if (bytorda==1)
S<-readBin(file, what="int",70000, size = 4, signed = T,endian ="big")
if (bytorda==0)
S<-readBin(file, what="int",70000, size = 4, signed = T,endian ="little")


#plot(S)
td<-length(S)
rawR<-S[seq(from=1, to=td,by=2)]
rawI<-S[seq(from=2, to=td,by=2)]
if (Dig==3)
{
if (DSPFVS>=10 & DSPFVS<=15 & DSPFVS!=11)
{
DECIM.VECTOR<-c(2,3,4,6,8,12,16,24,32,48,64,96,128,192,
256,384,512,768,1024,1536,2048)
DSPFVS.VECTOR<-c(10,12:15)
pos.a<-which(DSPFVS==DSPFVS.VECTOR)
pos.b<-which(DECIM.VECTOR==DECIM)
delay<-array(c(179, 201, 533, 709, 1097, 1449, 2225, 2929,
 4481, 5889, 8993, 11809, 18017, 23649, 36065, 47329,
72161, 94689, 144353, 189409, 288737,184, 219, 384,
602, 852, 1668, 2292, 3368, 4616, 6768, 9264, 13568,
18560, 27392, 36992, 55040, 73856, 110336, 147584,
220928, 295040, 11, 17, 23, 35, 47, 71, 95, 143, 191,
287, 383, 575, rep(0,9),60, 90, 118, 179, 244, 360, 492, 724,
980, 1444, 1958, 2886, 3912, 5768, 7820, 11532,rep(0,5),
58,110, 0, 152, 202, 318, 418, 642, 842, 1290, 1690,
2586, 3386, rep(0,8)), c(21,5))
delay.point<-round(delay[pos.b,pos.a]/2/DECIM)
}
if (DSPFVS>=20 & DSPFVS<=23)
{
n7<-which(ficha2=="##$GRPDLY")+1
delay.point<-as.numeric(ficha2[n7])*2
}
if(DSPFVS<10 | DSPFVS>23 |DSPFVS==11 | DSPFVS==16 | DSPFVS==17 | DSPFVS==18 | DSPFVS==19)
{
tclvalue(tkmessageBox(message="Unknown format",icon="warning",type="ok"))
stop("unknown format")
}
rawR<-rawR[-c(1:(delay.point))]
rawI<-rawI[-c(1:(delay.point))]
td<-(td-2*(delay.point))
}
si=2*td
fidRaw<-rawR
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

