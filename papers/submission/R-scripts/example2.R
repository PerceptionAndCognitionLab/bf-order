# library('diagram')

#Code for Symbolic Distance 
layout(matrix(nrow=3,ncol=6,c(1,1,2,2,3,3,4,4,4,5,5,5,6,6,6,7,7,7),byrow=T))
par(mar=c(.05,.05,.05,.05),cex=1.1,mgp=c(2,.3,0))
myCol=c('lightblue','lavender','lightgreen')

#A
names=c(4:2,6:8)
o=c(1,4,2,5,3,6)
M=matrix(nrow=length(names),byrow=F,ncol=length(names),data=0)
M[1,2]=M[2,3]=''
M[4,5]=M[5,6]=''
plotmat(t(M[o,o]),pos=c(2,2,2),name=names[o],curve=0,box.type="round",box.size=.07,box.prop=1,box.col=rep(myCol,each=2),arr.length=0)
axis(2,at=c(.2,.8),lab=c("Faster","Slower"),cex=1.3,line=-1.25, shadow.size = 0)
# box()
mtext(side=3,adj=.5,line=-2,"A",cex=1.4)

#B
names=c(2:4,6:8)
M=matrix(nrow=length(names),byrow=F,ncol=length(names),data=0)
M[1,2]=M[2,3]=''
M[4,5]=M[5,6]=''
pos=matrix(nrow=6,ncol=2)
pos[,1]=c(.1,.4,.7,.3,.6,.9)
pos[,2]=c(.65,.65,.65,.35,.35,.35)
plotmat(M,pos,name=names,curve=0,box.type="round",box.size=.04,box.prop=1,box.col=c(rev(myCol),myCol),arr.length=0,arr.type='triangle'
        , shadow.size = 0)
# box()
mtext(side=3,adj=.5,line=-2,"B",cex=1.4)

#C
names=c(2:4,8:6)
o=c(1,4,2,5,3,6)
M=matrix(nrow=length(names),byrow=F,ncol=length(names),data=0)
M[1,2]=M[2,3]=''
M[4,5]=M[5,6]=''
plotmat(t(M[o,o]),pos=c(2,2,2),name=names[o],curve=0,box.type="round",box.size=.06,box.prop=1,box.col=myCol[c(3,3,2,2,1,1)],arr.length=0
        , shadow.size = 0)
# box()
mtext(side=3,adj=.5,line=-2,"C",cex=1.4)



#####################################

myCol=c("paleturquoise","peachpuff")
names=rep(c("C","I"),10)
pos=matrix(ncol=2,nrow=length(names))
pos[,1]=rep(seq(.12,.9,length=10),each=2)
pos[,2]=c(.75,.25)[rep(2:1,10)]
M=matrix(nrow=length(names),byrow=F,ncol=length(names),data=0)
myLines=cbind(seq(1,20,2),seq(2,20,2))
M[myLines]=''
G=plotmat(M,pos,name=names,curve=0,box.type="circle",box.size=.04,box.prop=1,box.col=myCol,arr.length=0,arr.type='triangle'
          , shadow.size = 0)
axis(2,at=c(.2,.8),lab=c("Faster","Slower"),cex=1.3,line=-1.25)
# box()
mtext(side=3,adj=.5,line=-2,"F",cex=1.4)

map=c(1:20)
map[13]=14
map[14]=13
map[17]=18
map[18]=17

myCol=rep(c("paleturquoise","peachpuff"),10)


pos=matrix(ncol=2,nrow=length(names))
pos[,1]=rep(seq(.12,.9,length=10),each=2)
pos[,2]=c(.75,.25)[rep(2:1,10)]
M=matrix(nrow=length(names),byrow=F,ncol=length(names),data=0)
myLines=cbind(seq(1,20,2),seq(2,20,2))
M[myLines]=''
G=plotmat(M,pos,name=names[map],curve=0,box.type="circle",box.size=.04,box.prop=1,box.col=myCol[map],arr.length=0,arr.type='triangle'
          , shadow.size = 0)
#axis(2,at=c(.2,.8),lab=c("Faster","Slower"),cex=1.3,line=-1.25)
# box()
mtext(side=3,adj=.5,line=-2,"G",cex=1.4)




