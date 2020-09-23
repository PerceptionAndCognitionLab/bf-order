loadOrientationData=function(){
  upper=5
  lower=.2
  dat=read.table(url('https://raw.githubusercontent.com/PerceptionAndCognitionLab/ctx-inhibition/public/shared/rouder.2010.orientation.dat'))
  colnames(dat)=c('sub','block','trial','trialB','cond','resp','rt','acc')

  dat$mycond=NA
  dat$mycond[dat$cond==5]=1
  dat$mycond[dat$cond==0]=2
  dat$mycond[dat$cond==4]=3
  dat$mycond[dat$cond==1]=4
  dat$mycond[dat$cond==3]=5
  dat$mycond[dat$cond==2]=6


  dat$sub=as.integer(as.factor(dat$sub))
  I=length(levels(as.factor(dat$sub))) #Number of Subs.


  #Wrong-button responses
  wrong.button=dat$resp>1
  error=dat$acc==0
  stop=wrong.button | dat$trialB==0 |error
  after.stop=c(0,stop)[1:length(stop)]
  a=dat$rt<lower
  b=dat$rt>upper
  dont.use=stop | after.stop |a |b
  clean=dat[!dont.use,]

  ct=clean[clean$block>2,]
  out=data.frame(ct$sub,ct$cond,ct$rt)
  colnames(out)=c("sub","cond","rt")
  return(out)}

codeStrength=function(dat){
  out=dat
  out$cond=rep(0,length(dat$cond))
  out$cond[dat$cond==0 | dat$cond==5]=1
  out$cond[dat$cond==2 | dat$cond==3]=2
  good=out$cond>0
  return(out[good,])}  
  

codeBias=function(dat){
  out=dat
  out$cond=rep(0,length(dat$cond))
  out$cond[dat$cond==2]=2
  out$cond[dat$cond==3]=1
  good=out$cond>0
  return(out[good,])}  

