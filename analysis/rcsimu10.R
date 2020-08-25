start.time <- Sys.time()

require(dplyr); require(survival);

logN.multi<-function(para, simu.obs.func,sep)
{
  para.beta<-para[1:sep];
  para.gamma<-para[-c(1:sep)];
  yi.star<-simu.obs.func[[1]];  x.val.multi<-simu.obs.func[[2]]; z.val.multi<-simu.obs.func[[3]]; delt.val<-simu.obs.func[[4]];
  wi<- (yi.star-x.val.multi%*%para.beta)/(exp(z.val.multi%*%para.gamma));
  val.loglike<-sum(delt.val*wi^2/2)+sum(delt.val*(z.val.multi%*%para.gamma))-sum((1-delt.val)*log(1-pnorm(wi)+10^(-9)))
  return(val.loglike);
}
logN.comp.multi<-function(para,simu.obs.func,sep)
{
  para.beta<-para[1:sep];
  para.gamma<-para[-c(1:sep)];
  yi.star<-simu.obs.func[[1]];  x.val.multi<-simu.obs.func[[2]]; z.val.multi<-simu.obs.func[[3]]; delt.val<-simu.obs.func[[4]];
  wi<- (yi.star-x.val.multi%*%para.beta)/(exp(z.val.multi%*%para.gamma));
  val.loglike<-sum(wi^2/2)+sum(z.val.multi%*%para.gamma);
  return(val.loglike);
}

simu.rc.normal.multi<-function(n.obs=10^3, fun.type='normal',
                               beta.val.multi=c(30,1,2,rep(0,3)),gam.val.multi=c(-5,3,4,rep(0,4)), 
                               cut.off=40)
{
  nrow.x<-length(beta.val.multi)-1;
  nrow.z<-length(gam.val.multi)-1;
  x.val.multi<-cbind(rep(1,n.obs),matrix(rnorm(n.obs*nrow.x,sd=3),nrow=n.obs));
  z.val.multi<-cbind(rep(1,n.obs),matrix(runif(n.obs*nrow.z),nrow=n.obs));
  
  sigma.val<-exp(z.val.multi%*%gam.val.multi);
  if(fun.type %in% c('normal','logis','unif','cauchy'))
    epsi<-switch(fun.type, 'normal'=unlist(lapply(c(1:n.obs), function(x) rnorm(1,sd=sigma.val[x]))),
                 'logis'=unlist(lapply(c(1:n.obs), function(x) rlogis(1)*sigma.val[x]*3^.5/pi)),
                 'cauchy'=unlist(lapply(c(1:n.obs), function(x) rcauchy(1)*sigma.val[x])),
                 'unif'=unlist(lapply(c(1:n.obs), function(x) (runif(1)-0.5)*sigma.val[x]*12^.5))
    ) 
  else{print('The distribution is not part of the simulation yet'); stop};
  #epsi<-unlist(lapply(c(1:n.obs), function(x) rnorm(1,sd=sigma.val[x])));## Make it proportional to sigma.val. Noticing that the var od espi is not sigma.val^2
  
  yi<-x.val.multi%*%beta.val.multi+epsi;
  
  yi.star<-unlist(lapply(yi,function(x) min(x,cut.off)));
  delt.val<-(yi<cut.off);
  simu.obs<-list(yi.star,x.val.multi,z.val.multi,delt.val,yi);
  names(simu.obs)<-c('yi.star','X','Z','delta','yi');
  
  rc.mle<-nlminb(start=rep(0,nrow.x+nrow.z+2),logN.multi,simu.obs.func=simu.obs,sep=nrow.x+1,control=list(eval.max=10^6,iter.max=10^6))$par;
  #rc.mle<-optim(par=rep(0,nrow.x+nrow.z+2),method="BFGS",logN.multi, simu.obs.func=simu.obs,sep=nrow.x+1,control=list(maxit=10^6));
  com.mle<-nlminb(start=rep(0,nrow.x+nrow.z+2),logN.comp.multi,simu.obs.func=simu.obs,sep=nrow.x+1,control=list(eval.max=10^6,iter.max=10^6))$par;
  #com.mle<-optim(par=rep(0,nrow.x+nrow.z+2),method="BFGS",logN.comp.multi, simu.obs.func=simu.obs,sep=nrow.x+1,control=list(maxit=10^6));
  
  return(list(rc.mle,com.mle,simu.obs))
}

## @knitr BJE.try0

obj.func.bje<-function(para,sep,flist.data, which.obj=1)
{#flist.data is a list with yi.star, X, Z, and delta, example:flist.data=out.normal.multi[[3]]
  #para<-c(29.99,1.01,2.01,rep(0.001,3),-4.9,3.01,4.01,rep(0.001,4))
  fyi.star<-flist.data$yi.star;
  fX<-flist.data$X; dim.beta<-dim(fX)[2];
  fZ<-flist.data$Z; dim.beta<-dim(fZ)[2];
  fdelta<-flist.data$delta;
  para.beta<-para[1:sep];
  para.gamma<-para[-c(1:sep)];
  
  ei<-fyi.star-fX%*%para.beta; 
  sigmai<-exp(fZ%*%para.gamma);
  es.ratio<-as.vector(ei/sigmai);
  
  #km.est <- Surv(ei/sigmai, fdelta)
  km.fit <- survfit(Surv(as.numeric(es.ratio), fdelta) ~ 1);
  rank.es.ratio<-rank(es.ratio);
  surv.es.ratio<-km.fit$surv[rank.es.ratio];
  
  #int.top<-0;
  ## deal with the situation when the largest resi is missing.
  cum.prod.dens.time<-frac.cum.surv<-es.ratio;
  es.ratio.star<-es.ratio;
  if(sum(km.fit$n.event==1)<km.fit$n)
  {
    uniq.surv<-km.fit$surv[km.fit$n.event==1]; 
    uniq.time<-km.fit$time[km.fit$n.event==1];
    n.surv<-length(km.fit$surv)
    
    if(km.fit$surv[n.surv]>0) 
    {uniq.surv<-c(uniq.surv,0);
    uniq.time<-c(uniq.time,km.fit$time[n.surv]);
    }
    # when the largest residual is also RC, we need to consider it as non-RC
    
    n.uniq.surv<-length(uniq.surv);
    
    
    uniq.den<-c(1,uniq.surv[-n.uniq.surv])-uniq.surv;
    
    map.to.uniq<-rowSums(outer(es.ratio[fdelta==FALSE],uniq.time,"<="));
    map.to.uniq.nozero<-map.to.uniq[map.to.uniq>0];
    # cum.prod.dens.time[(fdelta==FALSE)]<-cumsum(rev(uniq.time*uniq.den))[map.to.uniq];
    
    cum.prod.dens.time[(fdelta==FALSE)][map.to.uniq>0]<-cumsum(rev(uniq.time*uniq.den))[map.to.uniq.nozero];
    #cum.prod.dens.time[(fdelta==FALSE)][map.to.uniq==0]<-es.ratio[(fdelta==FALSE)][map.to.uniq==0]+1
    
    es.ratio.star[fdelta==FALSE]<- cum.prod.dens.time[fdelta==FALSE];
    #es.ratio.star[(fdelta==FALSE)][map.to.uniq==0]<-es.ratio.star[(fdelta==FALSE)][map.to.uniq==0]+1
    ## this is associated with the obj imitate the normality assumption
    frac.cum.surv[fdelta==FALSE]<-cum.prod.dens.time[fdelta==FALSE]/surv.es.ratio[fdelta==FALSE]*sigmai[fdelta==FALSE]
    #frac.cum.surv[(fdelta==FALSE)][map.to.uniq==0]<-es.ratio[(fdelta==FALSE)][map.to.uniq==0]+1
  }
  
  if(which.obj== 1) {obj.our<- sum(es.ratio.star^2)/2+sum(fZ%*%para.gamma)
  } else {fhat.yi<-fdelta*fyi.star+(1-fdelta)*(fX%*%para.beta+frac.cum.surv);
  obj.bje<-t(fhat.yi-fX%*%para.beta)%*%fX;
  obj.our<-sum(obj.bje^2);}#+sum(fZ%*%para.gamma);
  
  
  return(obj.our);
  
}

#################################################################################

n.runs <- 10
rc.simu <- data.frame(matrix(nrow = n.runs, ncol = 13))
names(rc.simu) <- c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "gamma0", "gamma1", "gamma2", "gamma3", "gamma4", "gamma5", "gamma6")

set.seed(12345)
for (i in 1:n.runs){
  out.normal.multi<-simu.rc.normal.multi(n.obs=10^3,beta.val.multi=c(39,1,2,rep(0,3)),fun.type='logis');
  para.current<-c(25,0.5,2.5,rep(1,3),-2.9,2,3,rep(1,4));
  ourbje.mle1<-optim(par=para.current,  method="BFGS",obj.func.bje,flist.data=out.normal.multi[[3]],sep=6,control=list(maxit=10^5))
  rc.simu[i,] <- ourbje.mle1$par
}

rc.simu <- data.frame(t(rc.simu))
names(rc.simu)[1:n.runs] <- c(1:n.runs)
rc.simu$mean <- apply(rc.simu, 1, mean)
rc.simu$std.error <- apply(rc.simu, 1, sd)

write.csv(rc.simu, file = "rcsimulation10.csv")

end.time <- Sys.time()
start.time-end.time
