library(plyr)
library(mixtools)
options(stringsAsFactors = FALSE)

#A function that returns the gaussian mixture with the optimal number of components, as
#determined by the bayesian information criterion.
gmm=function(x, kmin=1, kmax=7, maxit=10000,maxrestarts=100){
  #kmin and kmax are the number of components beyond which the function will not go.
  x=x[!is.na(x)]
  
  if(kmin<2){
    mu=mean(x)
    sigma=sd(x)
    loglik=-length(x)*(0.5*log(2*pi)+log(sigma))-0.5*sum((x-mu)^2)/(sigma^2)
    posterior=matrix(rep(1,length(x)),ncol=1)
    colnames(posterior)='comp.1'
    
    mixture=list(x=x,lambda=1,mu=mu,sigma=sigma, logLik=loglik, posterior=posterior, conv=TRUE,iter=1)
    
    bic=2*log(length(x))-2*loglik
    k=2
  }else{
    bic=Inf
    k=kmin
  }
  
  while(k<=kmax){
    
    iter=capture.output({mixture1=normalmixEM(x,k=k, fast=TRUE, maxit=maxit,maxrestarts=maxrestarts)})
    iter=as.numeric(gsub(".*?([0-9]+).*", "\\1", iter))
    iter=iter[!is.na(iter)]
    mixture1$iter=iter
    mixture1$conv=(iter<maxit)
    
    bic1=log(length(x))*(3*k-1)-2*mixture1$loglik
    if(bic1>bic){
      break
    }else{
      bic=bic1
      mixture=mixture1
    }
    k=k+1
  }
  return(mixture)
}

t_trad=function(data,tau, type='traditional'){
  
  if(type=='traditional'){
    return(sum(data$dt[data$dt<tau])-data$tot)
  }else{
    m=mean(data$dt[data$dt<tau])
    if(is.na(m)){
      m=0
    }
    return(length(data$dt)*m-data$tot)
  }
}


iterate_root=function(data, tau, type='traditional'){
  y1=t_trad(data,tau[1], type)
  y2=t_trad(data,tau[2], type)
  if(y1==0){
    return(c(tau[1],tau[1]))
  }
  
  if(y2==0){
    return(c(tau[2],tau[2]))
  }
  
  if(sign(y1)==sign(y2)){
    return(NULL)
  }
  
  taun=mean(tau)
  
  yn=t_trad(data,taun, type)
  
  if(yn==0){
    return(c(taun,taun))
  }
  
  if(sign(y1)==sign(yn)){
    return(c(taun,tau[2]))
  }else{
    return(c(tau[1],taun))
  }
  
}

find_threshold=function(data,eps=0.0001, type='traditional'){
  
  tau=c(0,max(data$dt)+1)
  while (abs(diff(tau))>eps){
    tau=iterate_root(data,tau, type)
  }
  
  return(mean(tau))
  
}

totmix=function(cs, username='username', time='time', category=NULL, nusers=NA, timein=0.1, timeout=7200, minevents=50, maxit=10000, maxrestarts=100, K=NA, Kmin=3, Kmax=7){
  
  tic=proc.time()[3]
  
  cs=plyr::rename(cs,c(username='username',time='time'))
  if(!is.null(category)){
    temp='category'
    names(temp)=names(category)[1]
    cs=plyr::rename(cs,temp)
    cs$category[is.na(cs$category)]=''
  }
  
  cs=cs[order(cs$username,cs$time),]
  ind=which(cs$username[-nrow(cs)]!=cs$username[-1]) #Ends of username blocks
  cs$dt=NA
  cs$dt[-nrow(cs)]=diff(cs$time)
  if(!is.null(category)){
    temp=paste0(cs$category[-nrow(cs)],'/',cs$category[-1])
    i=which((cs$category[-1]!=cs$category[-nrow(cs)]))
    cs$category[which((cs$category[-1]!=cs$category[-nrow(cs)]))]=temp[i]
  }
  cs$dt[ind]=NA
  cs=subset(cs,!is.na(cs$dt))
  
  #Subset to those where the consecutive clicks are in the same category of content
  if(!is.null(category)){
    cs=subset(cs,cs$category %in% category)
  }
  
  #Impose restrictions: the minimum and the maximum dt's to include. This is optional.
  cs=subset(cs,(cs$dt<=timeout)&(cs$dt>=timein))
  
  users=plyr::rename(aggregate(cs$time, by=list("username"=cs$username),length),c('x'='nevents'))
  
  #To get meaningful values, let's require at least minevents events from a user
  users=subset(users,users$nevents>=minevents)
  users=users[order(-users$nevents),]
  
  if(!is.na(nusers)){
    users=users[1:min(nusers,nrow(users)),]
  }
  
  t=rep(NA,nrow(users)) ##This will be time on task, per user
  user_tau=rep(NA,nrow(users)) #This will be the estimated threshold per user
  user_tau_older=rep(NA,nrow(users))
  r=rep(NA,nrow(users)) ##This will be time on task ratio to net time, per user
  m=rep(NA,nrow(users)) ##This will be the mean durations of the entirely-on-task intervals, per user
  gof=rep(NA,nrow(users)) ##This will be the goodness of fit (correlation of mixture CDF and ECDF), per user
  conv=rep(NA,nrow(users))
  iter=rep(NA,nrow(users))
  npeaks=rep(NA,nrow(users))
  u_density=list()
  u_mixture=list()
  #####End of data preparation
  
  for (i in 1:nrow(users)){
    
    dt=subset(cs,cs$username==users$username[i])$dt
    dt=dt[!is.na(dt)]
    dt=dt[dt>0]
    dat=log(dt)
    
    
    #Density for plotting purposes
    d=density(dat,na.rm=TRUE,cat=0)
    # d=data.frame(x=d$x,density=d$y)
    x=sort(dat)
    d=data.frame(x=x,y=approx(x=d$x,y=d$y,xout=x)$y)
    
    try({
      
      if(is.na(K)){
        mixture=gmm(dat, kmin=Kmin, kmax=Kmax, maxit=maxit,maxrestarts=maxrestarts)    
      }else{
        mixture=gmm(dat,kmin=K,kmax=K, maxit=maxit,maxrestarts=maxrestarts)
      }
      
      conv[i]=mixture$conv
      iter[i]=mixture$iter
      
      k=length(mixture$lambda)
      npeaks[i]=k
      
      d$mix_density=0
      for (j in 1:k){
        d$mix_density=d$mix_density+mixture$lambda[j]*dnorm(d$x,mean=mixture$mu[j],sd=mixture$sigma[j])
      }
      
      d$mix_cdf=0
      for (j in 1:k){
        d$mix_cdf=d$mix_cdf+mixture$lambda[j]*pnorm(d$x,mean=mixture$mu[j],sd=mixture$sigma[j])
      }
      cdf=ecdf(d$x)
      d$ecdf=cdf(d$x)
      ##Goodness of fit:
      gof[i]=cor(d$mix_cdf,d$ecdf)
      
      #Return from log times to times
      d$dt=exp(d$x)
      d$x=NULL
      
      mixture$m=rep(NA,k)
      for(j in 1:k){
        mixture$m[j]=sum(dt*mixture$posterior[,j])/sum(mixture$posterior[,j])
      }
      
      
      if(k>1){
        k_off=which.max(mixture$m) #Identify the index of the last component (I am not certain that mixtools always indexes components in the order of cinreasing mu)
        temp=mixture$lambda*mixture$m
        # m[i]=sum(temp)-temp[k_off]
        m[i]=(sum(temp)-temp[k_off])/(1-mixture$lambda[k_off])
        t[i]=m[i]*length(dat)
        r[i]=t[i]/sum(dt)
        #Find the user-specific effective threshold
        user_tau_older[i]=find_threshold(data=list(tot=t[i],dt=dt), type='traditional')
        user_tau[i]=find_threshold(data=list(tot=t[i],dt=dt), type='corrected')
        
        
      }
      
      u_density[[i]]=d
      u_mixture[[i]]=mixture
      
      
    })
    cat(paste(rep('\b',15),collapse=''),round(100*i/nrow(users),3),'%')
  }
  
  ##Just in case
  conv[is.na(conv)]=FALSE
  
  subset_cs=subset(cs,cs$username %in% users$username)
  
  tot=list(u_data=u_density
           , u_mixture=u_mixture
           , u_tau_older=user_tau_older
           , u_tau=user_tau
           , mean_tot_interval=m
           ,tot=t
           ,tot_to_net=r
           ,ncomp=npeaks
           ,users=users
           ,conv=conv
           ,iter=iter
           ,gof=gof
           # , data=subset_cs
  )
  
  
  data=list(tot=sum(t[tot$conv]))
  dt=subset_cs$dt[subset_cs$username %in% tot$users$username[tot$conv]];
  data$dt=dt[!is.na(dt)]
  tot$tau_older=find_threshold(data,type='traditional')
  tot$tau=find_threshold(data,type='corrected')
  
  
  ##Order components aesthetically:
  
  tot=tot[c('users'
            ,'tau','u_tau','tau_older','u_tau_older','tot','tot_to_net','mean_tot_interval','conv','gof'
            ,'ncomp','iter','u_data','u_mixture')]
  
  tot$runtime=proc.time()[3]-tic
  cat('\nComputation time:',tot$runtime,'sec.\n\n')
  return(tot)
}
