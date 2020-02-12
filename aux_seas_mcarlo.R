require(lubridate)

prob_Rain=function(x,intervals=c(-1,0)) {
  stopifnot(length(x)>0) 
  if (sum(x) == 0){return(list(norain=length(x),
                              rain=0,
                              rain_ecf=ecdf(x),
                              miss=length(which(is.na(x))),
                              len=length(x)))}
  x1=na.omit(x)
  temp=as.numeric(table(cut(x1, breaks=intervals))/length(x1))
  list(norain=temp[1],
       rain=1-temp[1],
       rain_ecf=ecdf(x[x>0]),
       miss=length(which(is.na(x))),
       len=length(x1))
}

prob_Temp=function(x) {
  stopifnot(length(x)>0)
  x1=na.omit(x)
  list(ecdf=ecdf(x[x>0]),
       miss=length(which(is.na(x))),
       len=length(x1))
}

create_month_pio=function(x,days=30,sampling=10){
  list_gp_rain=res=list();
  for ( i in 1:sampling) { temp=runif(days);list_gp_rain[[i]]=ifelse(temp>x$norain,1,0)}
  list_rain=list_gp_rain;
  for ( i in 1:sampling) { for (j in which(list_gp_rain[[1]]==1)) 
  { list_rain[[i]][j]=as.numeric(quantile(x$rain_ecf,runif(1)))
  }
  }
  res$gp_stat=c(mean(unlist(lapply(list_gp_rain,sum))),sd(unlist(lapply(list_gp_rain,sum))))
  res$rain_stat=c(mean(unlist(lapply(list_rain,sum))),sd(unlist(lapply(list_rain,sum))))
  res$list_rain=list_rain
  return(res)
}

create_month_temp=function(x,days=30,sampling=10){
  list_temp=res=list();
  for ( i in 1:sampling) { list_temp[[i]]=rep(NA,days)}
   for ( i in 1:sampling) { for (j in 1:days) 
  { list_temp[[i]][j]=as.numeric(quantile(x$ecdf,runif(1)))
  }
  }
  res$temp_stat=c(mean(unlist(lapply( list_temp,function(x) {mean(x)}))),
                  sd(unlist(lapply( list_temp,function(x) {x}))))
  return(res)
}

gen_WT_month=function(x,days=1) {
  intervals=as.numeric(c(0,cumsum(x)))
  i=1;res=list()
  repeat{res[[i]]=findInterval(runif(1), intervals);if (i == days){break};i=i+1 }
  return(unlist(res))
}

dates_clim=function(x,window=15,daygrep=F) {if(daygrep==T) {return(as.Date(x)+(-window:window))};substr(as.Date(x)+(-window:window),5,10)}

fill.matrix = function(expr, nrow=1, ncol=1) {
  matrix(eval(expr, envir=list(x=nrow*ncol)), nrow=nrow, ncol=ncol)
}

###############################################################################################