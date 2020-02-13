library(raster)
library(rgdal)
library(lubridate)
library(classInt)
library(randtoolbox)
library(ggplot2)
library(ggpubr)
library(nortest)
library(resample)
library(MASS)
library(fitdistrplus)

######################################################################################

setwd("/home/alf/Scrivania/lav_montecarlo")

source("aux_seas_mcarlo.R")

######################################################################################
# setup parameters

windows=15
dataset_prec=readRDS("dataset_prec.rds")
rains_tosc=readRDS("dataset_prec.rds")
pct9_daily=readRDS("dates_pct9_daily.rds")
iniclim="1981-01-01"
endclim="2010-12-31"
city="Firenze"
WC=c(1:9)
sim_months=10000

######################################################################################

mat_clim=dataset_prec[which( (dataset_prec$dates>as.Date(iniclim) & (dataset_prec$dates<as.Date(endclim)))==T),]
pct9_daily_clim=pct9_daily[which( (dataset_prec$dates>as.Date(iniclim) & (dataset_prec$dates<as.Date(endclim)))==T),]
mat_clim=merge(mat_clim,pct9_daily_clim)
###################################################################################################################################

f_PCT_ecm_marzo=read.csv("Pesi-PCT-ecm_Giorno-marzo.csv",header=T)
frequenze_pct9=read.csv("frequenze_pct9.csv",header=F,sep=" ")

dates_forecast=as.Date(ISOdate(f_PCT_ecm_marzo$year,f_PCT_ecm_marzo$month,f_PCT_ecm_marzo$day))
month_pesi_PCT_ecm_marzo=apply(f_PCT_ecm_marzo[which(f_PCT_ecm_marzo$month==3),4:12]/51,2,mean)
day_pesi_PCT_ecm_marzo=data.frame(dates_forecast,f_PCT_ecm_marzo[which(f_PCT_ecm_marzo$month==3),4:12]/51)

########################################################################################################################
res_obj=list()
for ( jj in 1:length(dates_forecast)) {
               mat_day=mat_clim[which(grepl(paste(dates_clim(dates_forecast[jj],daygrep=F),collapse = "|"), mat_clim$dates)==T),]
               mat_day_temp=mat_day[,c("dates",city,"ct")]
               res=list()
               for ( i in WC){  
                              temp=subset(mat_day_temp,ct==i)[,c(city)]
                              res[[i]]=prob_Rain(rep(0,30))
                              if(length(temp)>0) {res[[i]]=prob_Rain(temp)}
                             }
res_obj[[jj]]=res
}
saveRDS(res_obj,"firenze_marzo_sampler.rds")
########################################################################################################################
# generate wt forecast days with daily forecast matrix


wt_months_days=apply(day_pesi_PCT_ecm_marzo[,2:10],1,function(x) gen_WT_month(x,1))

res_final=data.frame(matrix(NA, nrow=sim_months, ncol=length(dates_forecast)))

for ( j in 1:length(dates_forecast)) {
     for ( i in 1:nrow(res_final)) {res_final[i,j]=ifelse(runif(1)>res_obj[[j]][[wt_months_days[j]]]$rain,
                                                          as.numeric(quantile(res_obj[[j]][[wt_months_days[j]]]$rain_ecf,runif(1))),
                                                            0);

                                    }
}

random_matrix=res_final=data.frame(matrix(NA, nrow=sim_months, ncol=length(dates_forecast)))
random_matrix=apply(random_matrix,c(1,2),function(x) runif(1))


for ( j in 1:length(dates_forecast)) {
for ( i in 1:nrow(random_matrix)) {random_matrix[i,j]=ifelse(random_matrix[i,j]>res_obj[[j]][[wt_months_days[j]]]$rain,
                                                     as.numeric(quantile(res_obj[[j]][[wt_months_days[j]]]$rain_ecf,runif(1))),
                                                     0);
                                   if ( i %% 1000 ==0) {wt_months_days=apply(day_pesi_PCT_ecm_marzo[,2:10],1,function(x) gen_WT_month(x,1))} 
}
}
saveRDS(res_final,"res_final.rds")
########################################################################################################################

res_month=apply(res_final,1,sum)
res_month2=apply(random_matrix,1,sum)

ecdf(res_month2)


qplot(res_month,
      geom="histogram",
      binwidth = 5,  
      main = "Firenze Distribuzione pioggie Marzo 2020", 
      xlab = "mm", 
      ylab = "Simulazioni (N=10000) ",
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(0,200))

q1=qplot(res_month2,
      geom="histogram",
      binwidth = 5,  
      main = "Firenze Distribuzione pioggie Marzo 2020 ECM PCT9 CT forecast", 
      xlab = "mm", 
      ylab = "Simulazioni (N=10000) ",
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(0,200))





############################################################################
# clim and bootstrap

dataset_prec$mese=month(dataset_prec$dates)
dataset_prec_marzo=subset(dataset_prec,mese==3)
fir_marzo=tapply(dataset_prec_marzo$Firenze,dataset_prec_marzo$anno,sum)

N=length(fir_marzo)
nboots=10000
boot.result=numeric(nboots)
for(i in 1:nboots){
                   boot.samp=sample(fir_marzo,N,replace=TRUE)
                   boot.result[i]=mean(boot.samp)
}



g=fitdistr(fir_marzo,"gamma")
boot_gamma=rgamma(10000,g$estimate[1],g$estimate[2])

fitgmme <- fitdist(as.numeric(fir_marzo), "gamma", method="mle")
summary(fitgmme)
boot_gamma2=rgamma(10000,g$estimate[1],g$estimate[2])


############################################################################


q2=qplot(boot_gamma,
      binwidth = 5,  
      main = "Firenze Distribuzione pioggie Marzo Clim Peretola ", 
      xlab = "mm", 
      ylab = "Simulazioni (N=10000) ",
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(0,200))

########################################################################################################################
ggarrange(q1,q2,nrow=2)



