library(raster)
library(rgdal)
library(lubridate)
library(classInt)
library(randtoolbox)
library(ggplot2)

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
saveRDS(res_final,"res_final.rds")
########################################################################################################################

res_month=apply(res_final,1,sum)

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


