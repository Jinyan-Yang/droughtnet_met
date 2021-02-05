# DRL_AUTO_250HI_SOIL_R_20190131.dat
library(HIEv)
library(doBy)
library(lubridate)
# set token for hiev
if(file.exists("c:/hiev/token.txt")){
  setToken()
}else{
  stop(c("Token need to be in c:/hiev/token.txt"))
}

#prepare the folders
if(!dir.exists("download"))dir.create("download")

# 
ng.swc.df <- downloadTOA5('DRL_AUTO_450LO_SOIL_R',topath = 'download/450lo')
# cb.swc.df <- downloadTOA5('DRL_AUTO_350LO_SOIL_R',topath = 'download/350lo')
mp.swc.df <- downloadTOA5('DRL_AUTO_250HI_SOIL_R',topath = 'download/250h')
qp.swc.df <- downloadTOA5('DRL_AUTO_350HI_SOIL_R',topath = 'download/350h')

# 
today.date <- Sys.Date()

plot.rain.func <- function(ng.swc.df,site.nm,past.days = 365){
  
  # ng.swc.df$mon <- format(ng.swc.df$DateTime,'%Y-%m')
  ng.sum.df <- doBy::summaryBy(Rain_mm_Tot~Date,data = ng.swc.df,
                               FUN=sum,na.rm=T,keep.names = T)
  
  ng.sum.df <- ng.sum.df[year(ng.sum.df$Date) %in% c(2019,2020),]
  
  plot(Rain_mm_Tot~Date,data = ng.sum.df[ng.sum.df$Date > today.date - past.days &
                                           ng.sum.df$Date < today.date, ],type='s',col='navy',
       xlab='',ylab='Rainfall(mm/d)')
  
  acu.rain.30 <- sum(ng.sum.df$Rain_mm_Tot[ng.sum.df$Date > today.date - past.days &
                  ng.sum.df$Date < today.date])
  
  legend('topright',legend = site.nm,bty='n')
  
  legend('topleft',legend = paste0('Rain.total = ',acu.rain.30),bty='n')
  
}

pdf('dn_rain.pdf',width = 10,height = 10*.618)
par(mfrow=c(2,2))
par(mar=c(3,5,1,1))
plot.rain.func(ng.swc.df,'Nyngan')
# plot.rain.func(cb.swc.df,'Cobar')
plot.rain.func(mp.swc.df,'Milparinka')
plot.rain.func(qp.swc.df,'Qilpie')

dev.off()
