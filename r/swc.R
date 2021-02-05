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

# HIEv R package will download files to here:
download.path <- file.path("download/")
setToPath(download.path)

downloadHIEv(searchHIEv('DRL_AUTO_250HI_SOIL_R'),topath = 'download/250hi')
downloadHIEv(searchHIEv('DRL_AUTO_350HI_SOIL_R'),topath = 'download/350hi')
downloadHIEv(searchHIEv('DRL_AUTO_450HI_SOIL_R'),topath = 'download/450hi')

downloadHIEv(searchHIEv('DRL_AUTO_250LO_SOIL_R'),topath = 'download/250LO')
downloadHIEv(searchHIEv('DRL_AUTO_350LO_SOIL_R'),topath = 'download/350LO')
downloadHIEv(searchHIEv('DRL_AUTO_450LO_SOIL_R'),topath = 'download/450LO')

ng.swc.df <- downloadTOA5('DRL_AUTO_450LO_SOIL_R',topath = 'download/450lo')
bk.swc.df <- downloadTOA5('DRL_AUTO_250LO_SOIL_R',topath = 'download/250lo')






check.swc.func <- function(ng.swc.df,out.name,ppt){
  ng.tmp.df <- summaryBy(.~Date,data = ng.swc.df,
                         FUN=mean,na.rm=TRUE,keep.names = T)
  ng.rain.df <- summaryBy(Rain_mm_Tot~Date,data = ng.swc.df,
                          FUN=sum,na.rm=TRUE,keep.names = T)
  
  ng.day.df <- ng.tmp.df
  ng.day.df$Rain_mm_Tot <- ng.rain.df$Rain_mm_Tot
  
  on.exit(dev.off())
  pdf(out.name,width = 6,height = 6)
  
  
  par(mar=c(5,5,1,5))
  par(mfrow=c(1,1))
  with(ng.day.df,plot(VW_Avg.1.~Date,type='l',col='grey',lwd=3,ylim=c(0,.3)))
  par(new=TRUE)
  with(ng.day.df,plot(Rain_mm_Tot~Date,type='s',col='blue',ann=F,axes=F))
  axis(4,at = seq(0,30,by = 10),labels = seq(0,30,by = 10))
  mtext('PPT (mm/day)',side = 4,line=2)
  
  legend('topright',legend = c(paste0('Site MAP = ',ppt),
                               paste0('PPT 2017 = ',sum(ng.day.df$Rain_mm_Tot[year(ng.day.df$Date) == 2017])),
                               paste0('PPT 2018 = ',sum(ng.day.df$Rain_mm_Tot[year(ng.day.df$Date) == 2018]))
                               ),
         pch=16,col='white')
  

  
  par(mar=c(5,5,1,1))
  par(mfrow=c(2,2))
  # site outlook
  plot(0,0,xlim=c(0,3),ylim=c(0,3),col='white',ann=F,axes=F)
  abline(v=0)
  abline(v=1)
  abline(v=2)
  abline(v=3)
  abline(h=0)
  abline(h=1)
  abline(h=2)
  abline(h=3)
  axis(1,at = 0.5:2.5,labels = paste0('Stripe',c('A','B',"C")),
       tick = F)
  points(0.5,0.5,pch='1',col = 'grey')
  points(1.5,0.5,pch='2',col = 'red')
  points(2.5,0.5,pch='3',col = 'grey')
  
  points(0.5,1.5,pch='4',col = 'red')
  points(1.5,1.5,pch='5',col = 'green')
  points(2.5,1.5,pch='6',col = 'red')
  
  points(0.5,2.5,pch='7',col = 'green')
  points(1.5,2.5,pch='8',col = 'grey')
  points(2.5,2.5,pch='9',col = 'green')
  
  legend('left',legend = c('Control','+','-'),pch=16,col=c('grey','green','red'),
         horiz = F,xpd=T,inset = -0.25,bty='n')
  
  # 
  with(ng.day.df,plot(VW_Avg.1.~Date,type='l',col='grey',lwd=3,ylim=c(0,.3),
                      ylab='SWV (%)'))
  with(ng.day.df,points(VW_Avg.4.~Date,type='l',col='red',lwd=1))
  with(ng.day.df,points(VW_Avg.7.~Date,type='l',col='green',lwd=1))
  title('A')
  
  with(ng.day.df,plot(VW_Avg.8.~Date,type='l',col='grey',lwd=3,ylim=c(0,.3),
                      ylab='SWV (%)'))
  with(ng.day.df,points(VW_Avg.2.~Date,type='l',col='red',lwd=1))
  with(ng.day.df,points(VW_Avg.5.~Date,type='l',col='green',lwd=1))
  title('B')
  
  with(ng.day.df,plot(VW_Avg.3.~Date,type='l',col='grey',lwd=3,ylim=c(0,.3),
                      ylab='SWV (%)'))
  with(ng.day.df,points(VW_Avg.6.~Date,type='l',col='red',lwd=1))
  with(ng.day.df,points(VW_Avg.9.~Date,type='l',col='green',lwd=1))
  title('C')
  
  
}

ng.swc.df <- downloadTOA5('DRL_AUTO_450LO_SOIL_R',topath = 'download/450lo')
check.swc.func(ng.swc.df,'ng swc.pdf',450)


qp.swc.df <- downloadTOA5('DRL_AUTO_350HI_SOIL_R',topath = 'download/350hi')
check.swc.func(qp.swc.df,'qp swc.pdf',350)

# with(ng.day.df[ng.day.df$Rain_mm_Tot > 0,],plot(VW_Avg.1.~Rain_mm_Tot,pch=16,col='grey'))
# with(ng.swc.df,plot(VW_Avg.3.~Rain_mm_Tot,pch=16,col='grey'))
# with(ng.swc.df,plot(VW_Avg.8.~Rain_mm_Tot,pch=16,col='grey'))
# 
# with(ng.swc.df,plot(VW_Avg.5.~Rain_mm_Tot,pch=16,col='grey'))
# with(ng.swc.df,plot(VW_Avg.4.~Rain_mm_Tot,pch=16,col='grey'))


ng.day.df$VW_Avg.1.norm <- (max(ng.day.df$VW_Avg.1.,na.rm = T) - ng.day.df$VW_Avg.1.) / 
  (max(ng.day.df$VW_Avg.1.,na.rm = T) - min(ng.day.df$VW_Avg.1.,na.rm = T))

ng.day.df$VW_Avg.4.norm <- (max(ng.day.df$VW_Avg.4.,na.rm = T) - ng.day.df$VW_Avg.4.) / 
  (max(ng.day.df$VW_Avg.4.,na.rm = T) - min(ng.day.df$VW_Avg.4.,na.rm = T))

ng.day.df$VW_Avg.7.norm <- (max(ng.day.df$VW_Avg.7.,na.rm = T) - ng.day.df$VW_Avg.7.) / 
  (max(ng.day.df$VW_Avg.7.,na.rm = T) - min(ng.day.df$VW_Avg.7.,na.rm = T))


ng.day.df$swc.c <- (ng.day.df$VW_Avg.1. + ng.day.df$VW_Avg.3. + ng.day.df$VW_Avg.8.) / 3

ng.day.df$swc.plus <- (ng.day.df$VW_Avg.5. + ng.day.df$VW_Avg.7. + ng.day.df$VW_Avg.9.) / 3

ng.day.df$swc.minus <- (ng.day.df$VW_Avg.2. + ng.day.df$VW_Avg.4. + ng.day.df$VW_Avg.6.) / 3
# 
with(ng.day.df,plot(swc.c~Date,type='l',col='grey',lwd=3,ylim=c(0,.25),
                    ylab='SWV (%)'))
with(ng.day.df,points(swc.minus~Date,type='l',col='red',lwd=1))
with(ng.day.df,points(swc.plus~Date,type='l',col='green',lwd=1))

