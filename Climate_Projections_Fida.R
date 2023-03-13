
# Load packages
library(imputeTS)
library(dplyr)
library(tidyr)
library(tmap)
library(GSODR) #Global Surface Summary of the Day ('GSOD') Weather
library("dplyr")
library(hydroTSM)
library(skimr)#summary statistics
library(raster)
library(maps)
library(ggplot2)
library(lubridate)
library(Cairo)
setwd("C:/Users/FBAGBO/Documents/R/Gloria_process/Fida")
clim_dt<- as_tibble(read.delim("Climate_data_Jarra.txt"))
clim_dt$date=as.Date(clim_dt$year)
clim_dt$Ann=format(clim_dt$date, "%Y")


# GSOD Weather Station Data Inventories
inventory <- get_inventory()
head(inventory)

# Retrieve stations information of a particular country
# As example I will use Benin republic (in West-Africa).

#print Gambia descriptive information: station id,name,latitude and longitude
Gambia_stat_id=unique(inventory[which(inventory$COUNTRY_NAME=='GAMBIA'),1:4])


## Download all weather stations for the 1989:2019 year in Benin using get_GSOD
tbar_burkina<-get_GSOD(years=1975:2021, country = 'BURKINA FASO')
tbar_Togo<-get_GSOD(years=1975:2021, country = 'TOGO')
tbar_Gambia<-get_GSOD(years=1975:2021, country = 'GambiaL')

dat_Burkina=tbar_burkina[,c('NAME','YEAR','MONTH','DAY','TEMP','MIN','MAX','PRCP','RH','WDSP')]
dat_Togo=tbar_Togo[,c('NAME','YEAR','MONTH','DAY','TEMP','MIN','MAX','PRCP','RH','WDSP')]
dat_Gambia=tbar_Gambia[,c('NAME','YEAR','MONTH','DAY','TEMP','MIN','MAX','PRCP','RH','WDSP')]



write.csv(dat_Burkina,'Data/data_Burkina.csv')
write.csv(dat_Togo,'Data/data_togo.csv')
write.csv(dat_Gambia,'Data/data_Gambia.csv')
## Interpolation des donnees 

# Set font
windowsFonts(B=windowsFont("Bookman Old Style"))

setwd("C:/Users/FBAGBO/Documents/R/Gloria_process/Fida/")

## Burkina
for(i in 1:9){
  
  temp=dat_Burkina[which(dat_Burkina$NAME==unique(dat_Burkina$NAME)[i]),]
  station_name=unlist(strsplit(unique(temp$NAME), "[,]"))[1]
  
  filenname_dd=paste("Station_temperature_Burkina_new_",station_name,".png",sep='')
  
  png(filenname_dd, family = "ArialMT", width=500, height=350,pointsize =15)
  monmean=temp %>% group_by(NAME,MONTH)%>% 
    summarise(Tmin=mean(MIN,na.rm = TRUE),TAVG=mean(TEMP,na.rm = TRUE),Tmax=mean(MAX,na.rm = TRUE))%>% 
    arrange(match(MONTH,month.name))
  
  
  
  print(station_name)
  boxplot(temp$TEMP ~temp$MONTH,
          ylab="Deg C" , xlab="Months", 
          main=paste(station_name),
          axes=FALSE,col='#E37831',ylim=c(22,40))
  axis(1,at=c(1:12),labels = month.abb,las=3.5);axis(2,las=2);box()
  lines(1:12,monmean$TAVG,col="#ff7f00",lwd=3)
  legend(x="bottomright",legend="Monthly Mean",
         cex = 0.6,lwd = 3, col = "#ff7f00",lty = 1,bty='n')
  
  
  dev.off()
  
}



## Togo

for(i in 1:9){

temp=dat_Togo[which(dat_Togo$NAME==unique(dat_Togo$NAME)[i]),]
station_name=unlist(strsplit(unique(temp$NAME), "[/]"))[1]

filenname_dd=paste("Station_temperature_Togo_new_",station_name,".png",sep='')

# png(filename =filenname_dd,bg = "white",width=500,height=400)
png(filenname_dd, family = "ArialMT", width=500, height=350,pointsize =15)

monmean=temp %>% group_by(NAME,MONTH)%>% 
  summarise(Tmin=mean(MIN,na.rm = TRUE),TAVG=mean(TEMP,na.rm = TRUE),Tmax=mean(MAX,na.rm = TRUE))%>% 
  arrange(match(MONTH,month.name))



print(station_name)
boxplot(temp$TEMP ~temp$MONTH,
        ylab="Deg C" , xlab="Months", 
        main=paste(station_name),
        axes=FALSE,col='#E37831',ylim=c(22,40))
axis(1,at=c(1:12),labels = month.abb,las=3.5);axis(2,las=2);box()
lines(1:12,monmean$TAVG,col="#ff7f00",lwd=3)
legend(x="bottomright",legend="Monthly Mean",
       cex = 0.6,lwd = 3, col = "#ff7f00",lty = 1,bty='n')


dev.off()

}

## Gambia 

for(i in 1:length(unique(dat_Gambia$NAME))){
  temp=dat_Gambia[which(dat_Senegal$NAME==unique(dat_Gambia$NAME)[i]),]
  station_name=unlist(strsplit(unique(temp$NAME), "[/]"))[1]
  
  filenname_dd=paste("Station_temperature_Gambia_new_",station_name,".png",sep='')
  
  # png(filename =filenname_dd,bg = "white",width=500,height=400)
  png(filenname_dd, family = "ArialMT", width=500, height=350,pointsize =15)
  
  monmean=temp %>% group_by(NAME,MONTH)%>% 
    summarise(Tmin=mean(MIN,na.rm = TRUE),TAVG=mean(TEMP,na.rm = TRUE),Tmax=mean(MAX,na.rm = TRUE))%>% 
    arrange(match(MONTH,month.name))

  print(station_name)
  boxplot(temp$TEMP ~temp$MONTH,
          ylab="Deg C" , xlab="Months", 
          main=paste(station_name),
          axes=FALSE,col='#E37831',ylim=c(22,40))
  axis(1,at=c(1:12),labels = month.abb,las=3.5);axis(2,las=2);box()
  lines(1:12,monmean$TAVG,col="#ff7f00",lwd=3)
  legend(x="bottomright",legend="Monthly Mean",
         cex = 0.6,lwd = 3, col = "#ff7f00",lty = 1,bty='n')
  
  
  dev.off()
  
}


#-------------------------------------------------------------------------------------------------#



# Retrieve, day, month and year from ts -- Burkina Faso

for (i in 1:length(unique(dat_Burkina$NAME))){
  
MAG=dat_Burkina[which(dat_Burkina$NAME==unique(dat_Burkina$NAME)[i]),]
station_name=unlist(strsplit(unique(MAG$NAME), "[/]"))[1]


MAG$PRCP=na_interpolation(MAG$PRCP)
MAG$MIN=na_interpolation(MAG$MIN)
MAG$MAX=na_interpolation(MAG$MAX)
MAG$WDSP=na_interpolation(MAG$WDSP)
MAG$RH=na_interpolation(MAG$RH)



MAG$Date=ymd(paste(MAG$YEAR,MAG$MONTH,MAG$DAY,sep='-'))


Yearly=MAG %>% group_by(YEAR) %>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
                                            MIN=mean(MIN,na.rm=TRUE),
                                            MAX=mean(MAX,na.rm=TRUE), 
                                            PRCP=sum(PRCP,na.rm=TRUE),
                                            RH =mean(RH,na.rm=TRUE),
                                            WDSP=mean(WDSP,na.rm=TRUE))

## YearMon
Yearmon=MAG %>% group_by(YEAR,MONTH) %>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
                                                   MIN=mean(MIN,na.rm=TRUE),
                                                   MAX=mean(MAX,na.rm=TRUE), 
                                                   PRCP=sum(PRCP,na.rm=TRUE),
                                                   RH =mean(RH,na.rm=TRUE),
                                                   WDSP=mean(WDSP,na.rm=TRUE))

mon_prcp=Yearmon%>% group_by(MONTH) %>% summarise(PRCP=mean(PRCP,na.rm=TRUE))

## Mon

monmean=MAG %>% group_by(MONTH) %>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
                                              MIN=mean(MIN,na.rm=TRUE),
                                              MAX=mean(MAX,na.rm=TRUE), 
                                              PRCP=sum(PRCP,na.rm=TRUE),
                                              RH =mean(RH,na.rm=TRUE),
                                              WDSP=mean(WDSP,na.rm=TRUE))


## Compute anomaly 

# Compute temperature standardised anomaly index
(index_base=subset(Yearly,YEAR >=1976 & YEAR <=2005))

mean_dt= index_base%>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
                                 MIN=mean(MIN,na.rm=TRUE),
                                 MAX=mean(MAX,na.rm=TRUE), 
                                 PRCP=mean(PRCP,na.rm=TRUE),
                                 RH =mean(RH,na.rm=TRUE),
                                 WDSP=mean(WDSP,na.rm=TRUE))

sd_dt= index_base%>% summarise(TEMP=sd(TEMP,na.rm=TRUE),
                               MIN=sd(MIN,na.rm=TRUE),
                               MAX=sd(MAX,na.rm=TRUE), 
                               PRCP=sd(PRCP,na.rm=TRUE),
                               RH =sd(RH,na.rm=TRUE),
                               WDSP=sd(WDSP,na.rm=TRUE))


dt_anom= Yearly %>% group_by(YEAR)%>% summarise(TEMP=((TEMP-mean_dt$TEMP)/(sd_dt$TEMP)),
                                                MIN=((MIN-mean_dt$MIN)/(sd_dt$MIN)),
                                                MAX=((MAX-mean_dt$MAX)/(sd_dt$MAX)), 
                                                PRCP=((PRCP-mean_dt$PRCP)/(sd_dt$PRCP)),
                                                RH =((RH-mean_dt$RH)/(sd_dt$RH)),
                                                WDSP=((WDSP-mean_dt$WDSP)/(sd_dt$WDSP)))


## plotting

### Yearly variation of differents variables


#-----Plot Font------------
windowsFonts(
  A=windowsFont("Arial Black"),
  B=windowsFont("Bookman Old Style"),
  C=windowsFont("Comic Sans MS"),
  D=windowsFont("Symbol")
)
#------------------------#

#----- Temperature---------------------
par(family = "B")

figure_label=paste("tmperature_yearly_",station_name,".png",sep='')
# CairoPNG(file =figure_label,width =613, height =490,bg = "transparent")

png(figure_label, family = "ArialMT", width=613, height=490,pointsize =15)
plot(Yearly$YEAR,Yearly$MAX, type='o',
     main='Trend of temperature - Niamey',xlab='Year',ylab='Temp [Deg]',
     las=2,lwd=3,pch=2,lty=1,col="#984ea3",cex=1,axes=FALSE, ylim=c(15,40))
lines(Yearly$YEAR,Yearly$MIN,type='o',las=1,lwd=3,pch=7,lty=1,cex=1,col="#ff7f00")
lines(Yearly$YEAR,Yearly$TEMP,type='o',las=1,lwd=3,pch=7,lty=1,cex=1,col="#274983")
axis(1,at=seq(min(Yearly$YEAR),max(Yearly$YEAR),5),las=3.5)
axis(2,las=0.8);box(lwd=2)

#grid(5, 5, lwd =0.1,col = "black")
legend("bottomright", legend=c("Tmax","Tmin","Tmean"),
       col=c('#984ea3',"#ff7f00","#274983"),pch=c(2,7),lty=1,cex=1,bty='n')

dev.off()
#-------------------------------------------------------#
graphics.off()

# temperature mean trend

par(family = "B")

figure_label=paste("Mean_temperature_yearly_",station_name,".png",sep='')
# CairoPNG(file =figure_label,width =613, height =490,bg = "transparent")
png(figure_label, family = "ArialMT", width=613, height=490,pointsize =15)
plot(Yearly$YEAR,Yearly$TEMP, type='o',
     main='Trend of Mean temperature - Niamey',xlab='Year',ylab='Temp [Deg]',
     las=2,lwd=3,lty=1,col='#E37831',cex=1)
box(lwd=2)

grid(5, 5, lwd =0.1,col = "black")
legend("bottomright", legend=c("Tmean"),col=c("#ff7f00"),pch=c(1),lty=1,cex=1,bty='n')
lmreg=lm(Yearly$TEMP ~ as.numeric(Yearly$YEAR))
abline(lmreg, col='red',lwd=2)
mtext(paste('y = ',round(lmreg$coefficients[2],2),'x +',round(lmreg$coefficients[1]),
            ' ; R = ',round(summary(lmreg)$r.squared,2),sep=""),cex = 0.8)

dev.off()
graphics.off()

figure_label=paste("boxplot_Month_temperature_obs_",station_name,".png",sep='')

# CairoPNG(file =figure_label,width =613, height =500,bg = "transparent")
png(figure_label, family = "ArialMT", width=613, height=490,pointsize =15)
boxplot(Yearmon$TEMP~Yearmon$MONTH ,
        ylab="[?C]" , xlab="Months", main="Monthly Temperature Trend",
        axes=FALSE)
axis(1,at=c(1:12),labels = month.abb,las=3.5);axis(2,las=2);box()
lines(1:12,monmean$TEMP,col="#ff7f00",lwd=3)
legend_name=paste("Monthly Mean", min(Yearmon$YEAR),"-",max(Yearmon$YEAR))
legend(x="bottomright",legend=legend_name,
       cex = 0.6,lwd = 3, col = "#ff7f00",lty = 1,bty='n')

dev.off()

#-----Month_temperature_past_fut Ploting---------------------

graphics.off()

figure_label=paste("Month_temperature_",station_name,".png",sep='')

# CairoPNG(file =figure_label,width =548, height =428,bg = "transparent")
png(figure_label, family = "ArialMT", width =548, height =428,pointsize =15)
par(family = "B")
plot(rownames(monmean),monmean$TEMP, type='o',
     main='Monthly temperature',xlab='Month',ylab='Temperature [?C]',
     las=2,lwd=3,pch=2,lty=1,col="#377eb8",cex=1,axes=FALSE,ylim=c(10,45))
lines(rownames(monmean),monmean$MAX,type='o',
      las=1,lwd=3,pch=7,lty=1,cex=1,col="#ff7f00")

lines(rownames(monmean),monmean$MIN,type='o',
      las=1,lwd=3,pch=7,lty=1,cex=1,col="#984ea3")

axis(1,at=c(1:12),labels = month.abb,las=3.5);axis(2,cex.axis=0.8);box()
#grid(5, 5, lwd =0.1,col = "black")
legend("bottom", legend=c("Mean", "MAX", "MIN"),
       col=c('#377eb8',"#ff7f00","#984ea3"),pch=c(2,7),lty=1,cex=0.7,bty='n')
dev.off()
#-------------------------------------------------------#

graphics.off()
## Temperature and humidity 

figure_label=paste("Temperature_Humidity_",station_name,".png",sep='')

# CairoPNG(file = figure_label,width =613, height =490,bg = "transparent")

png(figure_label, family = "ArialMT",width =613, height =490,pointsize =15)
par(mar = c(5,4,4, 4) + 0.3)

plot(monmean$MONTH,monmean$TEMP,type='o' ,
     pch = 16, col = 2,xlab='Year', ylab='',axes=FALSE,las=2,lwd=3)
box()
axis(side = 2, at = pretty(range(monmean$TEMP)),col = 2,las=1,lwd=2,cex.axis=0.8)

par(new = TRUE)                            
plot(monmean$MONTH,monmean$RH,type='o', pch = 17, col = 3,              # Create second plot without axes
     axes = FALSE, xlab = "", ylab = "",las=2,lwd=3)
axis(side = 4, at = pretty(range(monmean$RH)),col = 3,las=2)
axis(1,at=c(1:12),labels = month.abb,las=3.5)
mtext("Rh", side = 4, line = 3)
mtext("[?C]", side = 2, line = 3) 
mtext("Monthly variation : Temperature vs Humidity", side = 3, line = 3) 

legend("bottom", legend=c("Temperature","Relative Humidity"),
       col=c(2,3),
       pch=c(16,17),lty=1,lwd=2,cex=0.8,bty='n'
)

dev.off()
#------------------------------------------------------------#

graphics.off()

## Temperature and humidity 

figure_label=paste("Temperature_RAINFALL_",station_name,".png",sep='')
# CairoPNG(file = figure_label,width =613, height =490,bg = "transparent")
png(figure_label, family = "ArialMT",width =613, height =490,pointsize =15)

par(mar = c(5,4,4, 4) + 0.3)

plot(monmean$MONTH,monmean$TEMP,type='o' ,
     pch = 16, col = 2,xlab='Year', ylab='',axes=FALSE,las=2,lwd=3)
box()
axis(side = 2, at = pretty(range(monmean$TEMP)),col = 2,las=1,lwd=2,cex.axis=0.8)

par(new = TRUE)                            
plot(monmean$MONTH,mon_prcp$PRCP,type='o', pch = 17, col = 3,              # Create second plot without axes
     axes = FALSE, xlab = "", ylab = "",las=2,lwd=3)
axis(side = 4, at = pretty(range(mon_prcp$PRCP)),col = 3,las=2)
axis(1,at=c(1:12),labels = month.abb,las=3.5)
mtext("[mm]", side = 4, line = 3)
mtext("[?C]", side = 2, line = 3) 
mtext("Monthly variation : Temperature vs Rainfall", side = 3, line = 3) 

legend("bottom", legend=c("Temperature","Rainfall"),
       col=c(2,3),
       pch=c(16,17),lty=1,lwd=2,cex=0.8,bty='n'
)

dev.off()
#------------------------------------------------------------#


graphics.off()


#----Annual Rainfall: PDSI -----------

figure_label=paste("Annual_rainfall_hist_",station_name,".png",sep='')
# CairoPNG(file =figure_label,width =613, height =490,bg = "transparent")
png(figure_label, family = "ArialMT",width =613, height =490,pointsize =15)

par(family = "B", mar = c(5, 4, 4, 4) + 0.2)
plot(dt_anom$YEAR,dt_anom$PRCP, type='h',
     main='Rainfall Anomaly - Observation',xlab='Year',ylab='Index',
     las=2,lwd=5,lty=1,cex=1,axes=FALSE,ylim=c(-5,5),lend="butt",
     col=ifelse(dt_anom$PRCP>0,'#984ea3','#E37831'))

#lines(obs_rainfall_annual$Year, smooth(obs_rainfall_annual$PSDI),lwd=2)
#lend="butt" to make sure that the bin are not rounded
axis(1,at=seq(min(dt_anom$YEAR),max(dt_anom$YEAR),5),las=3.5,cex.axis=0.8,lwd=2);
axis(2,las=1,cex.axis=0.8)
# segments(1986,0,2020,0)
box(lwd=2);
legend("bottomright", legend=c("Dry Year", "Wet Year"),
       col=c('#E37831',"#984ea3"),pch=c(1,1),lty=1,lwd=2,cex=1,bty='n')

# grid(8,5, lwd =0.05,col = "black")
#grid(5, 5, lwd = par("lwd"),col = "black")
dev.off()
#---------------------------------------------------------------------#

graphics.off()

figure_label=paste("boxplot_Month_rainfall_obs_",station_name,".png",sep='')
# CairoPNG(file =figure_label,width =613, height =500,bg = "transparent")
png(figure_label, family = "ArialMT",width =613, height =500,pointsize =15)
boxplot(Yearmon$PRCP~Yearmon$MONTH ,
        ylab="mm" , xlab="Months", main="Monthly Rainfall Trend",
        axes=FALSE)
axis(1,at=c(1:12),labels = month.abb,las=3.5);axis(2,las=2);box()
lines(1:12,mon_prcp$PRCP,col="#ff7f00",lwd=3)
legend(x="topleft",legend=c("Monthly Mean"),
       cex = 0.6,lwd = 3, col = "#ff7f00",lty = 1,bty='n')

dev.off()

graphics.off()

#----- Humidity---------------------
# par(family = "B",mai=c(4,0.7,0.2,0.2))
figure_label=paste("humididty_yearly_",station_name,".png",sep='')
# CairoPNG(file =figure_label,width =613, height =490,bg = "transparent")
png(figure_label, family = "ArialMT",width =613, height =490,pointsize =15)
plot(Yearly$YEAR,Yearly$RH, type='o',
     main='Relative humidity',xlab='Year',ylab='RH',
     las=2,lwd=3,pch=2,lty=1,col="#984ea3",cex=1,axes=FALSE,ylim=c(min(Yearly$RH)-10,max(Yearly$RH)+10))
axis(1,at=seq(min(Yearly$YEAR),max(Yearly$YEAR),5),las=3.5)
axis(2,las=0.8);box(lwd=2)

#grid(5, 5, lwd =0.1,col = "black")
legend("bottomright", legend=c("RH"),
       col=c('#984ea3',"#ff7f00","#274983"),pch=c(2,7),lty=1,cex=1,bty='n')

dev.off()
#-------------------------------------------------------#
graphics.off()


}

# Retrieve, day, month and year from ts  -- Senegal

for (i in 1:length(unique(dat_Gambia$NAME))){
  
  MAG=dat_Gambia[which(dat_Senegal$NAME==unique(dat_Gambia$NAME)[i]),]
  station_name=unlist(strsplit(unique(MAG$NAME), "[/]"))[1]
  
  
  MAG$PRCP=na_interpolation(MAG$PRCP)
  MAG$MIN=na_interpolation(MAG$MIN)
  MAG$MAX=na_interpolation(MAG$MAX)
  MAG$WDSP=na_interpolation(MAG$WDSP)
  MAG$RH=na_interpolation(MAG$RH)
  
  
  
  MAG$Date=ymd(paste(MAG$YEAR,MAG$MONTH,MAG$DAY,sep='-'))
  
  
  Yearly=MAG %>% group_by(YEAR) %>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
                                              MIN=mean(MIN,na.rm=TRUE),
                                              MAX=mean(MAX,na.rm=TRUE), 
                                              PRCP=sum(PRCP,na.rm=TRUE),
                                              RH =mean(RH,na.rm=TRUE),
                                              WDSP=mean(WDSP,na.rm=TRUE))
  
  ## YearMon
  Yearmon=MAG %>% group_by(YEAR,MONTH) %>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
                                                     MIN=mean(MIN,na.rm=TRUE),
                                                     MAX=mean(MAX,na.rm=TRUE), 
                                                     PRCP=sum(PRCP,na.rm=TRUE),
                                                     RH =mean(RH,na.rm=TRUE),
                                                     WDSP=mean(WDSP,na.rm=TRUE))
  
  mon_prcp=Yearmon%>% group_by(MONTH) %>% summarise(PRCP=mean(PRCP,na.rm=TRUE))
  
  ## Mon
  
  monmean=MAG %>% group_by(MONTH) %>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
                                                MIN=mean(MIN,na.rm=TRUE),
                                                MAX=mean(MAX,na.rm=TRUE), 
                                                PRCP=sum(PRCP,na.rm=TRUE),
                                                RH =mean(RH,na.rm=TRUE),
                                                WDSP=mean(WDSP,na.rm=TRUE))
  
  
  ## Compute anomaly 
  
  # Compute temperature standardised anomaly index
  (index_base=subset(Yearly,YEAR >=1976 & YEAR <=2005))
  
  mean_dt= index_base%>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
                                   MIN=mean(MIN,na.rm=TRUE),
                                   MAX=mean(MAX,na.rm=TRUE), 
                                   PRCP=mean(PRCP,na.rm=TRUE),
                                   RH =mean(RH,na.rm=TRUE),
                                   WDSP=mean(WDSP,na.rm=TRUE))
  
  sd_dt= index_base%>% summarise(TEMP=sd(TEMP,na.rm=TRUE),
                                 MIN=sd(MIN,na.rm=TRUE),
                                 MAX=sd(MAX,na.rm=TRUE), 
                                 PRCP=sd(PRCP,na.rm=TRUE),
                                 RH =sd(RH,na.rm=TRUE),
                                 WDSP=sd(WDSP,na.rm=TRUE))
  
  
  dt_anom= Yearly %>% group_by(YEAR)%>% summarise(TEMP=((TEMP-mean_dt$TEMP)/(sd_dt$TEMP)),
                                                  MIN=((MIN-mean_dt$MIN)/(sd_dt$MIN)),
                                                  MAX=((MAX-mean_dt$MAX)/(sd_dt$MAX)), 
                                                  PRCP=((PRCP-mean_dt$PRCP)/(sd_dt$PRCP)),
                                                  RH =((RH-mean_dt$RH)/(sd_dt$RH)),
                                                  WDSP=((WDSP-mean_dt$WDSP)/(sd_dt$WDSP)))
  
  
  ## plotting
  
  ### Yearly variation of differents variables
  
  
  #-----Plot Font------------
  windowsFonts(
    A=windowsFont("Arial Black"),
    B=windowsFont("Bookman Old Style"),
    C=windowsFont("Comic Sans MS"),
    D=windowsFont("Symbol")
  )
  #------------------------#
  
  #----- Temperature---------------------
  par(family = "B")
  
  figure_label=paste("tmperature_yearly_",station_name,".png",sep='')
  # CairoPNG(file =figure_label,width =613, height =490,bg = "transparent")
  
  png(figure_label, family = "ArialMT", width=613, height=490,pointsize =15)
  plot(Yearly$YEAR,Yearly$MAX, type='o',
       main='Trend of temperature - Niamey',xlab='Year',ylab='Temp [Deg]',
       las=2,lwd=3,pch=2,lty=1,col="#984ea3",cex=1,axes=FALSE, ylim=c(15,40))
  lines(Yearly$YEAR,Yearly$MIN,type='o',las=1,lwd=3,pch=7,lty=1,cex=1,col="#ff7f00")
  lines(Yearly$YEAR,Yearly$TEMP,type='o',las=1,lwd=3,pch=7,lty=1,cex=1,col="#274983")
  axis(1,at=seq(min(Yearly$YEAR),max(Yearly$YEAR),5),las=3.5)
  axis(2,las=0.8);box(lwd=2)
  
  #grid(5, 5, lwd =0.1,col = "black")
  legend("bottomright", legend=c("Tmax","Tmin","Tmean"),
         col=c('#984ea3',"#ff7f00","#274983"),pch=c(2,7),lty=1,cex=1,bty='n')
  
  dev.off()
  #-------------------------------------------------------#
  graphics.off()
  
  # temperature mean trend
  
  par(family = "B")
  
  figure_label=paste("Mean_temperature_yearly_",station_name,".png",sep='')
  # CairoPNG(file =figure_label,width =613, height =490,bg = "transparent")
  png(figure_label, family = "ArialMT", width=613, height=490,pointsize =15)
  plot(Yearly$YEAR,Yearly$TEMP, type='o',
       main='Trend of Mean temperature - Niamey',xlab='Year',ylab='Temp [Deg]',
       las=2,lwd=3,lty=1,col='#E37831',cex=1)
  box(lwd=2)
  
  grid(5, 5, lwd =0.1,col = "black")
  legend("bottomright", legend=c("Tmean"),col=c("#ff7f00"),pch=c(1),lty=1,cex=1,bty='n')
  lmreg=lm(Yearly$TEMP ~ as.numeric(Yearly$YEAR))
  abline(lmreg, col='red',lwd=2)
  mtext(paste('y = ',round(lmreg$coefficients[2],2),'x +',round(lmreg$coefficients[1]),
              ' ; R = ',round(summary(lmreg)$r.squared,2),sep=""),cex = 0.8)
  
  dev.off()
  graphics.off()
  
  figure_label=paste("boxplot_Month_temperature_obs_",station_name,".png",sep='')
  
  # CairoPNG(file =figure_label,width =613, height =500,bg = "transparent")
  png(figure_label, family = "ArialMT", width=613, height=490,pointsize =15)
  boxplot(Yearmon$TEMP~Yearmon$MONTH ,
          ylab="[?C]" , xlab="Months", main="Monthly Temperature Trend",
          axes=FALSE)
  axis(1,at=c(1:12),labels = month.abb,las=3.5);axis(2,las=2);box()
  lines(1:12,monmean$TEMP,col="#ff7f00",lwd=3)
  legend_name=paste("Monthly Mean", min(Yearmon$YEAR),"-",max(Yearmon$YEAR))
  legend(x="bottomright",legend=legend_name,
         cex = 0.6,lwd = 3, col = "#ff7f00",lty = 1,bty='n')
  
  dev.off()
  
  #-----Month_temperature_past_fut Ploting---------------------
  
  graphics.off()
  
  figure_label=paste("Month_temperature_",station_name,".png",sep='')
  
  # CairoPNG(file =figure_label,width =548, height =428,bg = "transparent")
  png(figure_label, family = "ArialMT", width =548, height =428,pointsize =15)
  par(family = "B")
  plot(rownames(monmean),monmean$TEMP, type='o',
       main='Monthly temperature',xlab='Month',ylab='Temperature [?C]',
       las=2,lwd=3,pch=2,lty=1,col="#377eb8",cex=1,axes=FALSE,ylim=c(10,45))
  lines(rownames(monmean),monmean$MAX,type='o',
        las=1,lwd=3,pch=7,lty=1,cex=1,col="#ff7f00")
  
  lines(rownames(monmean),monmean$MIN,type='o',
        las=1,lwd=3,pch=7,lty=1,cex=1,col="#984ea3")
  
  axis(1,at=c(1:12),labels = month.abb,las=3.5);axis(2,cex.axis=0.8);box()
  #grid(5, 5, lwd =0.1,col = "black")
  legend("bottom", legend=c("Mean", "MAX", "MIN"),
         col=c('#377eb8',"#ff7f00","#984ea3"),pch=c(2,7),lty=1,cex=0.7,bty='n')
  dev.off()
  #-------------------------------------------------------#
  
  graphics.off()
  ## Temperature and humidity 
  
  figure_label=paste("Temperature_Humidity_",station_name,".png",sep='')
  
  # CairoPNG(file = figure_label,width =613, height =490,bg = "transparent")
  
  png(figure_label, family = "ArialMT",width =613, height =490,pointsize =15)
  par(mar = c(5,4,4, 4) + 0.3)
  
  plot(monmean$MONTH,monmean$TEMP,type='o' ,
       pch = 16, col = 2,xlab='Year', ylab='',axes=FALSE,las=2,lwd=3)
  box()
  axis(side = 2, at = pretty(range(monmean$TEMP)),col = 2,las=1,lwd=2,cex.axis=0.8)
  
  par(new = TRUE)                            
  plot(monmean$MONTH,monmean$RH,type='o', pch = 17, col = 3,              # Create second plot without axes
       axes = FALSE, xlab = "", ylab = "",las=2,lwd=3)
  axis(side = 4, at = pretty(range(monmean$RH)),col = 3,las=2)
  axis(1,at=c(1:12),labels = month.abb,las=3.5)
  mtext("Rh", side = 4, line = 3)
  mtext("[?C]", side = 2, line = 3) 
  mtext("Monthly variation : Temperature vs Humidity", side = 3, line = 3) 
  
  legend("bottom", legend=c("Temperature","Relative Humidity"),
         col=c(2,3),
         pch=c(16,17),lty=1,lwd=2,cex=0.8,bty='n'
  )
  
  dev.off()
  #------------------------------------------------------------#
  
  graphics.off()
  
  ## Temperature and humidity 
  
  figure_label=paste("Temperature_RAINFALL_",station_name,".png",sep='')
  # CairoPNG(file = figure_label,width =613, height =490,bg = "transparent")
  png(figure_label, family = "ArialMT",width =613, height =490,pointsize =15)
  
  par(mar = c(5,4,4, 4) + 0.3)
  
  plot(monmean$MONTH,monmean$TEMP,type='o' ,
       pch = 16, col = 2,xlab='Year', ylab='',axes=FALSE,las=2,lwd=3)
  box()
  axis(side = 2, at = pretty(range(monmean$TEMP)),col = 2,las=1,lwd=2,cex.axis=0.8)
  
  par(new = TRUE)                            
  plot(monmean$MONTH,mon_prcp$PRCP,type='o', pch = 17, col = 3,              # Create second plot without axes
       axes = FALSE, xlab = "", ylab = "",las=2,lwd=3)
  axis(side = 4, at = pretty(range(mon_prcp$PRCP)),col = 3,las=2)
  axis(1,at=c(1:12),labels = month.abb,las=3.5)
  mtext("[mm]", side = 4, line = 3)
  mtext("[?C]", side = 2, line = 3) 
  mtext("Monthly variation : Temperature vs Rainfall", side = 3, line = 3) 
  
  legend("bottom", legend=c("Temperature","Rainfall"),
         col=c(2,3),
         pch=c(16,17),lty=1,lwd=2,cex=0.8,bty='n'
  )
  
  dev.off()
  #------------------------------------------------------------#
  
  
  graphics.off()
  
  
  #----Annual Rainfall: PDSI -----------
  
  figure_label=paste("Annual_rainfall_hist_",station_name,".png",sep='')
  # CairoPNG(file =figure_label,width =613, height =490,bg = "transparent")
  png(figure_label, family = "ArialMT",width =613, height =490,pointsize =15)
  
  par(family = "B", mar = c(5, 4, 4, 4) + 0.2)
  plot(dt_anom$YEAR,dt_anom$PRCP, type='h',
       main='Rainfall Anomaly - Observation',xlab='Year',ylab='Index',
       las=2,lwd=5,lty=1,cex=1,axes=FALSE,ylim=c(-5,5),lend="butt",
       col=ifelse(dt_anom$PRCP>0,'#984ea3','#E37831'))
  
  #lines(obs_rainfall_annual$Year, smooth(obs_rainfall_annual$PSDI),lwd=2)
  #lend="butt" to make sure that the bin are not rounded
  axis(1,at=seq(min(dt_anom$YEAR),max(dt_anom$YEAR),5),las=3.5,cex.axis=0.8,lwd=2);
  axis(2,las=1,cex.axis=0.8)
  # segments(1986,0,2020,0)
  box(lwd=2);
  legend("bottomright", legend=c("Dry Year", "Wet Year"),
         col=c('#E37831',"#984ea3"),pch=c(1,1),lty=1,lwd=2,cex=1,bty='n')
  
  # grid(8,5, lwd =0.05,col = "black")
  #grid(5, 5, lwd = par("lwd"),col = "black")
  dev.off()
  #---------------------------------------------------------------------#
  
  graphics.off()
  
  figure_label=paste("boxplot_Month_rainfall_obs_",station_name,".png",sep='')
  # CairoPNG(file =figure_label,width =613, height =500,bg = "transparent")
  png(figure_label, family = "ArialMT",width =613, height =500,pointsize =15)
  boxplot(Yearmon$PRCP~Yearmon$MONTH ,
          ylab="mm" , xlab="Months", main="Monthly Rainfall Trend",
          axes=FALSE)
  axis(1,at=c(1:12),labels = month.abb,las=3.5);axis(2,las=2);box()
  lines(1:12,mon_prcp$PRCP,col="#ff7f00",lwd=3)
  legend(x="topleft",legend=c("Monthly Mean"),
         cex = 0.6,lwd = 3, col = "#ff7f00",lty = 1,bty='n')
  
  dev.off()
  
  graphics.off()
  
  #----- Humidity---------------------
  # par(family = "B",mai=c(4,0.7,0.2,0.2))
  figure_label=paste("humididty_yearly_",station_name,".png",sep='')
  # CairoPNG(file =figure_label,width =613, height =490,bg = "transparent")
  png(figure_label, family = "ArialMT",width =613, height =490,pointsize =15)
  plot(Yearly$YEAR,Yearly$RH, type='o',
       main='Relative humidity',xlab='Year',ylab='RH',
       las=2,lwd=3,pch=2,lty=1,col="#984ea3",cex=1,axes=FALSE,ylim=c(min(Yearly$RH)-10,max(Yearly$RH)+10))
  axis(1,at=seq(min(Yearly$YEAR),max(Yearly$YEAR),5),las=3.5)
  axis(2,las=0.8);box(lwd=2)
  
  #grid(5, 5, lwd =0.1,col = "black")
  legend("bottomright", legend=c("RH"),
         col=c('#984ea3',"#ff7f00","#274983"),pch=c(2,7),lty=1,cex=1,bty='n')
  
  dev.off()
  #-------------------------------------------------------#
  graphics.off()
  
  
}

# Retrieve, day, month and year from ts  -- Togo
for (i in 1:length(unique(dat_Togo$NAME))){
  
  MAG=dat_Togo[which(dat_Togo$NAME==unique(dat_Togo$NAME)[i]),]
  station_name=unlist(strsplit(unique(MAG$NAME), "[/]"))[1]
  
  
  MAG$PRCP=na_interpolation(MAG$PRCP)
  MAG$MIN=na_interpolation(MAG$MIN)
  MAG$MAX=na_interpolation(MAG$MAX)
  MAG$WDSP=na_interpolation(MAG$WDSP)
  MAG$RH=na_interpolation(MAG$RH)
  
  
  
  MAG$Date=ymd(paste(MAG$YEAR,MAG$MONTH,MAG$DAY,sep='-'))
  
  
  Yearly=MAG %>% group_by(YEAR) %>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
                                              MIN=mean(MIN,na.rm=TRUE),
                                              MAX=mean(MAX,na.rm=TRUE), 
                                              PRCP=sum(PRCP,na.rm=TRUE),
                                              RH =mean(RH,na.rm=TRUE),
                                              WDSP=mean(WDSP,na.rm=TRUE))
  
  ## YearMon
  Yearmon=MAG %>% group_by(YEAR,MONTH) %>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
                                                     MIN=mean(MIN,na.rm=TRUE),
                                                     MAX=mean(MAX,na.rm=TRUE), 
                                                     PRCP=sum(PRCP,na.rm=TRUE),
                                                     RH =mean(RH,na.rm=TRUE),
                                                     WDSP=mean(WDSP,na.rm=TRUE))
  
  mon_prcp=Yearmon%>% group_by(MONTH) %>% summarise(PRCP=mean(PRCP,na.rm=TRUE))
  
  ## Mon
  
  monmean=MAG %>% group_by(MONTH) %>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
                                                MIN=mean(MIN,na.rm=TRUE),
                                                MAX=mean(MAX,na.rm=TRUE), 
                                                PRCP=sum(PRCP,na.rm=TRUE),
                                                RH =mean(RH,na.rm=TRUE),
                                                WDSP=mean(WDSP,na.rm=TRUE))
  
  
  ## Compute anomaly 
  
  # Compute temperature standardised anomaly index
  (index_base=subset(Yearly,YEAR >=1976 & YEAR <=2005))
  
  mean_dt= index_base%>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
                                   MIN=mean(MIN,na.rm=TRUE),
                                   MAX=mean(MAX,na.rm=TRUE), 
                                   PRCP=mean(PRCP,na.rm=TRUE),
                                   RH =mean(RH,na.rm=TRUE),
                                   WDSP=mean(WDSP,na.rm=TRUE))
  
  sd_dt= index_base%>% summarise(TEMP=sd(TEMP,na.rm=TRUE),
                                 MIN=sd(MIN,na.rm=TRUE),
                                 MAX=sd(MAX,na.rm=TRUE), 
                                 PRCP=sd(PRCP,na.rm=TRUE),
                                 RH =sd(RH,na.rm=TRUE),
                                 WDSP=sd(WDSP,na.rm=TRUE))
  
  
  dt_anom= Yearly %>% group_by(YEAR)%>% summarise(TEMP=((TEMP-mean_dt$TEMP)/(sd_dt$TEMP)),
                                                  MIN=((MIN-mean_dt$MIN)/(sd_dt$MIN)),
                                                  MAX=((MAX-mean_dt$MAX)/(sd_dt$MAX)), 
                                                  PRCP=((PRCP-mean_dt$PRCP)/(sd_dt$PRCP)),
                                                  RH =((RH-mean_dt$RH)/(sd_dt$RH)),
                                                  WDSP=((WDSP-mean_dt$WDSP)/(sd_dt$WDSP)))
  
  
  ## plotting
  
  ### Yearly variation of differents variables
  
  
  #-----Plot Font------------
  windowsFonts(
    A=windowsFont("Arial Black"),
    B=windowsFont("Bookman Old Style"),
    C=windowsFont("Comic Sans MS"),
    D=windowsFont("Symbol")
  )
  #------------------------#
  
  #----- Temperature---------------------
  par(family = "B")
  
  figure_label=paste("tmperature_yearly_",station_name,".png",sep='')
  # CairoPNG(file =figure_label,width =613, height =490,bg = "transparent")
  
  png(figure_label, family = "ArialMT", width=613, height=490,pointsize =15)
  plot(Yearly$YEAR,Yearly$MAX, type='o',
       main='Trend of temperature - Niamey',xlab='Year',ylab='Temp [Deg]',
       las=2,lwd=3,pch=2,lty=1,col="#984ea3",cex=1,axes=FALSE, ylim=c(15,40))
  lines(Yearly$YEAR,Yearly$MIN,type='o',las=1,lwd=3,pch=7,lty=1,cex=1,col="#ff7f00")
  lines(Yearly$YEAR,Yearly$TEMP,type='o',las=1,lwd=3,pch=7,lty=1,cex=1,col="#274983")
  axis(1,at=seq(min(Yearly$YEAR),max(Yearly$YEAR),5),las=3.5)
  axis(2,las=0.8);box(lwd=2)
  
  #grid(5, 5, lwd =0.1,col = "black")
  legend("bottomright", legend=c("Tmax","Tmin","Tmean"),
         col=c('#984ea3',"#ff7f00","#274983"),pch=c(2,7),lty=1,cex=1,bty='n')
  
  dev.off()
  #-------------------------------------------------------#
  graphics.off()
  
  # temperature mean trend
  
  par(family = "B")
  
  figure_label=paste("Mean_temperature_yearly_",station_name,".png",sep='')
  # CairoPNG(file =figure_label,width =613, height =490,bg = "transparent")
  png(figure_label, family = "ArialMT", width=613, height=490,pointsize =15)
  plot(Yearly$YEAR,Yearly$TEMP, type='o',
       main='Trend of Mean temperature - Niamey',xlab='Year',ylab='Temp [Deg]',
       las=2,lwd=3,lty=1,col='#E37831',cex=1)
  box(lwd=2)
  
  grid(5, 5, lwd =0.1,col = "black")
  legend("bottomright", legend=c("Tmean"),col=c("#ff7f00"),pch=c(1),lty=1,cex=1,bty='n')
  lmreg=lm(Yearly$TEMP ~ as.numeric(Yearly$YEAR))
  abline(lmreg, col='red',lwd=2)
  mtext(paste('y = ',round(lmreg$coefficients[2],2),'x +',round(lmreg$coefficients[1]),
              ' ; R = ',round(summary(lmreg)$r.squared,2),sep=""),cex = 0.8)
  
  dev.off()
  graphics.off()
  
  figure_label=paste("boxplot_Month_temperature_obs_",station_name,".png",sep='')
  
  # CairoPNG(file =figure_label,width =613, height =500,bg = "transparent")
  png(figure_label, family = "ArialMT", width=613, height=490,pointsize =15)
  boxplot(Yearmon$TEMP~Yearmon$MONTH ,
          ylab="[?C]" , xlab="Months", main="Monthly Temperature Trend",
          axes=FALSE)
  axis(1,at=c(1:12),labels = month.abb,las=3.5);axis(2,las=2);box()
  lines(1:12,monmean$TEMP,col="#ff7f00",lwd=3)
  legend_name=paste("Monthly Mean", min(Yearmon$YEAR),"-",max(Yearmon$YEAR))
  legend(x="bottomright",legend=legend_name,
         cex = 0.6,lwd = 3, col = "#ff7f00",lty = 1,bty='n')
  
  dev.off()
  
  #-----Month_temperature_past_fut Ploting---------------------
  
  graphics.off()
  
  figure_label=paste("Month_temperature_",station_name,".png",sep='')
  
  # CairoPNG(file =figure_label,width =548, height =428,bg = "transparent")
  png(figure_label, family = "ArialMT", width =548, height =428,pointsize =15)
  par(family = "B")
  plot(rownames(monmean),monmean$TEMP, type='o',
       main='Monthly temperature',xlab='Month',ylab='Temperature [?C]',
       las=2,lwd=3,pch=2,lty=1,col="#377eb8",cex=1,axes=FALSE,ylim=c(10,45))
  lines(rownames(monmean),monmean$MAX,type='o',
        las=1,lwd=3,pch=7,lty=1,cex=1,col="#ff7f00")
  
  lines(rownames(monmean),monmean$MIN,type='o',
        las=1,lwd=3,pch=7,lty=1,cex=1,col="#984ea3")
  
  axis(1,at=c(1:12),labels = month.abb,las=3.5);axis(2,cex.axis=0.8);box()
  #grid(5, 5, lwd =0.1,col = "black")
  legend("bottom", legend=c("Mean", "MAX", "MIN"),
         col=c('#377eb8',"#ff7f00","#984ea3"),pch=c(2,7),lty=1,cex=0.7,bty='n')
  dev.off()
  #-------------------------------------------------------#
  
  graphics.off()
  ## Temperature and humidity 
  
  figure_label=paste("Temperature_Humidity_",station_name,".png",sep='')
  
  # CairoPNG(file = figure_label,width =613, height =490,bg = "transparent")
  
  png(figure_label, family = "ArialMT",width =613, height =490,pointsize =15)
  par(mar = c(5,4,4, 4) + 0.3)
  
  plot(monmean$MONTH,monmean$TEMP,type='o' ,
       pch = 16, col = 2,xlab='Year', ylab='',axes=FALSE,las=2,lwd=3)
  box()
  axis(side = 2, at = pretty(range(monmean$TEMP)),col = 2,las=1,lwd=2,cex.axis=0.8)
  
  par(new = TRUE)                            
  plot(monmean$MONTH,monmean$RH,type='o', pch = 17, col = 3,              # Create second plot without axes
       axes = FALSE, xlab = "", ylab = "",las=2,lwd=3)
  axis(side = 4, at = pretty(range(monmean$RH)),col = 3,las=2)
  axis(1,at=c(1:12),labels = month.abb,las=3.5)
  mtext("Rh", side = 4, line = 3)
  mtext("[?C]", side = 2, line = 3) 
  mtext("Monthly variation : Temperature vs Humidity", side = 3, line = 3) 
  
  legend("bottom", legend=c("Temperature","Relative Humidity"),
         col=c(2,3),
         pch=c(16,17),lty=1,lwd=2,cex=0.8,bty='n'
  )
  
  dev.off()
  #------------------------------------------------------------#
  
  graphics.off()
  
  ## Temperature and humidity 
  
  figure_label=paste("Temperature_RAINFALL_",station_name,".png",sep='')
  # CairoPNG(file = figure_label,width =613, height =490,bg = "transparent")
  png(figure_label, family = "ArialMT",width =613, height =490,pointsize =15)
  
  par(mar = c(5,4,4, 4) + 0.3)
  
  plot(monmean$MONTH,monmean$TEMP,type='o' ,
       pch = 16, col = 2,xlab='Year', ylab='',axes=FALSE,las=2,lwd=3)
  box()
  axis(side = 2, at = pretty(range(monmean$TEMP)),col = 2,las=1,lwd=2,cex.axis=0.8)
  
  par(new = TRUE)                            
  plot(monmean$MONTH,mon_prcp$PRCP,type='o', pch = 17, col = 3,              # Create second plot without axes
       axes = FALSE, xlab = "", ylab = "",las=2,lwd=3)
  axis(side = 4, at = pretty(range(mon_prcp$PRCP)),col = 3,las=2)
  axis(1,at=c(1:12),labels = month.abb,las=3.5)
  mtext("[mm]", side = 4, line = 3)
  mtext("[?C]", side = 2, line = 3) 
  mtext("Monthly variation : Temperature vs Rainfall", side = 3, line = 3) 
  
  legend("bottom", legend=c("Temperature","Rainfall"),
         col=c(2,3),
         pch=c(16,17),lty=1,lwd=2,cex=0.8,bty='n'
  )
  
  dev.off()
  #------------------------------------------------------------#
  
  
  graphics.off()
  
  
  #----Annual Rainfall: PDSI -----------
  
  figure_label=paste("Annual_rainfall_hist_",station_name,".png",sep='')
  # CairoPNG(file =figure_label,width =613, height =490,bg = "transparent")
  png(figure_label, family = "ArialMT",width =613, height =490,pointsize =15)
  
  par(family = "B", mar = c(5, 4, 4, 4) + 0.2)
  plot(dt_anom$YEAR,dt_anom$PRCP, type='h',
       main='Rainfall Anomaly - Observation',xlab='Year',ylab='Index',
       las=2,lwd=5,lty=1,cex=1,axes=FALSE,ylim=c(-5,5),lend="butt",
       col=ifelse(dt_anom$PRCP>0,'#984ea3','#E37831'))
  
  #lines(obs_rainfall_annual$Year, smooth(obs_rainfall_annual$PSDI),lwd=2)
  #lend="butt" to make sure that the bin are not rounded
  axis(1,at=seq(min(dt_anom$YEAR),max(dt_anom$YEAR),5),las=3.5,cex.axis=0.8,lwd=2);
  axis(2,las=1,cex.axis=0.8)
  # segments(1986,0,2020,0)
  box(lwd=2);
  legend("bottomright", legend=c("Dry Year", "Wet Year"),
         col=c('#E37831',"#984ea3"),pch=c(1,1),lty=1,lwd=2,cex=1,bty='n')
  
  # grid(8,5, lwd =0.05,col = "black")
  #grid(5, 5, lwd = par("lwd"),col = "black")
  dev.off()
  #---------------------------------------------------------------------#
  
  graphics.off()
  
  figure_label=paste("boxplot_Month_rainfall_obs_",station_name,".png",sep='')
  # CairoPNG(file =figure_label,width =613, height =500,bg = "transparent")
  png(figure_label, family = "ArialMT",width =613, height =500,pointsize =15)
  boxplot(Yearmon$PRCP~Yearmon$MONTH ,
          ylab="mm" , xlab="Months", main="Monthly Rainfall Trend",
          axes=FALSE)
  axis(1,at=c(1:12),labels = month.abb,las=3.5);axis(2,las=2);box()
  lines(1:12,mon_prcp$PRCP,col="#ff7f00",lwd=3)
  legend(x="topleft",legend=c("Monthly Mean"),
         cex = 0.6,lwd = 3, col = "#ff7f00",lty = 1,bty='n')
  
  dev.off()
  
  graphics.off()
  
  #----- Humidity---------------------
  # par(family = "B",mai=c(4,0.7,0.2,0.2))
  figure_label=paste("humididty_yearly_",station_name,".png",sep='')
  # CairoPNG(file =figure_label,width =613, height =490,bg = "transparent")
  png(figure_label, family = "ArialMT",width =613, height =490,pointsize =15)
  plot(Yearly$YEAR,Yearly$RH, type='o',
       main='Relative humidity',xlab='Year',ylab='RH',
       las=2,lwd=3,pch=2,lty=1,col="#984ea3",cex=1,axes=FALSE,ylim=c(min(Yearly$RH)-10,max(Yearly$RH)+10))
  axis(1,at=seq(min(Yearly$YEAR),max(Yearly$YEAR),5),las=3.5)
  axis(2,las=0.8);box(lwd=2)
  
  #grid(5, 5, lwd =0.1,col = "black")
  legend("bottomright", legend=c("RH"),
         col=c('#984ea3',"#ff7f00","#274983"),pch=c(2,7),lty=1,cex=1,bty='n')
  
  dev.off()
  #-------------------------------------------------------#
  graphics.off()
  
  
}


# Retrieve, day, month and year from ts  -- Togo
# 
# for (i in 1:length(unique(dat_Togo$NAME))){
#   
#   MAG=dat_Togo[which(dat_Togo$NAME==unique(dat_Togo$NAME)[i]),]
#   station_name=unlist(strsplit(unique(MAG$NAME), "[/]"))[1]
#   
#   
#   MAG$PRCP=na_interpolation(MAG$PRCP)
#   MAG$MIN=na_interpolation(MAG$MIN)
#   MAG$MAX=na_interpolation(MAG$MAX)
#   MAG$WDSP=na_interpolation(MAG$WDSP)
#   MAG$RH=na_interpolation(MAG$RH)
#   
#   
#   
#   MAG$Date=ymd(paste(MAG$YEAR,MAG$MONTH,MAG$DAY,sep='-'))
#   
#   
#   Yearly=MAG %>% group_by(YEAR) %>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
#                                               MIN=mean(MIN,na.rm=TRUE),
#                                               MAX=mean(MAX,na.rm=TRUE), 
#                                               PRCP=sum(PRCP,na.rm=TRUE),
#                                               RH =mean(RH,na.rm=TRUE),
#                                               WDSP=mean(WDSP,na.rm=TRUE))
#   
#   ## YearMon
#   Yearmon=MAG %>% group_by(YEAR,MONTH) %>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
#                                                      MIN=mean(MIN,na.rm=TRUE),
#                                                      MAX=mean(MAX,na.rm=TRUE), 
#                                                      PRCP=sum(PRCP,na.rm=TRUE),
#                                                      RH =mean(RH,na.rm=TRUE),
#                                                      WDSP=mean(WDSP,na.rm=TRUE))
#   
#   mon_prcp=Yearmon%>% group_by(MONTH) %>% summarise(PRCP=mean(PRCP,na.rm=TRUE))
#   
#   ## Mon
#   
#   monmean=MAG %>% group_by(MONTH) %>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
#                                                 MIN=mean(MIN,na.rm=TRUE),
#                                                 MAX=mean(MAX,na.rm=TRUE), 
#                                                 PRCP=sum(PRCP,na.rm=TRUE),
#                                                 RH =mean(RH,na.rm=TRUE),
#                                                 WDSP=mean(WDSP,na.rm=TRUE))
#   
#   
#   ## Compute anomaly 
#   
#   # Compute temperature standardised anomaly index
#   (index_base=subset(Yearly,YEAR >=1976 & YEAR <=2005))
#   
#   mean_dt= index_base%>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
#                                    MIN=mean(MIN,na.rm=TRUE),
#                                    MAX=mean(MAX,na.rm=TRUE), 
#                                    PRCP=mean(PRCP,na.rm=TRUE),
#                                    RH =mean(RH,na.rm=TRUE),
#                                    WDSP=mean(WDSP,na.rm=TRUE))
#   
#   sd_dt= index_base%>% summarise(TEMP=sd(TEMP,na.rm=TRUE),
#                                  MIN=sd(MIN,na.rm=TRUE),
#                                  MAX=sd(MAX,na.rm=TRUE), 
#                                  PRCP=sd(PRCP,na.rm=TRUE),
#                                  RH =sd(RH,na.rm=TRUE),
#                                  WDSP=sd(WDSP,na.rm=TRUE))
#   
#   
#   dt_anom= Yearly %>% group_by(YEAR)%>% summarise(TEMP=((TEMP-mean_dt$TEMP)/(sd_dt$TEMP)),
#                                                   MIN=((MIN-mean_dt$MIN)/(sd_dt$MIN)),
#                                                   MAX=((MAX-mean_dt$MAX)/(sd_dt$MAX)), 
#                                                   PRCP=((PRCP-mean_dt$PRCP)/(sd_dt$PRCP)),
#                                                   RH =((RH-mean_dt$RH)/(sd_dt$RH)),
#                                                   WDSP=((WDSP-mean_dt$WDSP)/(sd_dt$WDSP)))
#   
#   
#   ## plotting
#   
#   ### Yearly variation of differents variables
#   
#   
#   #-----Plot Font------------
#   windowsFonts(
#     A=windowsFont("Arial Black"),
#     B=windowsFont("Bookman Old Style"),
#     C=windowsFont("Comic Sans MS"),
#     D=windowsFont("Symbol")
#   )
#   #------------------------#
#   
#   #----- Temperature---------------------
#   par(family = "B")
#   
#   figure_label=paste("tmperature_yearly_",station_name,".png",sep='')
#   CairoPNG(file =figure_label,width =613, height =490,bg = "transparent")
#   plot(Yearly$YEAR,Yearly$MAX, type='o',
#        main='Trend of temperature - Niamey',xlab='Year',ylab='Temp [Deg]',
#        las=2,lwd=3,pch=2,lty=1,col="#984ea3",cex=1,axes=FALSE, ylim=c(15,40))
#   lines(Yearly$YEAR,Yearly$MIN,type='o',las=1,lwd=3,pch=7,lty=1,cex=1,col="#ff7f00")
#   lines(Yearly$YEAR,Yearly$TEMP,type='o',las=1,lwd=3,pch=7,lty=1,cex=1,col="#274983")
#   axis(1,at=seq(min(Yearly$YEAR),max(Yearly$YEAR),5),las=3.5)
#   axis(2,las=0.8);box(lwd=2)
#   
#   #grid(5, 5, lwd =0.1,col = "black")
#   legend("bottomright", legend=c("Tmax","Tmin","Tmean"),
#          col=c('#984ea3',"#ff7f00","#274983"),pch=c(2,7),lty=1,cex=1,bty='n')
#   
#   dev.off()
#   #-------------------------------------------------------#
#   graphics.off()
#   
#   # temperature mean trend
#   
#   par(family = "B")
#   
#   figure_label=paste("Mean_temperature_yearly_",station_name,".png",sep='')
#   CairoPNG(file =figure_label,width =613, height =490,bg = "transparent")
#   
#   plot(Yearly$YEAR,Yearly$TEMP, type='o',
#        main='Trend of Mean temperature - Niamey',xlab='Year',ylab='Temp [Deg]',
#        las=2,lwd=3,lty=1,col='#E37831',cex=1)
#   box(lwd=2)
#   
#   grid(5, 5, lwd =0.1,col = "black")
#   legend("bottomright", legend=c("Tmean"),col=c("#ff7f00"),pch=c(1),lty=1,cex=1,bty='n')
#   lmreg=lm(Yearly$TEMP ~ as.numeric(Yearly$YEAR))
#   abline(lmreg, col='red',lwd=2)
#   mtext(paste('y = ',round(lmreg$coefficients[2],2),'x +',round(lmreg$coefficients[1]),
#               ' ; R = ',round(summary(lmreg)$r.squared,2),sep=""),cex = 0.8)
#   
#   dev.off()
#   graphics.off()
#   
#   figure_label=paste("boxplot_Month_temperature_obs_",station_name,".png",sep='')
#   
#   CairoPNG(file =figure_label,width =613, height =500,bg = "transparent")
#   boxplot(Yearmon$TEMP~Yearmon$MONTH ,
#           ylab="[?C]" , xlab="Months", main="Monthly Temperature Trend",
#           axes=FALSE)
#   axis(1,at=c(1:12),labels = month.abb,las=3.5);axis(2,las=2);box()
#   lines(1:12,monmean$TEMP,col="#ff7f00",lwd=3)
#   legend_name=paste("Monthly Mean", min(Yearmon$YEAR),"-",max(Yearmon$YEAR))
#   legend(x="bottomright",legend=legend_name,
#          cex = 0.6,lwd = 3, col = "#ff7f00",lty = 1,bty='n')
#   
#   dev.off()
#   
#   #-----Month_temperature_past_fut Ploting---------------------
#   
#   graphics.off()
#   
#   figure_label=paste("Month_temperature_",station_name,".png",sep='')
#   
#   CairoPNG(file =figure_label,width =548, height =428,bg = "transparent")
#   par(family = "B")
#   plot(rownames(monmean),monmean$TEMP, type='o',
#        main='Monthly temperature',xlab='Month',ylab='Temperature [?C]',
#        las=2,lwd=3,pch=2,lty=1,col="#377eb8",cex=1,axes=FALSE,ylim=c(10,45))
#   lines(rownames(monmean),monmean$MAX,type='o',
#         las=1,lwd=3,pch=7,lty=1,cex=1,col="#ff7f00")
#   
#   lines(rownames(monmean),monmean$MIN,type='o',
#         las=1,lwd=3,pch=7,lty=1,cex=1,col="#984ea3")
#   
#   axis(1,at=c(1:12),labels = month.abb,las=3.5);axis(2,cex.axis=0.8);box()
#   #grid(5, 5, lwd =0.1,col = "black")
#   legend("bottom", legend=c("Mean", "MAX", "MIN"),
#          col=c('#377eb8',"#ff7f00","#984ea3"),pch=c(2,7),lty=1,cex=0.7,bty='n')
#   dev.off()
#   #-------------------------------------------------------#
#   
#   graphics.off()
#   ## Temperature and humidity 
#   
#   figure_label=paste("Temperature_Humidity_",station_name,".png",sep='')
#   
#   CairoPNG(file = figure_label,width =613, height =490,bg = "transparent")
#   par(mar = c(5,4,4, 4) + 0.3)
#   
#   plot(monmean$MONTH,monmean$TEMP,type='o' ,
#        pch = 16, col = 2,xlab='Year', ylab='',axes=FALSE,las=2,lwd=3)
#   box()
#   axis(side = 2, at = pretty(range(monmean$TEMP)),col = 2,las=1,lwd=2,cex.axis=0.8)
#   
#   par(new = TRUE)                            
#   plot(monmean$MONTH,monmean$RH,type='o', pch = 17, col = 3,              # Create second plot without axes
#        axes = FALSE, xlab = "", ylab = "",las=2,lwd=3)
#   axis(side = 4, at = pretty(range(monmean$RH)),col = 3,las=2)
#   axis(1,at=c(1:12),labels = month.abb,las=3.5)
#   mtext("Rh", side = 4, line = 3)
#   mtext("[?C]", side = 2, line = 3) 
#   mtext("Monthly variation : Temperature vs Humidity", side = 3, line = 3) 
#   
#   legend("bottom", legend=c("Temperature","Relative Humidity"),
#          col=c(2,3),
#          pch=c(16,17),lty=1,lwd=2,cex=0.8,bty='n'
#   )
#   
#   dev.off()
#   #------------------------------------------------------------#
#   
#   graphics.off()
#   
#   ## Temperature and humidity 
#   
#   figure_label=paste("Temperature_RAINFALL_",station_name,".png",sep='')
#   CairoPNG(file = figure_label,width =613, height =490,bg = "transparent")
#   par(mar = c(5,4,4, 4) + 0.3)
#   
#   plot(monmean$MONTH,monmean$TEMP,type='o' ,
#        pch = 16, col = 2,xlab='Year', ylab='',axes=FALSE,las=2,lwd=3)
#   box()
#   axis(side = 2, at = pretty(range(monmean$TEMP)),col = 2,las=1,lwd=2,cex.axis=0.8)
#   
#   par(new = TRUE)                            
#   plot(monmean$MONTH,mon_prcp$PRCP,type='o', pch = 17, col = 3,              # Create second plot without axes
#        axes = FALSE, xlab = "", ylab = "",las=2,lwd=3)
#   axis(side = 4, at = pretty(range(mon_prcp$PRCP)),col = 3,las=2)
#   axis(1,at=c(1:12),labels = month.abb,las=3.5)
#   mtext("[mm]", side = 4, line = 3)
#   mtext("[?C]", side = 2, line = 3) 
#   mtext("Monthly variation : Temperature vs Rainfall", side = 3, line = 3) 
#   
#   legend("bottom", legend=c("Temperature","Rainfall"),
#          col=c(2,3),
#          pch=c(16,17),lty=1,lwd=2,cex=0.8,bty='n'
#   )
#   
#   dev.off()
#   #------------------------------------------------------------#
#   
#   
#   graphics.off()
#   
#   
#   #----Annual Rainfall: PDSI -----------
#   
#   figure_label=paste("Annual_rainfall_hist_",station_name,".png",sep='')
#   CairoPNG(file =figure_label,width =613, height =490,bg = "transparent")
#   par(family = "B", mar = c(5, 4, 4, 4) + 0.2)
#   plot(dt_anom$YEAR,dt_anom$PRCP, type='h',
#        main='Rainfall Anomaly - Observation',xlab='Year',ylab='Index',
#        las=2,lwd=5,lty=1,cex=1,axes=FALSE,ylim=c(-5,5),lend="butt",
#        col=ifelse(dt_anom$PRCP>0,'#984ea3','#E37831'))
#   
#   #lines(obs_rainfall_annual$Year, smooth(obs_rainfall_annual$PSDI),lwd=2)
#   #lend="butt" to make sure that the bin are not rounded
#   axis(1,at=seq(min(dt_anom$YEAR),max(dt_anom$YEAR),5),las=3.5,cex.axis=0.8,lwd=2);
#   axis(2,las=1,cex.axis=0.8)
#   # segments(1986,0,2020,0)
#   box(lwd=2);
#   legend("bottomright", legend=c("Dry Year", "Wet Year"),
#          col=c('#E37831',"#984ea3"),pch=c(1,1),lty=1,lwd=2,cex=1,bty='n')
#   
#   # grid(8,5, lwd =0.05,col = "black")
#   #grid(5, 5, lwd = par("lwd"),col = "black")
#   dev.off()
#   #---------------------------------------------------------------------#
#   
#   graphics.off()
#   
#   figure_label=paste("boxplot_Month_rainfall_obs_",station_name,".png",sep='')
#   CairoPNG(file =figure_label,width =613, height =500,bg = "transparent")
#   boxplot(Yearmon$PRCP~Yearmon$MONTH ,
#           ylab="mm" , xlab="Months", main="Monthly Rainfall Trend",
#           axes=FALSE)
#   axis(1,at=c(1:12),labels = month.abb,las=3.5);axis(2,las=2);box()
#   lines(1:12,mon_prcp$PRCP,col="#ff7f00",lwd=3)
#   legend(x="topleft",legend=c("Monthly Mean"),
#          cex = 0.6,lwd = 3, col = "#ff7f00",lty = 1,bty='n')
#   
#   dev.off()
#   
#   graphics.off()
#   
#   #----- Humidity---------------------
#   # par(family = "B",mai=c(4,0.7,0.2,0.2))
#   figure_label=paste("humididty_yearly_",station_name,".png",sep='')
#   CairoPNG(file =figure_label,width =613, height =490,bg = "transparent")
#   plot(Yearly$YEAR,Yearly$RH, type='o',
#        main='Relative humidity',xlab='Year',ylab='RH',
#        las=2,lwd=3,pch=2,lty=1,col="#984ea3",cex=1,axes=FALSE,ylim=c(min(Yearly$RH)-10,max(Yearly$RH)+10))
#   axis(1,at=seq(min(Yearly$YEAR),max(Yearly$YEAR),5),las=3.5)
#   axis(2,las=0.8);box(lwd=2)
#   
#   #grid(5, 5, lwd =0.1,col = "black")
#   legend("bottomright", legend=c("RH"),
#          col=c('#984ea3',"#ff7f00","#274983"),pch=c(2,7),lty=1,cex=1,bty='n')
#   
#   dev.off()
#   #-------------------------------------------------------#
#   graphics.off()
#   
#   
# }



# # Retrieve, day, month and year from ts --- Senegal
# 
# for (i in 1:length(unique(dat_Senegal$NAME))){
#   
#   MAG=dat_Senegal[which(dat_Senegal$NAME==unique(dat_Senegal$NAME)[i]),]
#   station_name=unlist(strsplit(unique(MAG$NAME), "[/]"))[1]
#   
#   
#   MAG$PRCP=na_interpolation(MAG$PRCP)
#   MAG$MIN=na_interpolation(MAG$MIN)
#   MAG$MAX=na_interpolation(MAG$MAX)
#   MAG$WDSP=na_interpolation(MAG$WDSP)
#   MAG$RH=na_interpolation(MAG$RH)
#   
#   
#   
#   MAG$Date=ymd(paste(MAG$YEAR,MAG$MONTH,MAG$DAY,sep='-'))
#   
#   
#   Yearly=MAG %>% group_by(YEAR) %>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
#                                               MIN=mean(MIN,na.rm=TRUE),
#                                               MAX=mean(MAX,na.rm=TRUE), 
#                                               PRCP=sum(PRCP,na.rm=TRUE),
#                                               RH =mean(RH,na.rm=TRUE),
#                                               WDSP=mean(WDSP,na.rm=TRUE))
#   
#   ## YearMon
#   Yearmon=MAG %>% group_by(YEAR,MONTH) %>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
#                                                      MIN=mean(MIN,na.rm=TRUE),
#                                                      MAX=mean(MAX,na.rm=TRUE), 
#                                                      PRCP=sum(PRCP,na.rm=TRUE),
#                                                      RH =mean(RH,na.rm=TRUE),
#                                                      WDSP=mean(WDSP,na.rm=TRUE))
#   
#   mon_prcp=Yearmon%>% group_by(MONTH) %>% summarise(PRCP=mean(PRCP,na.rm=TRUE))
#   
#   ## Mon
#   
#   monmean=MAG %>% group_by(MONTH) %>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
#                                                 MIN=mean(MIN,na.rm=TRUE),
#                                                 MAX=mean(MAX,na.rm=TRUE), 
#                                                 PRCP=sum(PRCP,na.rm=TRUE),
#                                                 RH =mean(RH,na.rm=TRUE),
#                                                 WDSP=mean(WDSP,na.rm=TRUE))
#   
#   
#   ## Compute anomaly 
#   
#   # Compute temperature standardised anomaly index
#   (index_base=subset(Yearly,YEAR >=1976 & YEAR <=2005))
#   
#   mean_dt= index_base%>% summarise(TEMP=mean(TEMP,na.rm=TRUE),
#                                    MIN=mean(MIN,na.rm=TRUE),
#                                    MAX=mean(MAX,na.rm=TRUE), 
#                                    PRCP=mean(PRCP,na.rm=TRUE),
#                                    RH =mean(RH,na.rm=TRUE),
#                                    WDSP=mean(WDSP,na.rm=TRUE))
#   
#   sd_dt= index_base%>% summarise(TEMP=sd(TEMP,na.rm=TRUE),
#                                  MIN=sd(MIN,na.rm=TRUE),
#                                  MAX=sd(MAX,na.rm=TRUE), 
#                                  PRCP=sd(PRCP,na.rm=TRUE),
#                                  RH =sd(RH,na.rm=TRUE),
#                                  WDSP=sd(WDSP,na.rm=TRUE))
#   
#   
#   dt_anom= Yearly %>% group_by(YEAR)%>% summarise(TEMP=((TEMP-mean_dt$TEMP)/(sd_dt$TEMP)),
#                                                   MIN=((MIN-mean_dt$MIN)/(sd_dt$MIN)),
#                                                   MAX=((MAX-mean_dt$MAX)/(sd_dt$MAX)), 
#                                                   PRCP=((PRCP-mean_dt$PRCP)/(sd_dt$PRCP)),
#                                                   RH =((RH-mean_dt$RH)/(sd_dt$RH)),
#                                                   WDSP=((WDSP-mean_dt$WDSP)/(sd_dt$WDSP)))
#   
#   
#   ## plotting
#   
#   ### Yearly variation of differents variables
#   
#   
#   #-----Plot Font------------
#   windowsFonts(
#     A=windowsFont("Arial Black"),
#     B=windowsFont("Bookman Old Style"),
#     C=windowsFont("Comic Sans MS"),
#     D=windowsFont("Symbol")
#   )
#   #------------------------#
#   
#   #----- Temperature---------------------
#   par(family = "B")
#   
#   figure_label=paste("tmperature_yearly_",station_name,".png",sep='')
#   CairoPNG(file =figure_label,width =613, height =490,bg = "transparent")
#   plot(Yearly$YEAR,Yearly$MAX, type='o',
#        main='Trend of temperature - Niamey',xlab='Year',ylab='Temp [Deg]',
#        las=2,lwd=3,pch=2,lty=1,col="#984ea3",cex=1,axes=FALSE, ylim=c(15,40))
#   lines(Yearly$YEAR,Yearly$MIN,type='o',las=1,lwd=3,pch=7,lty=1,cex=1,col="#ff7f00")
#   lines(Yearly$YEAR,Yearly$TEMP,type='o',las=1,lwd=3,pch=7,lty=1,cex=1,col="#274983")
#   axis(1,at=seq(min(Yearly$YEAR),max(Yearly$YEAR),5),las=3.5)
#   axis(2,las=0.8);box(lwd=2)
#   
#   #grid(5, 5, lwd =0.1,col = "black")
#   legend("bottomright", legend=c("Tmax","Tmin","Tmean"),
#          col=c('#984ea3',"#ff7f00","#274983"),pch=c(2,7),lty=1,cex=1,bty='n')
#   
#   dev.off()
#   #-------------------------------------------------------#
#   graphics.off()
#   
#   # temperature mean trend
#   
#   par(family = "B")
#   
#   figure_label=paste("Mean_temperature_yearly_",station_name,".png",sep='')
#   CairoPNG(file =figure_label,width =613, height =490,bg = "transparent")
#   
#   plot(Yearly$YEAR,Yearly$TEMP, type='o',
#        main='Trend of Mean temperature - Niamey',xlab='Year',ylab='Temp [Deg]',
#        las=2,lwd=3,lty=1,col='#E37831',cex=1)
#   box(lwd=2)
#   
#   grid(5, 5, lwd =0.1,col = "black")
#   legend("bottomright", legend=c("Tmean"),col=c("#ff7f00"),pch=c(1),lty=1,cex=1,bty='n')
#   lmreg=lm(Yearly$TEMP ~ as.numeric(Yearly$YEAR))
#   abline(lmreg, col='red',lwd=2)
#   mtext(paste('y = ',round(lmreg$coefficients[2],2),'x +',round(lmreg$coefficients[1]),
#               ' ; R = ',round(summary(lmreg)$r.squared,2),sep=""),cex = 0.8)
#   
#   dev.off()
#   graphics.off()
#   
#   figure_label=paste("boxplot_Month_temperature_obs_",station_name,".png",sep='')
#   
#   CairoPNG(file =figure_label,width =613, height =500,bg = "transparent")
#   boxplot(Yearmon$TEMP~Yearmon$MONTH ,
#           ylab="[?C]" , xlab="Months", main="Monthly Temperature Trend",
#           axes=FALSE)
#   axis(1,at=c(1:12),labels = month.abb,las=3.5);axis(2,las=2);box()
#   lines(1:12,monmean$TEMP,col="#ff7f00",lwd=3)
#   legend_name=paste("Monthly Mean", min(Yearmon$YEAR),"-",max(Yearmon$YEAR))
#   legend(x="bottomright",legend=legend_name,
#          cex = 0.6,lwd = 3, col = "#ff7f00",lty = 1,bty='n')
#   
#   dev.off()
#   
#   #-----Month_temperature_past_fut Ploting---------------------
#   
#   graphics.off()
#   
#   figure_label=paste("Month_temperature_",station_name,".png",sep='')
#   
#   CairoPNG(file =figure_label,width =548, height =428,bg = "transparent")
#   par(family = "B")
#   plot(rownames(monmean),monmean$TEMP, type='o',
#        main='Monthly temperature',xlab='Month',ylab='Temperature [?C]',
#        las=2,lwd=3,pch=2,lty=1,col="#377eb8",cex=1,axes=FALSE,ylim=c(10,45))
#   lines(rownames(monmean),monmean$MAX,type='o',
#         las=1,lwd=3,pch=7,lty=1,cex=1,col="#ff7f00")
#   
#   lines(rownames(monmean),monmean$MIN,type='o',
#         las=1,lwd=3,pch=7,lty=1,cex=1,col="#984ea3")
#   
#   axis(1,at=c(1:12),labels = month.abb,las=3.5);axis(2,cex.axis=0.8);box()
#   #grid(5, 5, lwd =0.1,col = "black")
#   legend("bottom", legend=c("Mean", "MAX", "MIN"),
#          col=c('#377eb8',"#ff7f00","#984ea3"),pch=c(2,7),lty=1,cex=0.7,bty='n')
#   dev.off()
#   #-------------------------------------------------------#
#   
#   graphics.off()
#   ## Temperature and humidity 
#   
#   figure_label=paste("Temperature_Humidity_",station_name,".png",sep='')
#   
#   CairoPNG(file = figure_label,width =613, height =490,bg = "transparent")
#   par(mar = c(5,4,4, 4) + 0.3)
#   
#   plot(monmean$MONTH,monmean$TEMP,type='o' ,
#        pch = 16, col = 2,xlab='Year', ylab='',axes=FALSE,las=2,lwd=3)
#   box()
#   axis(side = 2, at = pretty(range(monmean$TEMP)),col = 2,las=1,lwd=2,cex.axis=0.8)
#   
#   par(new = TRUE)                            
#   plot(monmean$MONTH,monmean$RH,type='o', pch = 17, col = 3,              # Create second plot without axes
#        axes = FALSE, xlab = "", ylab = "",las=2,lwd=3)
#   axis(side = 4, at = pretty(range(monmean$RH)),col = 3,las=2)
#   axis(1,at=c(1:12),labels = month.abb,las=3.5)
#   mtext("Rh", side = 4, line = 3)
#   mtext("[?C]", side = 2, line = 3) 
#   mtext("Monthly variation : Temperature vs Humidity", side = 3, line = 3) 
#   
#   legend("bottom", legend=c("Temperature","Relative Humidity"),
#          col=c(2,3),
#          pch=c(16,17),lty=1,lwd=2,cex=0.8,bty='n'
#   )
#   
#   dev.off()
#   #------------------------------------------------------------#
#   
#   graphics.off()
#   
#   ## Temperature and humidity 
#   
#   figure_label=paste("Temperature_RAINFALL_",station_name,".png",sep='')
#   CairoPNG(file = figure_label,width =613, height =490,bg = "transparent")
#   par(mar = c(5,4,4, 4) + 0.3)
#   
#   plot(monmean$MONTH,monmean$TEMP,type='o' ,
#        pch = 16, col = 2,xlab='Year', ylab='',axes=FALSE,las=2,lwd=3)
#   box()
#   axis(side = 2, at = pretty(range(monmean$TEMP)),col = 2,las=1,lwd=2,cex.axis=0.8)
#   
#   par(new = TRUE)                            
#   plot(monmean$MONTH,mon_prcp$PRCP,type='o', pch = 17, col = 3,              # Create second plot without axes
#        axes = FALSE, xlab = "", ylab = "",las=2,lwd=3)
#   axis(side = 4, at = pretty(range(mon_prcp$PRCP)),col = 3,las=2)
#   axis(1,at=c(1:12),labels = month.abb,las=3.5)
#   mtext("[mm]", side = 4, line = 3)
#   mtext("[?C]", side = 2, line = 3) 
#   mtext("Monthly variation : Temperature vs Rainfall", side = 3, line = 3) 
#   
#   legend("bottom", legend=c("Temperature","Rainfall"),
#          col=c(2,3),
#          pch=c(16,17),lty=1,lwd=2,cex=0.8,bty='n'
#   )
#   
#   dev.off()
#   #------------------------------------------------------------#
#   
#   
#   graphics.off()
#   
#   
#   #----Annual Rainfall: PDSI -----------
#   
#   figure_label=paste("Annual_rainfall_hist_",station_name,".png",sep='')
#   CairoPNG(file =figure_label,width =613, height =490,bg = "transparent")
#   par(family = "B", mar = c(5, 4, 4, 4) + 0.2)
#   plot(dt_anom$YEAR,dt_anom$PRCP, type='h',
#        main='Rainfall Anomaly - Observation',xlab='Year',ylab='Index',
#        las=2,lwd=5,lty=1,cex=1,axes=FALSE,ylim=c(-5,5),lend="butt",
#        col=ifelse(dt_anom$PRCP>0,'#984ea3','#E37831'))
#   
#   #lines(obs_rainfall_annual$Year, smooth(obs_rainfall_annual$PSDI),lwd=2)
#   #lend="butt" to make sure that the bin are not rounded
#   axis(1,at=seq(min(dt_anom$YEAR),max(dt_anom$YEAR),5),las=3.5,cex.axis=0.8,lwd=2);
#   axis(2,las=1,cex.axis=0.8)
#   # segments(1986,0,2020,0)
#   box(lwd=2);
#   legend("bottomright", legend=c("Dry Year", "Wet Year"),
#          col=c('#E37831',"#984ea3"),pch=c(1,1),lty=1,lwd=2,cex=1,bty='n')
#   
#   # grid(8,5, lwd =0.05,col = "black")
#   #grid(5, 5, lwd = par("lwd"),col = "black")
#   dev.off()
#   #---------------------------------------------------------------------#
#   
#   graphics.off()
#   
#   figure_label=paste("boxplot_Month_rainfall_obs_",station_name,".png",sep='')
#   CairoPNG(file =figure_label,width =613, height =500,bg = "transparent")
#   boxplot(Yearmon$PRCP~Yearmon$MONTH ,
#           ylab="mm" , xlab="Months", main="Monthly Rainfall Trend",
#           axes=FALSE)
#   axis(1,at=c(1:12),labels = month.abb,las=3.5);axis(2,las=2);box()
#   lines(1:12,mon_prcp$PRCP,col="#ff7f00",lwd=3)
#   legend(x="topleft",legend=c("Monthly Mean"),
#          cex = 0.6,lwd = 3, col = "#ff7f00",lty = 1,bty='n')
#   
#   dev.off()
#   
#   graphics.off()
#   
#   #----- Humidity---------------------
#   # par(family = "B",mai=c(4,0.7,0.2,0.2))
#   figure_label=paste("humididty_yearly_",station_name,".png",sep='')
#   CairoPNG(file =figure_label,width =613, height =490, bg = "transparent")
#   plot(Yearly$YEAR,Yearly$RH, type='o',
#        main='Relative humidity',xlab='Year',ylab='RH',
#        las=2,lwd=3,pch=2,lty=1,col="#984ea3",cex=1,axes=FALSE,ylim=c(min(Yearly$RH)-10,max(Yearly$RH)+10))
#   axis(1,at=seq(min(Yearly$YEAR),max(Yearly$YEAR),5),las=3.5)
#   axis(2,las=0.8);box(lwd=2)
#   
#   #grid(5, 5, lwd =0.1,col = "black")
#   legend("bottomright", legend=c("RH"),
#          col=c('#984ea3',"#ff7f00","#274983"),pch=c(2,7),lty=1,cex=1,bty='n')
#   
#   dev.off()
#   #-------------------------------------------------------#
#   graphics.off()
#   
#   
# }

