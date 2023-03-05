install.packages ("ncdf4")
library(ncdf4)
install.packages("chron")
library(chron)

install.packages("lattice")
library(lattice)

install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("zoo")
library(zoo)
install.packages("animation")
library(animation)


library(ncdf4)
library(chron)
library(lattice)
library(RColorBrewer)
library(zoo)
library(animation)



ncin<-nc_open("cru_ts4.06.1901.2021.tmp.dat.nc")

lon <- ncvar_get(ncin,"lon")
 nlon <- dim(lon)
 lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
 time <- ncvar_get(ncin,"time")
 nt <-dim(time)
 
 
 tunits <- ncatt_get(ncin,"time","units")
  tustr <- strsplit(tunits$value, " ")
  tdstr <- strsplit(unlist(tustr)[3], "-")
  tmonth <- as.integer(unlist(tdstr)[2])
  tday <- as.integer(unlist(tdstr)[3])
  tyear <- as.integer(unlist(tdstr)[1])
  time_ch<-chron(time,origin=c(tmonth, tday, tyear))
  time_m_y<-as.yearmon(time_ch)
  Sys.setlocale("LC_TIME", "C")
  time_m_y<-as.yearmon(time_ch)
  
  m <- 6
   tmp_slice <- ncvar_get(ncin,"tmp",start=c(1,1,m),count=c(nlon,nlat,1))
  
   range(na.omit(as.numeric(tmp_slice)))
   
   
   image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")), xlim=c(-180,180),ylim=c(-90,90),zlim=c(-17,39))
   
   
   
    m<-12
    tmp_slice <- ncvar_get(ncin,"tmp",start=c(1,1,m),count=c(nlon,nlat,1))
    range(na.omit(as.numeric(tmp_slice)))
   
    image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")), xlim=c(-180,180),ylim=c(-90,90),zlim=c(-19,39))
    text(-150,-70,time_m_y[m])
   
   
    image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")), xlim=c(-180,180),ylim=c(-90,90),zlim=c(-45,34))
   
   
    grid <- expand.grid(lon=lon, lat=lat)
   
    
    cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
   
    levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, col.regions=(rev(brewer.pal(10,"RdBu"))),aspect=0.5)
   
   
    
     site_tmp<-ncvar_get(ncin,"tmp",start=c(401,281,1),count=c(1,1,1452))
     plot(time_m_y, site_tmp,t="l")
    
     slice_m<-seq(6,length(time_m_y),12) #wybieram pozycje czerwców
      plot(time_m_y[slice_m],site_tmp[slice_m],t="b")
    
      radom_tmp<-ncvar_get(ncin,"tmp",start=c(403,283,1),count=c(1,1,1452))
    
   
       slice_m<-seq(6,length(time_m_y),12)
       plot(time_m_y[slice_m],radom_tmp[slice_m],t="b")
       abline(a=mean(radom_tmp[slice_m]),b=0,col="Red",lwd=2)
   
   
       g1<-slice_m[61:90]
        g2<-slice_m[91:120]
   
   
        plot(time_m_y[g1],radom_tmp[g1],t="b",ylim=c(15,22),axes=FALSE)
        
        lines(time_m_y[g1],radom_tmp[g2],t="b",col="Red")
        
        df1<-data.frame(x=time_m_y[g1],y= radom_tmp[g1])
        df1
        
        df2<-data.frame(x=time_m_y[g2],y= radom_tmp[g2])
    
        m1<-lm(y~x,data=df1)
         m2<-lm(y~x,data=df2)
         
         
         lines(time_m_y[g1],predict(m1))
          lines(time_m_y[g2],predict(m2),col="Red")
         
          
          which(lon==49.75)
          
           which(lat==21.25) 
          
          jas³o<-ncvar_get(ncin,"tmp",start=c(460,223,1),count=c(1,1,1452))
          
          
          jas³o
         
          
           slice_m<-seq(1,length(time_m_y),12)
           plot(time_m_y[slice_m],jas³o[slice_m],t="b")
           abline(a=mean(jas³o[slice_m]),b=0,col="Red",lwd=2)
          
          
           
           
           
           
           plot(time_m_y[g1], jas³o[g1],t="b",xlim=c(1961,2020))
            lines(time_m_y[g2], jas³o[g2],t="b",col="Red")
           
          
            df1<-data.frame(x=time_m_y[g1],y= jas³o[g1])
            df1
          
            
            df2<-data.frame(x=time_m_y[g2],y= jas³o[g2])
          
            
            m1<-lm(y~x,data=df1)
             m2<-lm(y~x,data=df2)
          
          
             
             lines(time_m_y[g1],predict(m1))
              lines(time_m_y[g2],predict(m2),col="Red")
              
              
              summary(m1)
              
              
              lines(time_m_y[g2],predict(m1,newdata=df2),col="Blue")
              
              summary(m2)
              
              
              
              
              
              
              which(lon==21.75)
              which(lat==50.25)
              
           Rzeszow<-ncvar_get(ncin,"tmp",start=c(404,281,1),count=c(1,1,1452))
              Rzeszow
              
              slice_m<-seq(12,length(time_m_y),12)
              plot(time_m_y[slice_m], Rzeszow[slice_m],t="b")
              abline(a=mean( Rzeszow[slice_m]),b=0,col="Red",lwd=2)
              
              
              
             g1<-slice_m[61:90]
             g2<-slice_m[91:120]
              
             plot(time_m_y[g1], Rzeszow[g1],t="b",xlim=c(1961,2020))
             lines(time_m_y[g2], Rzeszow[g2],t="b",col="Red")
              
             df1<-data.frame(x=time_m_y[g1],y= Rzeszow[g1])
             df1
             
             df2<-data.frame(x=time_m_y[g2],y= Rzeszow[g2])
              
             m1<-lm(y~x,data=df1)
             m2<-lm(y~x,data=df2)
             
             
             lines(time_m_y[g1],predict(m1))
             lines(time_m_y[g2],predict(m2),col="Red")
              
              
             lines(time_m_y[g2],predict(m1,newdata=df2),col="Blue")
              title("GRUDZIEÑ")
              
              