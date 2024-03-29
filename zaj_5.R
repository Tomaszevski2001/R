install.packages("dplR")
library(dplR)
###Chronologia mong009 (1326-1998) - chronologia modrzewiowa (modrzewia sybreyjskiego) wielowiekowa kompozytowa, czyli powsta�a na bazie pr�b drewna z drzew �yj�cych oraz martwych przez cross-dating.
mong009_chron<-read.crn("mong009wr.crn")
head(mong009_chron)
plot(mong009_chron)



install.packages ("ncdf4")
library(ncdf4)
ncin<-nc_open("cru_ts4.06.1901.2021.tmp.dat.nc")
lon <- ncvar_get(ncin,"lon")
lat <- ncvar_get(ncin,"lat")
#mong009 location: lon: 91.57, lat: 49.92
lon_i<-which(lon==91.75)
lat_i<-which(lat==49.75)
lon_i
lat_i
tmp_mong009<- ncvar_get(ncin,"tmp",start=c(lon_i,lat_i,1),count=c(1,1,1452))
plot(tmp_mong009,t="l")
#potrzebne dane meteo - ?rednia temperatura czerwca
tmp6_mong009<-tmp_mong009[seq(6, 1452,12)]
years<-seq(1901,2021,1)
plot(years, scale (tmp6_mong009), t="b")



####korelacja dendro i meteo####
#nowa ramka danych dla okresu wsp�lnego danych, czyli 1901-1998.
years_chron<-as.numeric(row.names(mong009_chron))
start_chron<-which(years_chron==1901)
end_chron<-length(mong009_chron[,1])
comm_chron<-mong009_chron[start_chron:end_chron,1]
comm_chron
#
comm_tmp6<-tmp6_mong009[1:(max(years_chron)-1900)]
comm_tmp6
#
data<-data.frame(comm_chron,comm_tmp6)
data
#
cor.test(data$comm_chron, data$comm_tmp6)
##korelacja jest istotna statystycznie, wykonajmy wi�c model dla ca�o�ci danych
mod_f<-lm(comm_tmp6~comm_chron,data)
summary(mod_f)
plot(data)
abline(mod_f,data)



##kalibracja -> okresa m�odszy (1950-1998)
data2<-data[50:98,] #okres danych to 98 lat
mod_1<-lm(comm_tmp6~comm_chron,data2)
summary(mod_1)
#zobaczmy jeszcze na warto�� korelacji
cor.test(data2$comm_tmp6, data2$comm_chron)



##weryfikacja -> okresa starszy (1901-1949)
data3<-data[1:49,]
data3
#sprawdzamy jak model utworzony na danych z okresu m�odszego (mod_1) sprawdza si� w odniesieniu do danych z okresu starszego (data3)
#czyli korelujemy rzeczywiste temperatury czerwca w okresie 1901-1949 z wymodelowanymi z zastosowaniem wcze�niej utworzonego modelu 
cor.test(data3$comm_tmp6, predict(mod_1,data3)) #korelacja jest istotna, czyli mo�na stosowa� model



#narysujmy dane rzeczywiste i wymodelowane
#dla m�odszego okresu
plot(years[50:98],data2$comm_tmp6,t="l")
lines(years[50:98], predict(mod_1,data2),t="l",col="Red")
#dla starszego okresu
plot(years[1:49], data3$comm_tmp6,t="l")
lines(years[1:49], predict(mod_1,data3),t="l",col="Red")
#po��czmy wykresy
plot(years[1:98], data$comm_tmp6,t="l")
lines(years[1:98],predict(mod_1,data),t="l",col="Red")
#skoro weryfikacja przebieg�a pomy�lnie, mo�emy zastosowa� model utworzony dla ca�ego okresu (mod_f)
summary(mod_f)
#narysujmy go z danymi rzeczywistymi utworzony na danych z ca�ego okresu (mod_f)
plot(years[1:98], data$comm_tmp6,t="l")
lines(years[1:98],predict(mod_f,data),t="l",col="Blue")
##wykorzystajmy ten model do rekonstrukcji tmp. z okresu, gdzie brak danych bezpo�rednich (meteo)
data4<-data.frame (comm_chron=mong009_chron[,1])
data4
mong009_tmp6_reconst<-predict(mod_f,data4)
mong009_tmp6_reconst
plot(years_chron, mong009_tmp6_reconst, t="l")