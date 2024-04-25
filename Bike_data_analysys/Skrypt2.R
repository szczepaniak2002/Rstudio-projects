
options(stringsAsFactors=FALSE)
library(sqldf)
library(geosphere)
library(lubridate)

library(tidyverse)
library(sf)
library(mapview)

NY_1 <- read.csv("dane/201801-citibike-tripdata.csv")
NY_4 <- read.csv("dane/201804-citibike-tripdata.csv")
NY_7 <- read.csv("dane/201807-citibike-tripdata.csv")
NY_10 <- read.csv("dane/201809-citibike-tripdata.csv")

JC_1 <- read.csv("dane/JC-201801-citibike-tripdata.csv")
JC_4 <- read.csv("dane/JC-201804-citibike-tripdata.csv")
JC_7 <- read.csv("dane/JC-201807-citibike-tripdata.csv")
JC_10 <- read.csv("dane/JC-201810-citibike-tripdata.csv")


Dane <- rbind(NY_1,NY_4)
Dane <- rbind(Dane,NY_7)
Dane <- rbind(Dane,NY_10)

DaneJc <- rbind(JC_1,JC_4)
DaneJc <- rbind(DaneJc,JC_7)
DaneJc <- rbind(DaneJc,JC_10)




# dodanie dni tygodnia do tabeli
dodawanieDni <- function(dataframe) {
  ramka <- dataframe
  ramka$day <- wday(ramka$starttime,label=TRUE) 
  return(ramka)
  }# 1 to niedziela, wi?c pi?tek to 6


# zamienia sekundy na godziny
czas_przejazdu <- function(df) {
  res <- df
  
  res$tripduration <- round((res$tripduration/3600 ), 10)
  
  return(res)
}

#zmienia rok urodzenia na ilo?? lat
formatowanie_wieku <- function(df) {
  res <- df
  res[,14] <- sapply(res[,14],as.numeric)
  
  res$age <- (res$birth.year - 2018) * -1
  
  return(res) }

#pomocnicza funkcja do dlogosc_przejazdu()
dist_geo <- function(lat_a, lon_a, lat_b, lon_b){
  
  round(distm(c(lon_a, lat_a), c(lon_b, lat_b), fun = distHaversine)/1000,10)
  
} 

#wylicza d?ugo?? przejazdu w lini prostej
dlugosc_przejazdu <- function(df) {
  res <- df
  
  res$tripdistance <- mapply(lat_a=res$start.station.latitude, lon_a=res$start.station.longitude, lat_b=res$end.station.latitude, lon_b=res$end.station.longitude,FUN = dist_geo)
  
  return(res)
  
}

# wybranie konkretnych godzin

godzina <- function(df) {
  res <- df
  date <- as.POSIXct(res$starttime, format ="%Y-%m-%d %H:%M:%S" )
  res$starttime <- format(date, format="%H")
  date <- as.POSIXct(res$stoptime, format ="%Y-%m-%d %H:%M:%S" )
  res$stoptime <- format(date, format="%H")
  return(res)

}




# wybranie tylko godzin mi?dzy 23 a 5 i potrzebnych danych 
wybieranie_danych <- function(df) {
  test <- df
  formatowanie_wieku(test)-> test
  dodawanieDni(test) -> test
  godzina(test) -> test
  czas_przejazdu(test)->test
  dlugosc_przejazdu(test) -> test
 
  
  
  test <- test[,c("tripduration","tripdistance","starttime","stoptime","start.station.name",
               "start.station.latitude","start.station.longitude","end.station.name",
               "end.station.latitude","end.station.longitude","birth.year","gender","day")]
  return(test)
}

test<-DaneJc
test2 <- Dane

wybieranie_danych(test) -> test
wybieranie_danych(test2)-> test2

# ?rednie wyniki dla predkosci najpierw w nocy potem w dzien
porownywanie_predkosci <- function(df) {
  doba <- df[!(df$starttime %in% c("00","01","02","03","04")) & 
               !(df$stoptime %in% c("00","01","02","03","04")),c("tripduration","tripdistance")]
  noc <- df[df$starttime %in% c("00","01","02","03","04") & 
              df$stoptime %in% c("00","01","02","03","04"),
              c("tripduration","tripdistance")]
  
  doba$speed <- (doba$tripdistance/doba$tripduration)
  noc$speed <- noc$tripdistance /noc$tripduration
  
  mean(doba$speed) -> predkosc_srednia
  mean(noc$speed) -> predkosc_srednia_noc
  
  return(c(predkosc_srednia_noc,predkosc_srednia))
  
  
  
}

porownywanie_predkosci(test2) -> predkosci
predkosciJc <- porownywanie_predkosci(test)
predkosci
predkosciJc





# 5% najszybszych pr?dko??i
najszybsze_predkosci <- function(df) {
  doba <- df[!(df$starttime %in% c("00","01","02","03","04")) & 
               !(df$stoptime %in% c("00","01","02","03","04")),c("tripduration","tripdistance")]
  noc <- df[df$starttime %in% c("00","01","02","03","04") & 
              df$stoptime %in% c("00","01","02","03","04"),
            c("tripduration","tripdistance")]
  
  doba$speed <- (doba$tripdistance/doba$tripduration)
  noc$speed <- noc$tripdistance /noc$tripduration
  
  doba <- doba[order(doba$speed, decreasing = TRUE),]
  noc <- noc[order(noc$speed, decreasing = TRUE),]
  
  x <- round(nrow(doba)/10)
  doba <- doba[1:x,]
  y <- round(nrow(noc)/10)
  noc <- noc[1:y,]
  
  mean(doba$speed) -> predkosc_srednia
  mean(noc$speed) -> predkosc_srednia_noc
  
  return(c(predkosc_srednia_noc,predkosc_srednia))
}

top_predkosci <- najszybsze_predkosci(test2)
top_predkosciJc <- najszybsze_predkosci(test)
top_predkosci
top_predkosciJc


#najwieksza predkosc w dzien i w nocy
max_predkosci <- function(df) {
  doba <- df[!(df$starttime %in% c("00","01","02","03","04")) & 
               !(df$stoptime %in% c("00","01","02","03","04")),c("tripduration","tripdistance")]
  noc <- df[df$starttime %in% c("00","01","02","03","04") & 
              df$stoptime %in% c("00","01","02","03","04"),
            c("tripduration","tripdistance")]
  doba$speed <- (doba$tripdistance/doba$tripduration)
  noc$speed <- noc$tripdistance /noc$tripduration
  doba <- doba[order(doba$speed, decreasing = TRUE),]
  noc <- noc[order(noc$speed, decreasing = TRUE),]
  doba <- doba[,"speed"]
  noc<- noc[,"speed"]
  doba<- head(doba,1)
  noc<- head(noc,1)
  
  return(noc)
  
  
}
max_predkosci(test2)-> maksymalne_predkosci
maksymalne_predkosci


# liczenie plci
plec <- function(df) {
  res <- df[df$starttime %in% c("00","01","02","03","04") & 
              df$stoptime %in% c("00","01","02","03","04"),
            c("tripduration","tripdistance","gender")]
  res <- aggregate(x = res$gender,by= res["gender"], FUN = length)
  colnames(res)[2] <- "Gender_Count"
  return(res)
  
  
}

ilosc_plci <- plec(test2)
ilosc_plciJc <- plec(test)
View(ilosc_plci)
View(ilosc_plciJc)


# porownanie predkosci kobiet i mezczyzn

predkosc_plci <- function(df) {
  res <- df[df$starttime %in% c("00","01","02","03","04") & 
              df$stoptime %in% c("00","01","02","03","04"),
            c("tripduration","tripdistance","gender")]
  res$speed <- res$tripdistance /res$tripduration
  res <- aggregate(x= res$speed,
                    by= res[c("gender")],
                    FUN = function(x) c(AverageGenderSpeed= mean(x)))
  return(res)
}

predkosc_plci(test2) -> tabela_predkosci_plci
predkosc_plci(test) -> tabela_predkosci_plciJc
View(tabela_predkosci_plci)
View(tabela_predkosci_plciJc)




# zliczanie wypozyczen w nocy w rozne dni tygodnia

liczenie_dni <- function(df) {
  res <- df[df$starttime %in% c("23","00","01","02","03","04") & 
              df$stoptime %in% c("23","00","01","02","03","04"),"day", 
            drop = FALSE ]
  res <- aggregate(x = res$day,by= res["day"], FUN = length)
  return(res)
  
}

dni_tygodnia <- liczenie_dni(test2)
dni_tygodniaJc <- liczenie_dni(test)
View(dni_tygodnia)
View(dni_tygodniaJc)



# jakie stacje s? najcz??ciej w pi?tek, sobot? 

ilosc_stacji <- function(df) {
  res <- df[df$starttime %in% c("00","01","02","03","04") & 
            df$stoptime %in% c("00","01","02","03","04")& ((df$day %in% "niedz\\.")
            | (df$day %in% "sob\\.")) ,c("end.station.name","stoptime","end.station.latitude","end.station.longitude"), 
            drop = FALSE ]
  rownames(res) <- NULL
  #polnoc
  polnoc <- res[res$stoptime %in% ("00"),c("end.station.name","end.station.latitude","end.station.longitude"),drop=FALSE] 
  polnoc <- aggregate(x = polnoc$end.station.name,by= polnoc[c("end.station.name","end.station.latitude","end.station.longitude")], FUN = length)
  colnames(polnoc)[1] <- "Polnoc_count"
  polnoc <- polnoc[order(polnoc$x, decreasing = TRUE),]
  polnoc <- head(polnoc,10)
  
  
  #pierwsza
  pierwsza <- res[res$stoptime %in% ("01"),c("end.station.name","end.station.latitude","end.station.longitude"),drop=FALSE] 
  pierwsza <- aggregate(x = pierwsza$end.station.name,by= pierwsza[c("end.station.name","end.station.latitude","end.station.longitude")], FUN = length)
  colnames(pierwsza)[1] <- "Pierwsza_count"
  pierwsza <- pierwsza[order(pierwsza$x, decreasing = TRUE),]
  pierwsza <- head(pierwsza,10)
  cbind(polnoc,pierwsza) -> wynik
  
  #druga
  druga <- res[res$stoptime %in% ("02"),c("end.station.name","end.station.latitude","end.station.longitude"),drop=FALSE] 
  druga <- aggregate(x = druga$end.station.name,by= druga[c("end.station.name","end.station.latitude","end.station.longitude")], FUN = length)
  colnames(druga)[1] <- "Druga_count"
  druga <- druga[order(druga$x, decreasing = TRUE),]
  druga <- head(druga,10)
  cbind(wynik,druga) -> wynik
  
  
  #trzecia
  trzecia <- res[res$stoptime %in% ("03"),c("end.station.name","end.station.latitude","end.station.longitude"),drop=FALSE] 
  trzecia <- aggregate(x = trzecia$end.station.name,by= trzecia[c("end.station.name","end.station.latitude","end.station.longitude")], FUN = length)
  colnames(trzecia)[1] <- "Trzecia_count"
  trzecia <- trzecia[order(trzecia$x, decreasing = TRUE),]
  trzecia <- head(trzecia,10)
  cbind(wynik,trzecia) -> wynik
  
  
  #czwarta
  czwarta <- res[res$stoptime %in% ("04"),c("end.station.name","end.station.latitude","end.station.longitude"),drop=FALSE] 
  czwarta <- aggregate(x = czwarta$end.station.name,by= czwarta[c("end.station.name","end.station.latitude","end.station.longitude")], FUN = length)
  colnames(czwarta)[1] <- "Czwarta_count"
  czwarta <- czwarta[order(czwarta$x, decreasing = TRUE),]
  czwarta <- head(czwarta,10)
  cbind(wynik,czwarta) -> wynik
  
  
  
  
  return(wynik)
}

ilosc_stacji(test2) -> stacje
ilosc_stacji(test) -> stacjeJc
View(stacje)
View(stacjeJc)

stacje[1:4] -> polnoc
stacje[5:8] -> pierwsza
stacje[9:12] -> druga
stacje[13:16] -> trzecia
stacje[17:20] -> czwarta
mapview(polnoc, xcol = "end.station.longitude", ycol = "end.station.latitude", crs = 4269, grid = FALSE)
mapview(pierwsza, xcol = "end.station.longitude", ycol = "end.station.latitude", crs = 4269, grid = FALSE)
mapview(druga, xcol = "end.station.longitude", ycol = "end.station.latitude", crs = 4269, grid = FALSE)
mapview(trzecia, xcol = "end.station.longitude", ycol = "end.station.latitude", crs = 4269, grid = FALSE)
mapview(czwarta, xcol = "end.station.longitude", ycol = "end.station.latitude", crs = 4269, grid = FALSE)

stacjeJc[1:4] -> polnocJc
stacjeJc[5:8] -> pierwszaJc
stacjeJc[9:12] -> drugaJc
stacjeJc[13:16] -> trzeciaJc
stacjeJc[17:20] -> czwartaJc
mapview(polnocJc, xcol = "end.station.longitude", ycol = "end.station.latitude", crs = 4269, grid = FALSE)
mapview(pierwszaJc, xcol = "end.station.longitude", ycol = "end.station.latitude", crs = 4269, grid = FALSE)
mapview(drugaJc, xcol = "end.station.longitude", ycol = "end.station.latitude", crs = 4269, grid = FALSE)
mapview(trzeciaJc, xcol = "end.station.longitude", ycol = "end.station.latitude", crs = 4269, grid = FALSE)
mapview(czwartaJc, xcol = "end.station.longitude", ycol = "end.station.latitude", crs = 4269, grid = FALSE)



































