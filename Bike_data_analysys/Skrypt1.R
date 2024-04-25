library(microbenchmark)
library(dplyr)
library(data.table)
install.packages("geosphere")


library(geosphere)
library(sqldf)


NY_1 <- read.csv("dane/201801-citibike-tripdata.csv")
NY_4 <- read.csv("dane/201804-citibike-tripdata.csv")
NY_7 <- read.csv("dane/201807-citibike-tripdata.csv")
NY_10 <- read.csv("dane/201810-citibike-tripdata.csv")

JC_1 <- read.csv("dane/JC-201801-citibike-tripdata.csv")
JC_4 <- read.csv("dane/JC-201804-citibike-tripdata.csv")
JC_7 <- read.csv("dane/JC-201807-citibike-tripdata.csv")
JC_10 <- read.csv("dane/JC-201810-citibike-tripdata.csv")


#funkcje do obserwacji3
funkcja_pomocnicza_jeden <- function(start, stop) {
  
  start <- as.integer(start)
  stop <- as.integer(stop)
  
  res = 0
  
  if (start < 400) {
    if (stop > 0) {
      if (start > 0) {
        if (stop <= 400) {
          res = stop - start
          
          if (stop%%100 < start%%100) {
            res <- res - 40
          } 
        } else {
          res = 360 - start
        }
      } else {
        if (stop <= 400){
          res = stop
          
          if (stop%%100 < start%%100) {
            res <- res - 40
          }  
        } else {
          res = 400
        }
      }
    }
  }
  
  return(floor((res/100))*60 + floor((res%%100)/60) * 60 + (res %% 100) %% 60)
}

funkcja_pomocnicza_dwa <- function(start, stop) {
  
  start <- as.integer(start)
  stop <- as.integer(stop)
  
  res = 0
  
  if (start < 800) {
    if (stop > 400) {
      if (start > 400) {
        if (stop <= 800) {
          res = stop - start
          
          if (stop%%100 < start%%100) {
            res <- res - 40
          } 
        } else {
          res = 760 - start
        }
      } else {
        if (stop <= 800){
          res = stop - 400
          
          if (stop%%100 < start%%100) {
            res <- res - 40
          }  
        } else {
          res = 400
        }
      }
    }
  }
  
  return(floor((res/100))*60 + floor((res%%100)/60) * 60 + (res %% 100) %% 60)
}

funkcja_pomocnicza_trzy <- function(start, stop) {
  
  start <- as.integer(start)
  stop <- as.integer(stop)
  
  res = 0
  
  if (start < 1200) {
    if (stop > 800) {
      if (start > 800) {
        if (stop <= 1200) {
          res = stop - start
          
          if (stop%%100 < start%%100) {
            res <- res - 40
          } 
        } else {
          res = 1160 - start
        }
      } else {
        if (stop <= 1200){
          res = stop - 800
          
          if (stop%%100 < start%%100) {
            res <- res - 40
          }  
        } else {
          res = 400
        }
      }
    }
  }
  
  return(floor((res/100))*60 + floor((res%%100)/60) * 60 + (res %% 100) %% 60)
}

funkcja_pomocnicza_cztery <- function(start, stop) {
  
  start <- as.integer(start)
  stop <- as.integer(stop)
  
  res = 0
  
  if (start < 1600) {
    if (stop > 1200) {
      if (start > 1200) {
        if (stop <= 1600) {
          res = stop - start
          
          if (stop%%100 < start%%100) {
            res <- res - 40
          } 
        } else {
          res = 1560 - start
        }
      } else {
        if (stop <= 1600){
          res = stop - 1200
          
          if (stop%%100 < start%%100) {
            res <- res - 40
          }  
        } else {
          res = 400
        }
      }
    }
  }
  
  return(floor((res/100))*60 + floor((res%%100)/60) * 60 + (res %% 100) %% 60)
}

funkcja_pomocnicza_piec <- function(start, stop) {
  
  start <- as.integer(start)
  stop <- as.integer(stop)
  
  res = 0
  
  if (start < 2000) {
    if (stop > 1600) {
      if (start > 1600) {
        if (stop <= 2000) {
          res = stop - start
          
          if (stop%%100 < start%%100) {
            res <- res - 40
          } 
        } else {
          res = 1960 - start
        }
      } else {
        if (stop <= 2000){
          res = stop - 1600
          
          if (stop%%100 < start%%100) {
            res <- res - 40
          }  
        } else {
          res = 400
        }
      }
    }
  }
  
  return(floor((res/100))*60 + floor((res%%100)/60) * 60 + (res %% 100) %% 60)
}

funkcja_pomocnicza_szesc <- function(start, stop) {
  
  start <- as.integer(start)
  stop <- as.integer(stop)
  
  res = 0
  
  if (start < 2400) {
    if (stop > 2000) {
      if (start > 2000) {
        if (stop <= 2400) {
          res = stop - start
          
          if (stop%%100 < start%%100) {
            res <- res - 40
          } 
        } else {
          res = 2360 - start
        }
      } else {
        if (stop <= 2400){
          res = stop - 2000
          
          if (stop%%100 < start%%100) {
            res <- res - 40
          }  
        } else {
          res = 400
        }
      }
    }
  }
  
  return(floor((res/100))*60 + floor((res%%100)/60) * 60 + (res %% 100) %% 60)
}

# zamienia sekundy na minuty
czas_przejazdu <- function(df) {
  res <- df
  
  res$tripduration <- round((res$tripduration / 60), 1)
  
  return(res)
}

#zmienia rok urodzenia na ilo?? lat
formatowanie_wieku <- function(df) {
  res <- df
  
  res$birth.year <- (res$birth.year - 2018) * -1
  
  return(res)
}

#zmienia ilo?? lat na kategorie wieku:
kategorie_wiekowe <- function(df) {
  res <- df
  
  res <- res[res$birth.year < 75,]
  
  res$birth.year[res$birth.year < 30] <- "16-30"
  res$birth.year[res$birth.year >= 30 & res$birth.year < 45] <- "30-45"
  res$birth.year[res$birth.year >= 45 & res$birth.year < 60] <- "45-60"
  res$birth.year[res$birth.year >= 60 & res$birth.year < 75] <- "60-75"
  
  colnames(res)[14] <- "age"
  
  return(res)
}

#wylicza d?ugo?? przejazdu w lini prostej
dlugosc_przejazdu <- function(df) {
  res <- df
  
  res$tripdistance <- mapply(lat_a=res$start.station.latitude, lon_a=res$start.station.longitude, lat_b=res$end.station.latitude, lon_b=res$end.station.longitude,FUN = dist_geo)
  
  return(res)
  
}

#wylicza predkosc przejazdu
predkosc_przejazdu <- function(df) {
  res <- df
  
  res$tripspeed <- res$tripdistance/(res$tripduration/60)
  
  return(res)
}

#pomocnicza funkcja do dlugosc_przejazdu()
dist_geo <- function(lat_a, lon_a, lat_b, lon_b){
  
  round(distm(c(lon_a, lat_a), c(lon_b, lat_b), fun = distHaversine)/1000,2)
  
} 

#wybieranie subskrybentow (rok)
subscribers <- function(df) {
  res <- df
  res <- res[res$usertype == "Subscriber",]
  
  return(res)
}

#wybieranie zwyklych klientow (1,3 dni)
customers <- function(df) {
  res <- df
  res <- res[res$usertype == "Customer" & res$tripduration < 4320,] # mniej ni? 3 dni
  
  return(res)
}

#tworzenie odpowiedniej ramki danych
ramka <- function(df) {
  res <- df
  
  res <- czas_przejazdu(res)
  res <- dlugosc_przejazdu(res)
  res <- predkosc_przejazdu(res)
  res <- formatowanie_wieku(res)
  res <- kategorie_wiekowe(res)
  
  res <- res[res$tripduration < 4320,]
  return(res)
}

#tworzenie jednej duzej macierzy z 8
jedna_macierz <- function(a,b,c,d,e,f,g,h) {
  res <- rbind(a,b,c,d,e,f,g,h)
  
  return(res)
}




#ramka do obserwacji
obserwacja <- function(df) {
  
  res <- df
  
  res <- sqldf("SELECT
                age,
                AVG(tripduration) as AverageTripduration,
                AVG(tripdistance) as AverageTripdistance,
                AVG(tripspeed) as AverageTripspeed,
                COUNT(*) AS Number
                FROM res
                GROUP BY age
                ORDER BY age")
  return(res)
} #avg duration,distance, speed wg wieku

obserwacja2 <- function(df) {
  
  res <- df
  
  res <- sqldf("SELECT
                AVG(tripduration) as AverageTripduration,
                AVG(tripdistance) as AverageTripdistance,
                AVG(tripspeed) as AverageTripspeed
                FROM res")
  return(res)
} #avg duration,distance og?lnie

obserwacja3 <- function(df) {
  res <- df
  
  res <- sqldf("SELECT
                STRFTIME('%H%M', starttime) AS starttime,
                STRFTIME('%H%M', stoptime) AS stoptime
                FROM res")
  res <- res[res$starttime < res$stoptime,]
  
  res <- na.omit(res)
  
  res$jeden <- mapply(funkcja_pomocnicza_jeden, res$starttime, res$stoptime)
  
  res$dwa <- mapply(funkcja_pomocnicza_dwa, res$starttime, res$stoptime)
  
  res$trzy <- mapply(funkcja_pomocnicza_trzy, res$starttime, res$stoptime)
  
  res$cztery <- mapply(funkcja_pomocnicza_cztery, res$starttime, res$stoptime)
  
  
  return(res)
} #start, stop, przedzialy czasowe

obserwacja4 <- function(df) {
  res <- df
  
  res <- sqldf("SELECT
                age,
                usertype,
                STRFTIME('%H%M', starttime) AS starttime,
                STRFTIME('%H%M', stoptime) AS stoptime
                FROM res")
  res <- res[res$starttime < res$stoptime,]
  
  res <- na.omit(res)
  
  res$jeden <- mapply(funkcja_pomocnicza_jeden, res$starttime, res$stoptime)
  
  res$dwa <- mapply(funkcja_pomocnicza_dwa, res$starttime, res$stoptime)
  
  res$trzy <- mapply(funkcja_pomocnicza_trzy, res$starttime, res$stoptime)
  
  res$cztery <- mapply(funkcja_pomocnicza_cztery, res$starttime, res$stoptime)
  
  res <- sqldf("SELECT
                age,
                usertype,
                AVG(jeden) AS '0-6',
                AVG(dwa) AS '6-12',
                AVG(trzy) AS '12-18',
                AVG(cztery) AS '18-24'
                FROM res
                GROUP BY age, usertype
                ORDER BY usertype, age")
  
  return(res)
} # podzial na wiek i subskrybentow i srednia godzinowa uzywalnosc

obserwacja5 <- function(df) {
  
  res <- df
  
  res <- sqldf("SELECT
                age,
                usertype,
                STRFTIME('%H%M', starttime) AS starttime,
                STRFTIME('%H%M', stoptime) AS stoptime
                FROM res")
  
  res <- res[res$starttime < res$stoptime,]
  
  res <- na.omit(res)
  
  res$jeden <- mapply(funkcja_pomocnicza_jeden, res$starttime, res$stoptime)
  
  res$dwa <- mapply(funkcja_pomocnicza_dwa, res$starttime, res$stoptime)
  
  res$trzy <- mapply(funkcja_pomocnicza_trzy, res$starttime, res$stoptime)
  
  res$cztery <- mapply(funkcja_pomocnicza_cztery, res$starttime, res$stoptime)
  
  res$piec <- mapply(funkcja_pomocnicza_piec, res$starttime, res$stoptime)
  
  res$szesc <- mapply(funkcja_pomocnicza_szesc, res$starttime, res$stoptime)
  
 res <- sqldf("SELECT
               age,
               usertype,
               godzina,
               AVG(wartosc) AS wartosc
               FROM (
                 SELECT
                 age,
                 usertype,
                 godzina,
                 CASE
                   WHEN  godzina == '0-4' THEN jeden
                   WHEN  godzina == '4-8' THEN dwa
                   WHEN  godzina == '8-12' THEN trzy
                   WHEN  godzina == '12-16' THEN cztery
                   WHEN  godzina == '16-20' THEN piec
                   WHEN godzina == '20-24' THEN szesc
                 END AS wartosc
                 FROM
                   (SELECT CASE MAX(jeden, dwa, trzy, cztery, piec, szesc)
                     WHEN jeden  THEN '0-4'
                     WHEN dwa  THEN '4-8'
                     WHEN trzy THEN '8-12'
                     WHEN cztery THEN '12-16'
                     WHEN piec THEN '16-20'
                     WHEN szesc THEN '20-24'
                   END AS godzina, *
                   FROM   res
                 )
                 WHERE wartosc > 5
               )
               GROUP BY usertype, age")


  return(res)
} # grupy wiekowe i subskrybentow i godziny w ktorych spedzaja czasu i ile srednio

#Prezentacja

#generowanie ramki

big <- jedna_macierz(NY_1,NY_4,NY_7,NY_10,JC_1,JC_4,JC_7,JC_10)

df <- ramka(big)

#slajd 1:

s1 <- obserwacja(df)

sub <- subscribers(df) # liczymy ilosc elementow w R w Environment

cust <- customers(df) # liczymy ilosc elementow w R w Environment

#Slajd 2:

s2 <- s1

s2_cust <- obserwacja(cust)

s2_sub <- obserwacja(sub)

#Slajd 3:

s3_sub <- obserwacja(sub)

s3_cust <- obserwacja(cust)

#Slajd 4:
  
s4_sub <- obserwacja2(sub)

s4_cust <- obserwacja2(cust)

#Slajd 5:

s5 <- obserwacja5(df) # zauwazamy: w jakich godzinach podrozuja najczesciej subskrybenci, klienci, 
                      #            ile czasu poswiecaja wtedy jezdzie            

NY_1 <- NULL
NY_4 <- NULL
NY_7 <- NULL
NY_10 <- NULL

JC_1 <- NULL
JC_4 <- NULL
JC_7 <- NULL
JC_10 <- NULL




