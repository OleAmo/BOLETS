library(sf)
library(httr)
library(jsonlite)
library(tidyverse)


#  ---------- API ERROR CONSUM DADES -----------
#  ---------------------------------------------

#    -) LÍMIT d'us de API OPEN METEO 
#    -) Per la API gratuïta d’Open-Meteo els límits són aproximadament:

#    -) 10.000 consultes al dia
#    -) 5.000 consultes per hora
#    -) 600 consultes per minut

#    -) He creat una funció DADES_API_ERROR
#    -) És pq si la API dona algun error (aprat del límit d'us)
#    -) La pugui detectar i NO PETI TOT



#  ------- COMPROVO LO DELS ERORS ---------
#  ----------------------------------------

comarques_punts <- st_read("data/raw/COMARQUES_punts.shp")

#  --- COMPROVO

date_1 = "2025-11-07"
date_2 = "2025-11-07"


num <- length(comarques_punts$id) 
num


vector <- c()
vector_2 <- c()

for (i in 1:9) {
  
  lat <- COORDS_create(comarques_punts[i,])$lat
  long <- COORDS_create(comarques_punts[i,])$long
  
  id <- comarques_punts[i,]$id
  dades_api <- dades_API_error(lat,long,date_1,date_1)
  T_max <- (max(dades_api$hourly$temperature_2m))
  Hum_max <- (max(dades_api$hourly$relative_humidity_2m))
  
  text <- paste(id,'-',T_max,'-',Hum_max)
  
  vector[i] <- text
  vector_2[i] <- c(T_max,Hum_max)
  
  print(text)
   
  
}


