library(sf)
library(httr)
library(jsonlite)
library(tidyverse)


# -------------- ERROR API (peta) ---------------
# ------------------------------------------------


#     -) NO se pk peró PETA en DADES_METO = list(create_DF_GEOM(lat, long, date, date))
#     -) La funció CREATE_DF_GEOM usa la API
#     -) Ple que veig de forma ALEATORIA quan uso la API per 2000 punts peta en algun punt
#     -) Avegades el 1300 a vegades 1900 i algo
#     -) Deu ser que si es conecta a internet moltes vegades peta
#     -) He de fer algo pk no peti
#     -) un canvin a la CRIDA de la API

#     -) Chat GPT em dona UNA SOLCUÓ
#     -) Crearé unes FUNCIONS NOVES
#     -) funcions_error.R

date <- "2025-11-07"

num <- length(comarques_punts$id) 
num


for (i in 1:5) {
  
  comarques_punts[i,]
  lat <- COORDS_create(comarques_punts[i,])$lat
  long <- COORDS_create(comarques_punts[i,])$long
  dades_meteo <- create_DF_GEOM(lat, long, date, date)
  id <- comarques_punts[i,]$id
  T_max <- dades_meteo$T_max
  T_min <- dades_meteo$T_min
  Hum_max <- dades_meteo$Hum_max
  Hum_min <- dades_meteo$Hum_min
  Win_max <- dades_meteo$Win_max
  Win_min <- dades_meteo$Win_min
  
  text <- paste(id,'-',T_max,'-',T_min,'-',Hum_max,'-',Hum_min,'-',Win_max,'-',Win_min )
  
  print(text)
  
}




#     -) Això PETA a la fila 1142
#     -) Comprovar que pot ser
#     -) Fare un FOR fila a fila i que escrigui que surt

comarques_punts %>%
  rowwise() %>%
  mutate(
    data = date,                       
    lat = COORDS_create(geometry)$lat,
    long = COORDS_create(geometry)$long,
    dades_meteo = list(create_DF_GEOM(lat, long, date, date)),
    T_max = dades_meteo$T_max,
    T_min = dades_meteo$T_min,
    Hum_max = dades_meteo$Hum_max,
    Hum_min = dades_meteo$Hum_min,
    Win_max = dades_meteo$Win_max,
    Win_min = dades_meteo$Win_min
  ) %>% 
  select(-dades_meteo)  %>%
  data.frame()



date <- as.Date(date)-1
date <- as.character(date)


resultats[[i+1]] <- catalunya




#  ------- COMPROVAR ERROÇRS DADES API ---------
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


