library(sf)
library(httr)
library(jsonlite)
library(tidyverse)

#  ------- FUNCIÓ = DADES API ---------
#  ------------------------------------

#    -) OBTENCIÓ DADES 

#    -) DADES de la API OPEN METEO
#    -) ATRIBUTS de la FUNCIÓ = LAT, LONG, DATA 1 i DATA 2

#    -) VULL EVITAR ELS ERROR de processar 2000 PUNTS
#    -) TANTS PUNTS avegades la API FALLA
#    -) He se saber que fer
#    -) CHATGPT m'ajuda


dades_API_error <- function(lat, long, date_1, date_2){
  
  resultat <- tryCatch({
    
    res_2 <- GET(
      "https://archive-api.open-meteo.com/v1/archive",
      query = list(
        latitude = lat,
        longitude = long,
        start_date = date_1,
        end_date = date_2,
        hourly = "temperature_2m,relative_humidity_2m,wind_speed_10m"
      ),
      timeout(20)
    )
    
    if(status_code(res_2) != 200){
      return(NULL)
    }
    
    text_2 <- content(res_2, "text", encoding = "UTF-8")
    dades_2 <- fromJSON(text_2)
    
    return(dades_2)
    
  }, error = function(e){
    
    message("Error API a lat=", lat, " long=", long, ": ", e$message)
    return(NULL)
    
  })
  
  return(resultat)
}


#  ------- COMPROVO LO DELS ERORS ---------
#  ------------------------------------


date_1 = "2025-11-07"
date_2 = "2025-11-07"

comarques_punts <- st_read("data/raw/COMARQUES_punts.shp")


num <- length(comarques_punts$id) 
num

n$hourly$temperature_2m


for (i in 1:num) {
  
  lat <- COORDS_create(comarques_punts[i,])$lat
  long <- COORDS_create(comarques_punts[i,])$long
  
  id <- comarques_punts[i,]$id
  n <- dades_API_error(lat,long,date_1,date_1)
  T_max <- (max(n$hourly$temperature_2m))
  
  print(paste(id,'-',T_max))
  
  
}

