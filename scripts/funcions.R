library(sf)
library(httr)
library(jsonlite)
library(tidyverse)

#  ------- FUNCIÓ = DADES API ---------
#  ------------------------------------

#    -) OBTENCIÓ DADES 

#    -) DADES de la API OPEN METEO
#    -) ATRIBUTS de la FUNCIÓ = LAT, LONG, DATA 1 i DATA 2

dades_API <- function(lat,long,date_1,date_2){
  
  res_2 <- GET(
    "https://archive-api.open-meteo.com/v1/archive",
    query = list(
      latitude = lat,
      longitude = long,
      start_date = date_1,
      end_date = date_2,
      hourly = "temperature_2m,relative_humidity_2m,windspeed_10m"
    )
  )  
  
  text_2 <- content(res_2, "text")
  dades_2 <- fromJSON(text_2)
  
  return(dades_2)
  
}


#  ------- FUNCIÓ = COMPROVA ERROSÇRS DADES API ---------
#  ------------------------------------------------------

#    -) LÍMIT d'us de API OPEN METEO 
#    -) Per la API gratuïta d’Open-Meteo els límits són aproximadament:

#    -) 10.000 consultes al dia
#    -) 5.000 consultes per hora
#    -) 600 consultes per minut

#    -) He creat una funció DADES_API_ERROR
#    -) És pq si la API dona algun error (aprat del límit d'us)
#    -) La pugui detectar i NO PETI TOT

#    SOLUCÍÓ CHATGPT = CONSULTES PER LOTS

#    Jo ara faig  

#    -) punt1 → GET(...)
#    -) punt2 → GET(...)
#    -) punt3 → GET(...)
#    -) ...
#    -) punt2000 → GET(...)

#    Jo he de fer  

#    -) consulta 1 → 100 punts
#    -) consulta 2 → 100 punts
#    -) consulta 3 → 100 punts

#    En R ho pots generar així: 
#    Open-Meteo permet passar vectors separats per comes:
  
  
#    -)   lat_v <- c(42.31,42.36,42.40)
#    -)   long_v <- c(2.65,2.71,2.80)

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
    
    # ---- DETECTAR ERROR del GET -------
    # -----------------------------------
    
    #    -) El STATUS_CODE = Detectata tipus de error del GET
    #    -) ERRORS típics:
    
    #    -) 200 = Tot correcte
    #    -) 404 = pàgina no trobada
    #    -) 403 = accés prohibit
    #    -) 429 = massa consultes
    #    -) 500 = error intern servidor
    #    -) 503 = servei temporalment caigut
    
    #    -) Pertant el IF si  STATUS_CODE diferent de 200 = dic NULL
    
    if(status_code(res_2) != 200){
      return(NULL)
      
    }
    
    text_2 <- content(res_2, "text", encoding = "UTF-8")
    dades_2 <- fromJSON(text_2)
    
    # ---- NOVA COMPROVACIÓ API -------
    # ---------------------------------
    
    # La API pot respondre 200 però retornar:
    # $error = TRUE
    # $reason = "Daily API request limit exceeded"
    
    if(!is.null(dades_2$error)){
      
      message(
        "Error API a lat=",lat,
        " long=",long,
        " : ",dades_2$reason
      )
      
      return(NULL)
    }
    
    return(dades_2)
    
    
    # ---- DETECTAR ERROR del TRY CACH -------
    # ----------------------------------------
    
    #    -) Si falla aplcio FUNCTION(e) = FUNCIO ERROR
    #    -) e$message =  És el missatge que donar d'error
    
  }, error = function(e){
    
    message("Error API a lat=", lat, " long=", long, ": ", e$message)
    return(NULL)
    
  })
  
  return(resultat)
}




# --------- CREACIÓ DADES en f(x) DIES ----------
# ------------------------------------------------

#   -) FUNCIO ATRIBUTS = DADES (Meteo), DATA (Inci i Final)
#   -) CALCULARÀ = Max, Min = Temp, Humitat, Vent
#   -) Ho DONA en format VECTOR


dades_create <- function(df,dia_Inici,dia_Final){
  
  df <- data.frame(df)
  
  dia_1 <- as.Date(dia_Inici)
  dia_2 <- as.Date(dia_Final)
  num <- as.integer(dia_2-dia_1)+1
  num
  
  max <- c()
  min <- c()
  a = 1
  b = a + 23
  
  max <- c(max,max(df[a:b,]))
  min <- c(min,min(df[a:b,]))
  
  
  if (dia_Inici==dia_Final){    # evito el error de processar Data Inicial = Data final
    max <- max
    min <- min
    
  } else {
    
    for (i in 1:(num-1)){
      a <- b + 1
      b <- b + 24
      max <- c(max,max(df[a:b,]))
      min <- c(min,min(df[a:b,]))
    }
    
  }
  
  return(list(
    max = max,
    min = min
  ))
  
}

# ----------- CREACIÓ DIES en f(x) DIES ----------
# ------------------------------------------------

#   -) FUNCIÓ ATRIBUT = Dia Inci i Final
#   -) CREA Vector = Del 1r a l'ultim dia
#   -) PREVEU l cas que DIA INICI = FINAL 


dies_create <- function(df,dia_Inici,dia_Final){
  
  if(dia_Inici == dia_Final){    # evito el error de processar DIA Inici = DIA Final
    dies <- c()
    dies <- c(dia_Inici)
    
  } else {
    
    df <- data.frame(df[1])
    
    
    dia_1 <- as.Date(dia_Inici)
    dia_2 <- as.Date(dia_Final)
    num <- as.integer(dia_2-dia_1)+1
    num
    
    dies <- c()
    a = 1
    b = a + 23
    
    dia_max <- max(df[a:b,]) 
    dia_max <- str_split_1(dia_max, "T")[1]
    
    dies <- c(dies,dia_max)
    
    
    for (i in 1:(num-1)){
      a <- b + 1
      b <- b + 24
      
      dia_max <- max(df[a:b,])
      dia_max <- str_split_1(dia_max, "T")[1]
      
      dies <- c(dies,dia_max)
    }
    
  }
  
  return(dies)
  
}


# ----------- CREACIÓ DATA FRAME en f(x) DIES ----------
# -------------------------------------------------------

#   -) FUNCIO ATRIBUT = Dades, Data Inci i Final
#   -) CREA UN DATA FRAME
#   -) Tindrà 7 COLUMNES = Dies, Temp Max, Temp Min,....
#   -) Les FILES son els DIES DIFERENTS

#   -) Dins seu té 2 FUNCIONS que ja gestionen les dades en f(x) dels dies:

#        -) dies_create() 
#        -) dades_create()


DF_create <- function(dades,dia_Inici,dia_Final){
  
  df <- data.frame(dades)
  
  t <- df[2]
  hum <- df[3]
  w <- df[4]
  
  #t <- df$hourly.temperature_2m
  #hum <- df$hourly.relative_humidity_2m
  #w <- df$hourly.windspeed_10m
  
  
  
  dies <- dies_create(df,dia_Inici,dia_Final)
  
  t_dades <- dades_create(t,dia_Inici,dia_Final)
  hum_dades <- dades_create(hum,dia_Inici,dia_Final)
  w_dades <- dades_create(w,dia_Inici,dia_Final)
  
  
  resultat <- data.frame(
    Dies = dies,
    T_max = t_dades$max,
    T_min = t_dades$min,
    Hum_max = hum_dades$max,
    Hum_min = hum_dades$min,
    Win_max = w_dades$max,
    Win_min = w_dades$min
    
  )
  
  
  return(resultat)
  
  
}

#df <- dades_API(41.4051879,1.9964941,"2026-03-02","2026-03-03")
#data <- dades_create(df,"2026-03-02","2026-03-03")
#data


#  ------- FUNCIÓ ASSIGNAR GEOMETRIA ---------
#  -------------------------------------------


#   -) FUNCIO ATRIBUTS = DataFrame, Long i Lat
#   -) CREO Columna GEOMETRIA
#   -) Transformo el CRS a 25831

#   -) CADA FILA és el MATEIX PUNT
#   -) Ja que cada FILA és DIA DIFERENT però MATEIX PUNT


#   -) Com REPETIR GEOMETRIA a cada fila?
#   -) FAIG funcio REP() = Repetir
#   -) nrow() = PER CADA FILA

#   -) I ho guardo a la carpeta PROCESSED com a SHAPE


assign_Geom <- function(dades,long,lat){
  
  geom <- st_sfc(st_point(c(long, lat)),crs = 4326) %>%
    st_transform(25831)
  
  df_meteo_geom <- st_sf(
    dades ,
    geometry = rep(geom, nrow(dades))
  )
  
  return(df_meteo_geom)
  
}


#  ------- FUNCIÓ DATA FRAME AMB GEOMETRIA ---------
#  -------------------------------------------------


#   -) FUNCIÓ ATRIBUTS = Lat, Long, Dat Inci, Final
#   -) Aplico TOTES les Funcions ANTERIORS
#   -) Em dona un DATA FRAME:
#         +) 8 Columnes = Dies, Temp_max, Temp_min,...Geometry
#         +) Tantes files com DIES DIFERENTS



create_DF_GEOM <- function(lat,long,data_1,data_2){
  
  dades_api <- dades_API(lat,long,data_1,data_2)
  
  dades_api_processed <- DF_create(dades_api$hourly,data_1,data_2)
  
  dades_api_processed_geom <- assign_Geom(dades_api_processed,long,lat)
  
  return(dades_api_processed_geom)
  
}

create_DF_NO_GEOM <- function(lat,long,data_1,data_2){
  
  dades_api <- dades_API(lat,long,data_1,data_2)
  
  dades_api_processed <- DF_create(dades_api$hourly,data_1,data_2)
  
  return(dades_api_processed)
  
}



#  ------- FUNCIÓ = LAT i LONG ---------
#  -------------------------------------

#    -) OBTENCIÓ de la LATITUD i LONGITUD
#    -) De Atributs hi ha una GEOMETRIA
#    -) La transformo a 4326 = la projecció de API METEO


COORDS_create <- function(dades){
  
  coord_df <- dades %>% 
    st_transform(crs = 4326) %>% 
    st_coordinates(comarques_punts[1,])
  
  long <- coord_df[1] 
  lat <- coord_df[2]
  
  
  resultat <- data.frame(
    lat = lat,
    long = long
  )
  
  return(resultat)
  
}


