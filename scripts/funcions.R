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



