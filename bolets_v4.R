
library(sf)
library(dplyr)
library(readr)


# ----- DADES SHAPE -----
# -----------------------

#     -) SHAPE creat amb QGIS
#     -) Son les comarques calatanes amb una malla de punts
#     -) La Malla és de 5 x 5 km
#     -) Per tant cada comarca te dins de 20 a 50 punts
#     -) Acada punt puc saber Temp, Vent,...gràcies a la API Open Meteo

comarques_punts <- st_read("data/raw/COMARQUES_punts.shp")

# ----- FUNCIONS en ALTRE ARXIU -------
# ------------------------------------

source("scripts/funcions.R")


#   ---------- OBJECTIU DE LA APP ---------
#   ---------------------------------------

#     -) Crear una APP que ajudi a detectar zones idonees on hi hagi bolets
#     -) He de saber quines variables fisiques son idonees pels bolets
#     -) Començaré amb DADES CLIMÀTIQUES

#     DADES CLIMÀTIQUES:

#     -) Usare la API openmeteo per treure dades de
#     -) Venen de la API OPEN METEO

#     -) He creat FUNCIONS dins de ARXIU funcionR que obté dades
#     -) Les dades son per CADA COORDENADA
#     -) Les dades son TEMP, HUMITAT i VENT
#     -) De cada una tenim Max i MIN

#     DADES COMARCA

#     -) Per cada COMARCA tinc un shape de punts (comarques_punts)
#     -) Calcularé per cada COMARCA la mitja de tots el punts
#     -) Així sabré per COMARCA la MITJA de TEMP, HUMIT, VENT

#     EM FALTARIA DADES de:

#     -) Obtenir la PLUJA
#     -) La ALTITUD
#     -) El tipus de VEGETACIÓP



# -----  PROVA INDEX 7 DIES -----
# --------------------------------

#     -) Prova d'index amb dades de 7 dies
#     -) Començar amb UNA COMARCA
#     -) Calcular 7 dies 
#     -) I veure com puc calcular INDEXs 

#     -) Creeo una FUNCIÓ
#     -) Calculara diferents DF x diferents dies




# ----- DONA ERROR --- DADES UN DIA A CATALUNYA ----
# --------------------------------------------------

#     -) Faig una prova de CATALUNYA de UN DIA
#     -) Em dona ERROR calaular les DADES de CATALUNY de UN DIA
#     -) Es raro pk en BOLETS_V1 abans em funcionava aquesta opció i ara ja no
#     -) No se que pot ser!!???

#     -) És RARO ja que si cacul el CV comarca per comarca (Bolets_v3) si que va
#     -) És algo de quan calculo DADES de TOT CATALUNAY
#     -) I si no puc calcular DADES de tot CAT no podre calcualr CV de tot CAT 


system.time({

date <- "2025-11-01"

catalunya <- comarques_punts %>%
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

})




# ------------------- 
# --------------------




catalunya_setmana <- function(comarques_punts,date){
  
  date <- date
  resultats <- list()
  
    for (i in 0:6){
      
    catalunya <- comarques_punts %>%
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
    
  }
  
  return(list(
    dia_1 = resultats[[1]],
    dia_2 = resultats[[2]],
    dia_3 = resultats[[3]],
    dia_4 = resultats[[4]],
    dia_5 = resultats[[5]],
    dia_6 = resultats[[6]],
    dia_7 = resultats[[7]]
  ))
}



# -----  ANALISI ESTADÍSTIC -----
# -------------------------------

#     -) MITJANA
#     -) DESVIACIÓ
#     -) COFICIENT DE VARIACIÓ = CV

#     CV < 10%   = molt poc variable
#     CV 10–20%  = variabilitat moderada
#     CV >20%    = bastant variable
#     CV >30%    = molt variable



# -----   COFICIENT DE VARIACIÓ = CV -----
# ------------   UN PUNT      ------------

#     -) Calculo el CV x 3 dades
#     -) Depenent del % serà més o menys variable

#     CV < 10%   = molt poc variable
#     CV 10–20%  = variabilitat moderada
#     CV >20%    = bastant variable
#     CV >30%    = molt variable


#     < 10% → dades molt homogènies → la mitjana és molt representativa
#     10–20% → variabilitat moderada → la mitjana encara és bastant útil
#     >20% → la mitjana comença a perdre representativitat
#     >30% → molta dispersió → la mitjana sola pot ser enganyosa


# ---------- CREO FUNCIÓ COMARCA CV ------ 
# ----------------------------------------


#     -) Calculo automaticament CV x COMARCA
#     -) li passo DATA
#     -) DATA = es el resultat de la funció COMARCA_SETMANA
#     -) Que és un DF amb les DADES dels 7 DIES (temp, humitat,...)

#     -) la funció COMARCA CV fa:
#     -) calcula per cada punt = MITJANA i DESVIACIO ESTANDARD
#     -) Un cop les té calcula el CV = COFICIENT DE VARIACIÓ
#     -) CV pot ser <10% , 10%-20% ,....
#     -) Com més petit =  MENYS VARIACIÓ de les dades en una setamana
#     -) Si varia POC = podem dir que en una setmana el valor es la MITJA


catalunya_CV <- function(data){
  
  
  T_max_c <- c()
  H_max_c <- c()
  W_max_c <- c()
  
  
  num_punts <- length(data[[1]][[7]])
  
  df <- data.frame()
  
  for(p in 1:num_punts){
    
    for (i in 1:7) {
      T_ <- data[[i]][[7]][p]
      Hum_ <- data[[i]][[9]][p]
      Win_ <- data[[i]][[11]][p]
      
      
      T_max_c <- c(T_max_c,T_)
      H_max_c <- c(H_max_c,Hum_)
      W_max_c <- c(W_max_c,Win_)
      
      cv_T <- 100 * sd(T_max_c) / mean(T_max_c)
      cv_H <- 100 * sd(H_max_c) / mean(H_max_c)
      cv_W <- 100 * sd(W_max_c) / mean(W_max_c)
      
    }
    
    lat = data[[1]]$lat[p]
    long = data[[1]]$long[p]
    
    df <- rbind(
      df, data.frame(
        id = data[[1]]$id[p],
        lat = lat,
        long = long,
        T_mitja = mean(T_max_c),
        Hum_mitja = mean(H_max_c),
        Win_mitja = mean(W_max_c), 
        T_cv = cv_T, 
        Hum_cv =cv_H, 
        Win_cv = cv_W
      ))
    
  }
  
  return(df)
  
}

# ---------- PROCÉS DE CALCUL AUTOMATITZAT V2 ----
# ------------------------------------------------

#     -) 1r = COMARQUES_SETMANA()
#     -) 2n = COMARCA_CV
#     -) 3r = FER JOIN
#     -) 4rt = GUARDAR SHAPES


create_shape_catalunya <- function(points,data){
  
  dades_7 <- catalunya_setmana(points,data)
  
  df <- catalunya_CV(dades_7)
  
  shape_join <- comarques_punts %>%
    left_join(df, by = "id")
  
  return(shape_join)
  
}


# ----  CREAR SAHPE ----
# ----------------------


system.time({
  
catalunya <- create_shape_catalunya(comarques_punts,"2025-11-07")

})

# ----  GUARDAR SAHPE ----
# ----------------------

st_write(solsones, "data/processed/Solsones_2025_11_07_CV.shp", delete_layer = TRUE)




# -----------------
# -----------------

#     -) Si de cada punt amb 7 dies de dades calculo el CV
#     -) Podria classificar els punts en 1,2,3,4 = 

#     -) 1 = < 10% = i pertant la MITJA és el valor del PUNT
#     -) 2 = 10–20% ?= podriem agafar també la mitja
#     -) 3 - 3 = Massa VARIABILITAT = DEIXEM EL PUNT EN BLANC
#     -) Així al mapa de QGIS no es veurà res i no afectarà 


#     -) Si el CV es de 1 o 2 podria dir que les dades de T, H i W son la mitja
#     -) Pertant en QGIS visulitzaria només els punts de CV 1 i 2


