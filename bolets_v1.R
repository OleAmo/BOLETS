
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


# -------  CREAR FUNCIÓ per obtenir COORDENADES ------
# ----------------------------------------------------

#     -) Vull una funció que passi de GEMETRIA a coordenades
#     -) Intdrodueixes una geometria 
#     -) Obtens LAT i LONG en porjecció 4326 = La que usa la API OPEN METEO

#     -) He creat la funció al arxu = funcions.R


#     COMPROVACIÓ
# ----------------

#     -) Comprovo que la funcio de crear LAT i LONG funciona
#     -) La uso i ho compmrovo
#     -) De pas comprovo que la funció de obtenir DADES de METEO API funciona

coords <- COORDS_create(comarques_punts[14,])

date <- "2026-05-10"
lat <- coords$lat
long <- coords$long

EXEMPLE <- create_DF_GEOM(lat,long,date,date)
EXEMPLE$Win_max 


# -----  CREAR TEMP TOTS PUNTS DE COMARCA -----
# ---------------------------------------------

#     -) Vull una DF de CADA COMARCA 
#     -) De cada comarca tinc una MALLA de 5000 m
#     -) Per tant de cada 5 km tinc DADES

#     CREAR DF:
#     -) Creo una columna amb LAT i LONG
#     -) Pk el MUTATE agafi la GEOMETRY de cada fila he de usar = rowwise()
#     -) Així m'hagafarà la geometria de CADA FILA



system.time({ })   # funció per saber que tarda una funció a fer-se

# ------ Exemple UNA COMARCA ------
# ---------------------------------

#     -) Exemple de creació de PUNTS de UNA COMARCA
#     -) Així ho visualitzo en QGIS i em faig una idea de distribucions
#     -) Una comarca tarda de 2 a 4 segons
#     -) Per tant 42 Comarques = 126 segons

date <- "2025-11-07" # és una data de TARDOR
comarca <- comarques_punts %>%
filter(NOMCOMAR == "Baix Empordà") %>%
rowwise() %>%
mutate(
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
  
comarca


# -----  HO PASSO A SHAPE -----
# -----------------------------

#     -) Ho passo a SHAPE per veure patrons visuals amb QGIS
#     -) No tinc clar com analitzar les dades de CATALUNYA

#     -) Una opció és CALCULAR la mitja de CADA COMARCA
#     -) Però llavors perdo AFINITAT a l'hora de trobar llocs idonis per bolets

#     -) Crec que ANALITZARÉ TOTS el punts de catalunya
#     -) I buscaré PATRONS, TAQUES o ILLES de punts idonis

#     -) HE PASSAT A SHAPE
#     -) Alt Empordà, Baix Empordà, Pla de l'Estany, Garrotxa i Gironès

comarca

st_write(comarca, "data/processed/Baix_Emporda.shp", delete_layer = TRUE)


# ------ Exemple TOTES COMARQUES ------
# ---------------------------------

#     -) Creo un DF amb TOTS els punts de CATALUNYA
#     -) De cada un calculo les DADES (T_max, T_min, Hum,...)

#     -) En fer TOTES les comarques la funció ha tardat 143 seg = 2.5 min
#     -) Les gravo a SHAPE
#     -) Així puc mirar patrons visuals


date <- "2025-11-07" # és una data de TARDOR
system.time({ 
  comarques <- comarques_punts %>%
    rowwise() %>%
    mutate(
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

st_write(comarques, "data/processed/comarques_2025_11_07.shp", delete_layer = TRUE)

comarques_2025_11_07 <- st_read("data/processed/comarques_2025_11_07.shp")

comarques_2025_11_07


#     EXEMPLE D'INDEX
# -------------------

#     -) Són dades tretes ràpidament del GOOGLE
#     -) He de buscar informació més precissa

#     -) Rang de HUMITAT Max = 80-90 %
#     -) Rang de HUMITAT Min =
#     -) Rang de Temperatuta Max =  10-18 °C
#     -) Rang de Temperatuta Min = >= 5
#     -) Rang de Vent Max = 4 punts x sobre de la minima

#     -) INDEX = de 0 a 8 punts
#     -) 8 Punts = Té TOTES les CONDICIONS ÒPTIMES

#     -) INDEX 2 = de 0 a 4 punts
#     -) 8 Punts = Té TOTES les CONDICIONS MAXIMES ÒPTIMES

#     -) Cada punt del mapa tindrè un màxim de 8 punts
#     -) Si té la Tmax ideal = +1, si té T_min ideal = +1 ,....

#     -) Pertant el PUNTS amb 8 punts tindran el màxim de valor x creixer bolets


comarques_2025_11_07_index <- comarques_2025_11_07 %>%
  
  mutate(
    T_max_opt = ifelse(T_max >= 10 & T_max <= 18, 1, 0),
    T_min_opt = ifelse(T_min >= 5 & T_min < 10, 1, 0),
    Hum_max_opt = ifelse(Hum_max >= 80 & Hum_max <= 90, 1, 0),
    Hum_min_opt = ifelse(Hum_min >= 70 & Hum_min <= 80, 1, 0),
    Win_max_opt = ifelse(Win_max >= 4 & Win_max <= 8, 1, 0),
    Win_min_opt = ifelse(Win_min >= 2 & Win_min <= 3, 1, 0),
    Index = T_max_opt+T_min_opt+Hum_max_opt+Hum_min_opt+Win_max_opt+Win_min_opt,
    Index_2 = T_max_opt+Hum_max_opt+Win_max_opt
    
  )

comarques_2025_11_07_index

# ---- GUARDAR SHAPE
# ------------------

#     -) Guardo i visualitzo amb QGIS

st_write(comarques_2025_11_07_index, "data/processed/comarques_2025_11_07_index.shp", delete_layer = TRUE)



# *****************************************
# ----------      FEINA A FER    ----------
# *****************************************

#     -) He de pensar que fer
#     -) He de trobar els PUNTS IDONIS pel CREIXEMENT de bolets

#     PUNTS IDONIS:

#     -) Què vol dir PUNTS IDONIS?
#     -) Poc Vent + Molta Humitat + Molta Temp?
#     -) Fa falta la pluja?
#     -) Quin sòn els RANGS IDEALS?

#     MILLORAR ÍNDEX:

#     -) Buscar dades més correctes
#     -) Buscar quines son les DADES que MES AFECTEN
#     -) Potser la temperatura te menys pes i el VENT afecta MOLT
#     -) Haures de usar % per fer el índex final

#     -) Comprovar que les DADES API són correctes
#     -) Potser és més precis amb les dades de la API del METEOCAT?

#     -) Calculo també els ÚLTIMS DIES?
#     -) Tinc en compte els ÚLTIMS DIES? Quans dies SON?

#     -) Un cop fet ho passo a SHINY



# -----  PROVA INDEX 7 DIES -----
# --------------------------------

#     -) Prova d'index amb dades de 7 dies
#     -) Començar amb UNA COMARCA
#     -) Calcular 7 dies 
#     -) I veure com puc calcular INDEXs 

#     -) Creeo una FUNCIÓ
#     -) Calculara diferents DF x diferents dies


comarques_setmana <- function(comarques_punts,date){
  
  date <- date
  resultats <- list()
  
    for (i in 0:6){
      
    comarca <- comarques_punts %>%
      filter(NOMCOMAR == "Berguedà") %>%
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
    
    resultats[[i+1]] <- comarca
    
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


# ---- QUAN TARDA a PROCESSAR UNA COMARCA ?
# ---- PERÍODE de 7 dies ------------------

#     -) Trda 52 seg
#     -) Es el temps per obtenir els 7 DF


system.time({
 exemple <- comarques_setmana(comarques_punts,"2025-11-07")
}) 

# ---- GUARDAR SHAPE
# ---- vISUALITZO amb QGIS
# ------------------------

#     -) Guardo els SHAPES
#     -) visualitzo amb QGIS


exemple$dia_1[4][1,]
exemple$dia_2[4][1,]
exemple$dia_3[4][1,]
exemple$dia_4[4][1,]
exemple$dia_5[4][1,]
exemple$dia_6[4][1,]
exemple$dia_7[4][1,]

st_write(exemple$dia_1, "data/processed/Bergueda_2025_11_07.shp", delete_layer = TRUE)
st_write(exemple$dia_2, "data/processed/Bergueda_2025_11_06.shp", delete_layer = TRUE)
st_write(exemple$dia_3, "data/processed/Bergueda_2025_11_05.shp", delete_layer = TRUE)
st_write(exemple$dia_4, "data/processed/Bergueda_2025_11_04.shp", delete_layer = TRUE)
st_write(exemple$dia_5, "data/processed/Bergueda_2025_11_03.shp", delete_layer = TRUE)
st_write(exemple$dia_6, "data/processed/Bergueda_2025_11_02.shp", delete_layer = TRUE)
st_write(exemple$dia_7, "data/processed/Bergueda_2025_11_01.shp", delete_layer = TRUE)



#     -) Ara he de CALCULAR LA MITJA
#     -) I extreuren informació

#     EXEMPLE:

#     -) T_MAX
#     -) per PUNT 1
#     -) als 7 DIES

#  Ho busco en funció de $ 

exemple$dia_1$T_max[1]
exemple$dia_2$T_max[1]
exemple$dia_3$T_max[1]
exemple$dia_4$T_max[1]
exemple$dia_5$T_max[1]
exemple$dia_6$T_max[1]
exemple$dia_7$T_max[1]


#  Ho busco en funció NÚMERO

# [[1]]   = DIA
# [[7]]   = Columna 7 = T_max  --> (9 = Hum_Max / 11 = Win_max )
# [1]     = Punt 1 de la comarca

exemple[[1]][[7]][1]
exemple[[2]][[7]][1]
exemple[[3]][[7]][1]


# -----  ANALISI TIPUS 1 -----
# ----------------------------

#     -) Creo 3 STRINGS
#     -) són les dades de 7 dies de 3 variables
#     -) Creo 1 Vector
#     -) Per analitzar VARIANÇA


T_max <- "T_max = "
Hum_max <- "Hum_max = "
Win_max <- "Win_max = "

T_max_c <- c()

for (i in 1:7) {
  T_ <- exemple[[i]][[7]][1]
  Hum_ <- exemple[[i]][[9]][1]
  Win_ <- exemple[[i]][[11]][1]
  
  T_max <- paste0(T_max,T_," - ")
  Hum_max <- paste0(Hum_max,Hum_," - ")
  Win_max <- paste0(Win_max,Win_," - ")
  
  T_max_c <- c(T_max_c,T_)
  
}

#     -) Creo 3 STRINGS
#     -) són les dades de 7 dies de 3 variables

print(T_max)
print(Hum_max)
print(Win_max)


# -----  ANALISI ESTADÍSTIC -----
# -------------------------------

#     -) MITJANA
#     -) DESVIACIÓ
#     -) COFICIENT DE VARIACIÓ = CV

#     CV < 10%   = molt poc variable
#     CV 10–20%  = variabilitat moderada
#     CV >20%    = bastant variable
#     CV >30%    = molt variable



mean(T_max_c)  # mitja
sd(T_max_c)    # desviació

#     -) COFICIENT DE VARIACIÓ = CV

cv_percent <- 100 * sd(T_max_c) / mean(T_max_c)
cv_percent


# -----   COFICIENT DE VARIACIÓ = CV -----
# ------------   UN PUNT      ------------

#     -) Calculo el CV x 3 dades
#     -) Depenent del % serà més o menys variable


T_max_c <- c()
H_max_c <- c()
W_max_c <- c()

for (i in 1:7) {
  T_ <- exemple[[i]][[7]][1]
  Hum_ <- exemple[[i]][[9]][1]
  Win_ <- exemple[[i]][[11]][1]


  T_max_c <- c(T_max_c,T_)
  H_max_c <- c(H_max_c,Hum_)
  W_max_c <- c(W_max_c,Win_)
  
  cv_T <- 100 * sd(T_max_c) / mean(T_max_c)
  cv_H <- 100 * sd(H_max_c) / mean(H_max_c)
  cv_W <- 100 * sd(W_max_c) / mean(W_max_c)
  
}

cv_T  
cv_H  
cv_W

#     CV < 10%   = molt poc variable
#     CV 10–20%  = variabilitat moderada
#     CV >20%    = bastant variable
#     CV >30%    = molt variable


#     < 10% → dades molt homogènies → la mitjana és molt representativa
#     10–20% → variabilitat moderada → la mitjana encara és bastant útil
#     >20% → la mitjana comença a perdre representativitat
#     >30% → molta dispersió → la mitjana sola pot ser enganyosa


# -----   COFICIENT DE VARIACIÓ = CV -----
# ------------   TOTS PUNTS    ------------

#     -) 


T_max_c <- c()
H_max_c <- c()
W_max_c <- c()


num_punts <- length(exemple[[i]][[7]])

df <- data.frame()

for(p in 1:num_punts){
  
  for (i in 1:7) {
      T_ <- exemple[[i]][[7]][p]
      Hum_ <- exemple[[i]][[9]][p]
      Win_ <- exemple[[i]][[11]][p]
      
      
      T_max_c <- c(T_max_c,T_)
      H_max_c <- c(H_max_c,Hum_)
      W_max_c <- c(W_max_c,Win_)
      
      cv_T <- 100 * sd(T_max_c) / mean(T_max_c)
      cv_H <- 100 * sd(H_max_c) / mean(H_max_c)
      cv_W <- 100 * sd(W_max_c) / mean(W_max_c)
      
  }
  
  lat = exemple[[1]]$lat[p]
  long = exemple[[1]]$long[p]
  
  df <- rbind(
    df, data.frame(
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

df

#  --- HO TRANSFORMO A SHAPE 
# ------ per veureho en QGIS

shape <- st_as_sf(
  df,
  coords = c("long","lat"),
  crs = 25831
)

st_write(shape, "data/processed/Bergueda_CV.shp", delete_layer = TRUE)



# -----------------
# -----------------

#     -) Si de cada punt amb 7 dies de dades calculo el CV
#     -) Podria classificar els punts en 1,2,3,4 = 

#     -) 1 = < 10% = i pertant la MITJA és el valor del PUNT
#     -) 2 = 10–20% ?= podriem agafar també la mitja
#     -) 3 - 3 = Massa VARIABILITAT = DEIXEM EL PUNT EN BLANC
#     -) Així al mapa de QGIS no es veurà res i no afectarà 








