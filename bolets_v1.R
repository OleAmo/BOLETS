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


