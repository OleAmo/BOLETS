library(sf)
library(dplyr)
library(readr)


# ----- DADES SHAPE -----
# -----------------------


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


# ---------------------------------------
# ------------- FEIN A FER
# ----------------------------------------

#     -) Crear DF del punts 
#     -) Crear columna COORDENADES

#     -) Comprovar que funciona la FUNCIO = create_DF_NO_GEOM()
#     -) Comprovar que dona les DADES

#     -) Crear un DF amb la MITJA per CADA COMARCA
#     -) Cada comarca té X PUNTS
#     -) Cacular la MITJA de les DADES DINS SEU



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

date <- "2024-05-08"
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

date <- "2024-05-08"

system.time({
  
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
  
})

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


